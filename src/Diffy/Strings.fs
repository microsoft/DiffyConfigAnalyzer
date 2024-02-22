namespace Diffy

open System.Collections.Generic
open Diffy.Regex

/// A module for implementing various string pattern algorithms.
module Strings =

    /// A pattern as either a char or a special token.
    type Pattern = PToken of Token | PChar of char | PAny

    /// A configuration parameter.
    type [<Struct>] ConfigParameter = { Config: int; Value: string }

    /// Parameter information.
    type [<Struct>] ParameterInfo = { TotalLength: int; Parameters: ConfigParameter[] }

    /// A pattern with parameter information about the configurations.
    type [<Struct>] ParameterizedPattern = {
        TotalLength: int;
        Cost: double;
        Pattern: Pattern[];
        Configs: int[];
        Parameters: Option<ParameterInfo>[]
    }

    /// A range of indices.
    type [<Struct>] private IndexRange = { Start: int; End: int }

    /// An edit action for sequence alignment.
    type private EditAction = Match | AddBlank1 | AddBlank2 | Regex of Token * int * int

    /// A default array to reduce allocations.
    let private defaultPatternAlignArray = Array2D.create 128 128 (0.0, Match)

    /// A default array to reduce allocations.
    let private defaultSequenceAlignArray = Array2D.create 128 128 (0.0, Match)

    /// Create a range from a start and endpoint.
    let private range (s : int) (e : int) : IndexRange =
        if e < s then { Start = -1; End = -1 } else { Start = s; End = e }
    
    /// Create a parameterized pattern for a given configuration index.
    let pattern (pp : Pattern[]) (index : int) : ParameterizedPattern =
        let parameters = Array.create pp.Length Option.None 
        { TotalLength = pp.Length; Cost = 0.0; Pattern = pp; Configs = [|index|]; Parameters = parameters }

    /// Computes the optimal sequence actions and distances for
    /// two sequences of elements of some type.
    let private sequenceAlignArray<'t when 't:equality> (s1 : 't[]) (s2 : 't[]) (cost : double[,]) : (double * EditAction)[,] =
        let result =
            if s1.Length < 128 && s2.Length < 128
            then defaultSequenceAlignArray
            else Array2D.create (s1.Length + 1) (s2.Length + 1) (0.0, Match)
        for i = 0 to s1.Length do
            result[i, 0] <- (i, AddBlank2)
        for j = 0 to s2.Length do
            result[0, j] <- (j, AddBlank1)
        for i = 1 to s1.Length do 
            for j = 1 to s2.Length do
                let matchCost = cost[i - 1, j - 1]
                if matchCost < 0.0 then failwith "invalid cost function"
                let mtch = if matchCost = System.Double.MaxValue then matchCost else matchCost + fst result[i - 1, j - 1]
                let addBlank1 = fst result[i, j - 1] + 1.0
                let addBlank2 = fst result[i - 1, j] + 1.0
                let m = min mtch (min addBlank2 addBlank1)
                let action = if m = addBlank1 then AddBlank1 else if m = addBlank2 then AddBlank2 else Match
                result[i, j] <- (m, action)
        result

    /// Computes the optimal sequence alignment between two sequences and
    /// returns two sequences of the same length with "empty" tokens inserted
    /// for the parts of the sequence that differ. It accepts a cost function
    /// that says how similar two characters are between 0.0 for exact equality
    /// and 1.0 for the most unequal.
    let SequenceAlignCost<'t when 't:equality> (s1 : 't[]) (s2 : 't[]) (empty : 't) (cost : double[,]) : ('t[] * 't[]) =
        let matrix = sequenceAlignArray s1 s2 cost
        let mutable r1 = []
        let mutable r2 = []
        let mutable i = s1.Length
        let mutable j = s2.Length
        while (i > 0 || j > 0) do
            match snd matrix[i, j] with
            | Match ->
                r1 <- s1[i - 1] :: r1
                r2 <- s2[j - 1] :: r2
                i <- i - 1
                j <- j - 1
            | AddBlank1 ->
                r1 <- empty :: r1
                r2 <- s2[j - 1] :: r2
                j <- j - 1
            | AddBlank2 ->
                r1 <- s1[i - 1] :: r1
                r2 <- empty :: r2
                i <- i - 1
            | Regex _ -> failwith "unreachable"
        (List.toArray r1, List.toArray r2)

    /// Computes the optimal sequence alignment between two sequences and
    /// returns two sequences of the same length with "empty" tokens inserted
    /// for the parts of the sequence that differ.
    let SequenceAlign<'t when 't:equality> (s1 : 't[]) (s2 : 't[]) (empty : 't) : ('t[] * 't[]) =
        let costs = Array2D.init s1.Length s2.Length (fun i j -> if s1[i] = s2[j] then 0.0 else System.Double.MaxValue)
        SequenceAlignCost s1 s2 empty costs

    /// Computes the edit distance between two sequences when
    /// using insertions/deletions only to transform one sequence to another.
    let EditDistance<'t when 't:equality> (s1 : 't[]) (s2 : 't[]) : double =
        let costs = Array2D.init s1.Length s2.Length (fun i j -> if s1[i] = s2[j] then 0.0 else System.Double.MaxValue)
        fst (sequenceAlignArray s1 s2 costs).[s1.Length, s2.Length]

    /// Removes indicies for the token if the match could be extended 
    let private filterIndicesGreedyToken(res: HashSet<int>[]) : HashSet<int>[] =
        if res.Length = 0 then res else
        for i = 0 to res.Length - 2 do
            res[i].ExceptWith(res[i+1])
        res

    /// A regex for the any pattern.
    let private anyRegex = Regex.All()

    /// An automaton for the any pattern.
    let private anyAutomaton = anyRegex.ToAutomaton()

    // A custom comparer for the priority queue.
    type private RegexSetComparer() = 
        interface IEqualityComparer<List<Regex>> with
            member this.Equals(x, y) =
                if x.Count <> y.Count then false
                else Seq.forall y.Contains x
            member this.GetHashCode(x) =
                let mutable hash = 7
                for elt in x do
                    hash <- 31 * hash + elt.GetHashCode()
                hash

    // A custom comparer for the cache.
    type private MoveStatesCacheComparer() = 
        interface IEqualityComparer<Regex * Regex * List<Regex>> with
            member this.Equals(x, y) =
                let (a1, b1, c1) = x
                let (a2, b2, c2) = y
                a1 = a2 && b1 = b2 && c1.Count = c2.Count && Seq.forall c2.Contains c1
            member this.GetHashCode(x) =
                let (a, b, c) = x
                let mutable hash = 31 * a.GetHashCode() + b.GetHashCode()
                for elt in c do
                    hash <- 31 * hash + elt.GetHashCode()
                hash

    /// A cache for the move states computation.
    let private moveStatesCache = Dictionary<Regex * Regex * List<Regex>, List<Regex>>(MoveStatesCacheComparer())

    /// Cache the results for moving the states of an automaton.
    let private getOrAddStatesCache (token : Token) (pattern : Regex) (automaton : Automaton) (states : List<Regex>) : List<Regex> =
        let key = (token.Regex, pattern, states)
        let (present, existing) = moveStatesCache.TryGetValue(key)
        if present then existing
        else 
            let result = token.Automaton.MoveStates(states, automaton)
            moveStatesCache.Add(key, result)
            result

    /// Move a set of states according to a token and pattern.
    let inline private moveStates (token : Token) (p : Pattern) (states : List<Regex>) =
        match p with
        | PChar c -> token.Automaton.MoveStates(states, c)
        | PToken t -> getOrAddStatesCache token t.Regex t.Automaton states
        | PAny -> getOrAddStatesCache token anyRegex anyAutomaton states

    /// Add or create values for a key.
    let inline private addValues (m : Dictionary<List<Regex>, HashSet<int>>) (key : List<Regex>) (values : HashSet<int>) =
        let (present, existing) = m.TryGetValue(key)
        if present then existing.UnionWith(values)
        else m[key] <- values

    /// Update the state set mapping when simulating a pattern on a token.
    let inline private updateMapping (token : Token) (p : Pattern) (mapping : Dictionary<List<Regex>, HashSet<int>>) =
        let newMapping = Dictionary(RegexSetComparer())
        for entry in mapping do
            let newStates = moveStates token p entry.Key
            addValues newMapping newStates entry.Value
        newMapping

    /// Finds the starting indices of the pattern sequence that
    /// could lead to a match at each given index.
    let FindMatchingStartIndices (token : Token) (p : Pattern[]) : HashSet<int>[] =
        let result = Array.init p.Length (fun _ -> HashSet<int>())
        let mutable mapping = Dictionary(RegexSetComparer())
        for i = 0 to p.Length - 1 do
            let states = List<Regex>([token.Automaton.InitialState])
            addValues mapping states (HashSet<int>([i]))
            mapping <- updateMapping token p[i] mapping
            for entry in mapping do
                if entry.Key |> Seq.forall token.Automaton.FinalStates.Contains then
                    result[i].UnionWith(entry.Value)
        if token.Greedy then filterIndicesGreedyToken result else result

    /// An approximate less than for doubles that accounts for routing errors.
    let inline private lessThanApprox (x : double) (y : double) = x + 0.0001 < y

    /// Gets the count of characters at a given index.
    let inline private getMatchCount (x : int) (p : ParameterizedPattern) : double =
        match p.Parameters[x] with
        | None -> p.Configs.Length
        | Some pi -> pi.TotalLength

    /// Calculates the cost of a token match.
    let inline private tokenCostMatch (tokenCost : double) (m : int) (i : int) (p : ParameterizedPattern) : double =
        let mutable cost = 0.0
        for x = m to i - 1 do
            cost <- cost + tokenCost * (getMatchCount x p)
        cost

    /// Computes the optimal sequence actions and distances for
    /// two sequences of elements of some type.
    let private patternAlignArray (tokens : Token[]) (p1 : ParameterizedPattern) (p2 : ParameterizedPattern) : (double * EditAction)[,] =
        // find the potential regex match points.
        let regexMatches1 = Array.map (fun t -> FindMatchingStartIndices t p1.Pattern) tokens
        let regexMatches2 = Array.map (fun t -> FindMatchingStartIndices t p2.Pattern) tokens
        // create the result array.
        let result =
            if p1.Pattern.Length < 128 && p2.Pattern.Length < 128
            then defaultPatternAlignArray
            else Array2D.create (p1.Pattern.Length + 1) (p2.Pattern.Length + 1) (0.0, Match)
        // initialize the base costs.
        let mutable initialCost1 = 0.0
        let mutable initialCost2 = 0.0
        for i = 0 to p1.Pattern.Length do
            result[i, 0] <- (initialCost1, AddBlank2)
            if i < p1.Pattern.Length then
                initialCost1 <- initialCost1 + (getMatchCount i p1)
        for j = 0 to p2.Pattern.Length do
            result[0, j] <- (initialCost2, AddBlank1)
            if j < p2.Pattern.Length then
                initialCost2 <- initialCost2 + (getMatchCount j p2)
        // dynamic programming algorithm.
        for i = 1 to p1.Pattern.Length do 
            for j = 1 to p2.Pattern.Length do
                // case where the characters are the same.
                let mtch =
                    match p1.Pattern[i - 1], p2.Pattern[j - 1] with
                    | PChar c1, PChar c2 when c1 = c2 -> fst result[i - 1, j - 1]
                    | _, _ -> System.Double.MaxValue
                // cases where we insert any <any> token.
                let addBlank1 = fst result[i, j - 1] + (tokenCostMatch 1.0 (j - 1) j p2)
                let addBlank2 = fst result[i - 1, j] + (tokenCostMatch 1.0 (i - 1) i p1)
                let mutable minValue = addBlank1
                let mutable minCase = AddBlank1
                if lessThanApprox addBlank2 minValue then
                    minValue <- addBlank2
                    minCase <- AddBlank2
                if lessThanApprox mtch minValue then
                    minValue <- mtch
                    minCase <- Match
                // case where theres is a regex match.
                for k = 0 to tokens.Length - 1 do
                    let token = tokens[k]
                    let mutable indices1 = regexMatches1[k][i - 1]
                    let mutable indices2 = regexMatches2[k][j - 1]
                    if token.MatchesEmptyString then
                        indices1.Add(i) |> ignore
                        indices2.Add(j) |> ignore
                    for m in indices1 do
                        for n in indices2 do
                            if i <> m || j <> n then
                                let mutable cost1 = tokenCostMatch token.Cost m i p1
                                let mutable cost2 = tokenCostMatch token.Cost n j p2
                                let regexCost = fst result[m, n] + cost1 + cost2
                                if lessThanApprox regexCost minValue then
                                    minValue <- regexCost
                                    minCase <- Regex (token, m, n)
                result[i, j] <- (minValue, minCase)
        result

    /// Merge two pattern sequences efficiently.
    let inline private mergePatterns (r1 : list<Pattern * IndexRange>) (r2 : list<Pattern * IndexRange>) : Pattern[] * IndexRange[] * IndexRange[] =
        let initialize (i : int) =
            match r1[i], r2[i] with
            | (PAny, rng1), (_, rng2) -> (PAny, rng1, rng2)
            | (_, rng1), (PAny, rng2) -> (PAny, rng1, rng2)
            | (p, rng1), (_, rng2) -> (p, rng1, rng2)
        in
        let merge (x : IndexRange) (y : IndexRange) = 
            if x.Start < 0 then y
            else if y.Start < 0 then x
            else {Start = min x.Start y.Start; End = max x.End y.End}
        in
        let merged = Array.init r1.Length initialize
        let mutable i = 0
        let mutable j = 0
        let mutable next = 1
        while next < merged.Length do
            let (p1, rng1a, rng1b) = merged[j]
            let (p2, rng2a, rng2b) = merged[next]
            match p1, p2 with
            | PAny, PAny ->
                merged[j] <- (PAny, merge rng1a rng2a, merge rng1b rng2b)
                next <- next + 1
            | _, _ ->
                merged[i] <- merged[j]
                i <- i + 1
                j <- next
                next <- next + 1
        merged[i] <- merged[j]
        Array.unzip3 merged[0..i]

    /// Get the parameter information from a range of an input.
    let private getParameterInfo (p : ParameterizedPattern) (range : IndexRange) : ParameterInfo =
        let mutable totalLength = 0 
        let configParameters = Dictionary()
        for config in p.Configs do
            configParameters[config] <- ""
        if range.Start >= 0 then
            for j = range.Start to range.End do
                match p.Pattern[j], p.Parameters[j] with
                | PChar c, None ->
                    for config in p.Configs do
                        configParameters[config] <- configParameters[config] + string c
                        totalLength <- totalLength + 1
                | PAny, Some info
                | PToken _, Some info ->
                    for configParam in info.Parameters do
                        configParameters[configParam.Config] <- configParameters[configParam.Config] + configParam.Value
                        totalLength <- totalLength + configParam.Value.Length
                | _ -> failwith "unreachable"
        let parameters = List()
        for kv in configParameters do
            parameters.Add({ Config = kv.Key; Value = kv.Value })
        { TotalLength = totalLength; Parameters = parameters.ToArray() }

    /// Join parameter info objects.
    let private joinParameterInfos (p1 : ParameterizedPattern) (p2 : ParameterizedPattern) (result : Pattern[]) (matches1 : IndexRange[]) (matches2 : IndexRange[]) : Option<ParameterInfo>[] =
        let parameterInfos = Array.create result.Length Option.None
        for i = 0 to result.Length - 1 do
            match result[i] with
            | PChar _ -> ()
            | PAny | PToken _ ->
                let parameterInfo1 = getParameterInfo p1 matches1[i]
                let parameterInfo2 = getParameterInfo p2 matches2[i]
                let totalLength = parameterInfo1.TotalLength + parameterInfo2.TotalLength
                let parameters = Array.append parameterInfo1.Parameters parameterInfo2.Parameters
                parameterInfos[i] <- Option.Some { TotalLength = totalLength; Parameters = parameters }
        parameterInfos

    /// Computes the optimal sequence alignment between two sequences and
    /// returns two sequences of the same length with "empty" tokens inserted
    /// for the parts of the sequence that differ. It accepts a cost function
    /// that says how similar two characters are between 0.0 for exact equality
    /// and 1.0 for the most unequal.
    let PatternAlign (tokens : Token[]) (p1 : ParameterizedPattern) (p2 : ParameterizedPattern) : ParameterizedPattern =
        let matrix = patternAlignArray tokens p1 p2
        let mutable r1 = []
        let mutable r2 = []
        let mutable i = p1.Pattern.Length
        let mutable j = p2.Pattern.Length
        let mutable cost = fst matrix[i, j]
        let mutable resultPattern = null
        let mutable matches1 = null
        let mutable matches2 = null
        while (i > 0 || j > 0) do
            match snd matrix[i, j] with
            | Match ->
                r1 <- (p1.Pattern[i - 1], range (i - 1) (i - 1)) :: r1
                r2 <- (p2.Pattern[j - 1], range (j - 1) (j - 1)) :: r2
                i <- i - 1
                j <- j - 1
            | AddBlank1 ->
                r1 <- (PAny, range -1 -1) :: r1
                r2 <- (p2.Pattern[j - 1], range (j - 1) (j - 1)) :: r2
                j <- j - 1
            | AddBlank2 ->
                r1 <- (p1.Pattern[i - 1], range (i - 1) (i - 1)) :: r1
                r2 <- (PAny, range -1 -1) :: r2
                i <- i - 1
            | Regex (t, m, n) ->
                r1 <- (PToken t, range m (i - 1)) :: r1
                r2 <- (PToken t, range n (j - 1)) :: r2
                i <- m
                j <- n
        if r1.Length = 0 then
            cost <- 0.0
            resultPattern <- [||]
            matches1 <- [||]
            matches2 <- [||]
        else
            let (result, m1, m2) = mergePatterns r1 r2
            resultPattern <- result
            matches1 <- m1
            matches2 <- m2
        let configs = Array.append p1.Configs p2.Configs
        let parameterInfos = joinParameterInfos p1 p2 resultPattern matches1 matches2
        let totalLength = p1.TotalLength + p2.TotalLength
        { TotalLength = totalLength; Cost = cost; Pattern = resultPattern; Configs = configs; Parameters = parameterInfos }
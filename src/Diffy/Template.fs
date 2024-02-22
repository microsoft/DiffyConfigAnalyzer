namespace Diffy

open System.Collections.Generic
open System.Text.Json
open Diffy.DataStructures

/// Statistics about a templating run.
type Stats = {
    /// The number of string templating cache hits.
    mutable StringCacheHit : int

    /// The number of string templating cache misses.
    mutable StringCacheMiss : int

    /// The number of list template cache hits.
    mutable ListCacheHit : int

    /// The number of list template cache misses.
    mutable ListCacheMiss : int

    /// The number of set template cache hits.
    mutable SetCacheHit : int

    /// The number of set template cache misses.
    mutable SetCacheMiss : int

    /// The number of repeat cache hits.
    mutable RepeatCacheHit : int

    /// The number of repeat cache misses.
    mutable RepeatCacheMiss : int

    /// The number of record cache hits.
    mutable RecordCacheHit : int

    /// The number of record cache misses.
    mutable RecordCacheMiss : int

    /// The total trees.
    mutable TreesTotal : int

    /// The trees removed by uniqueTrees.
    mutable TreesRemoved : int

    /// The trees removed by uniqueTrees.
    mutable InferStringTime : int64
}

/// A templating configuration.
type Environment = {

    /// The custom tokens.
    Tokens : Token[]

    /// The number of configurations.
    NumConfigs : int

    /// The threshold between 0.0 and 1.0 for placing
    /// elements in the same group.
    GroupingThreshold : double

    /// A threshold between 0.0 and 1.0 for deciding between
    /// what type of list template to give.
    ListTypeThreshold : double

    /// Whether or not to use exact key matching.
    ExactKeyMatching : bool

    /// The maximum size of a list to template. Negative if any size is allowed.
    MaxListSize : int

    /// Whether to only include group counts and not members.
    CountOnly : bool

    /// A cache for templating operations.
    ResultCache : Dictionary<uint64 * uint64 * CacheContext, PricedTemplate>

    /// Paths to exclude from templating.
    Exclusions : System.Text.RegularExpressions.Regex[]

    /// Paths to include in templating.
    Inclusions : System.Text.RegularExpressions.Regex[]

    /// Whether or not to disable repeat templates.
    DisableRepeat : bool

    /// The current path through record keys.
    Path : string

    /// The statistics for the run.
    Stats : Stats

    /// Whether to print debugging information.
    Debug : bool
}

/// A caller context for a cache entry.
and CacheContext = CInvalid | CString | CList | CSet | CRepeat | CRecord

/// A template with a guard consisting of those configurations
/// that have the template that follows.
and [<Struct>] GuardedTemplate(ids : BitSet, template : Template) =
    
    /// A mapping from the config index to the element in the list/set.
    member this.Ids = ids
    
    /// The template being guarded.
    member this.Template = template
    
    /// Convert this guarded template to a string.
    override this.ToString() = $"GuardedTemplate({ids}, {template})"

/// A priced template with a cost associated with it.
and [<Struct>] PricedTemplate(cost : double, template : Template) =

    /// A cost associated with the template that is normalized
    /// to be between 0.0 and 1.0.
    member this.Cost = cost
    
    /// The template being priced.
    member this.Template = template

    /// Convert this guarded template to a string.
    override this.ToString() = $"PricedTemplate({cost}, {template})"

/// A template type for data given in a tree format.
and Template =
    | TNull
    | TConst of string
    | TToken of Token * Strings.ConfigParameter[]
    | TAny of Strings.ConfigParameter[]
    | TConcat of Template[]
    | TList of Template[]
    | TSet of Template[]
    | TRepeat of Template[]
    | TRecord of KeyTemplates : Template[] * ValueTemplates : Template[]
    | TCases of GuardedTemplate[]

/// Module for templates and their functionality.
module Template =

    /// The default statistics.
    let defaultStats() = {
        StringCacheHit = 0
        StringCacheMiss = 0
        ListCacheHit = 0
        ListCacheMiss = 0
        SetCacheHit = 0
        SetCacheMiss = 0
        RepeatCacheHit = 0
        RepeatCacheMiss = 0
        RecordCacheHit = 0
        RecordCacheMiss = 0
        TreesTotal = 0
        TreesRemoved = 0
        InferStringTime = 0
    }

    /// The default environment.
    let DefaultEnvironment () : Environment = {
        Tokens = [||]
        NumConfigs = -1
        GroupingThreshold = 0.5
        ListTypeThreshold = 0.05
        ExactKeyMatching = true
        MaxListSize = -1
        CountOnly = false
        ResultCache = new Dictionary<uint64 * uint64 * CacheContext, PricedTemplate>(32768)
        Exclusions = [||]
        Inclusions = [||] 
        DisableRepeat = false
        Path = "/"
        Debug = false
        Stats = defaultStats()
    }

    /// Convert a string template to as string.
    let rec StringTemplateToString (t : Template) =
        match t with
        | Template.TAny _ -> "[any]"
        | Template.TConst s -> s
        | Template.TToken (t, _) -> $"[{t.Name}]"
        | Template.TConcat ss ->
            let mutable result = ""
            for s in ss do
                match s with
                | Template.TAny _ -> result <- result + "[any]"
                | Template.TConst s -> result <- result + s
                | Template.TToken (t, _) -> result <- result + $"[{t.Name}]"
                | _ -> failwith "unreachable"
            result
        | Template.TCases elts ->
            let elts = Array.map (fun (gt : GuardedTemplate) -> StringTemplateToString gt.Template) elts
            System.String.Join("|", elts)
        | _ -> failwith "unreachable"

    /// Perform a substitution for the ids used in a template.
    let rec private substituteIds (env : Environment) (substitution : Dictionary<int, int[]>) (t : Template) : Template = 
        match t with
        | Template.TNull
        | Template.TConst _ -> t
        | Template.TConcat ts -> Template.TConcat (Array.map (substituteIds env substitution) ts)
        | Template.TAny configParams ->
            let newIds = List<Strings.ConfigParameter>()
            for configParam in configParams do
                for resultId in substitution[configParam.Config] do
                    newIds.Add({Config = resultId; Value = configParam.Value})
            let result = newIds.ToArray()
            result |> Array.sortInPlaceBy (fun par -> par.Config)
            TAny result
        | Template.TToken (t, configParams) ->
            let newIds = List<Strings.ConfigParameter>()
            for configParam in configParams do
                for resultId in substitution[configParam.Config] do
                    newIds.Add({Config = resultId; Value = configParam.Value})
            let result = newIds.ToArray()
            result |> Array.sortInPlaceBy (fun par -> par.Config)
            TToken (t, result)
        | Template.TCases elts ->
            let guardedTemplates = Array.create elts.Length (GuardedTemplate(BitSet(env.NumConfigs), Template.TNull))
            for i = 0 to elts.Length - 1 do
                let guardedTemplate = elts[i]
                let newGroupIds = List()
                for id in guardedTemplate.Ids do
                    newGroupIds.AddRange(substitution[id])
                let newTemplate = substituteIds env substitution guardedTemplate.Template
                guardedTemplates[i] <- GuardedTemplate(BitSet.OfSeq(env.NumConfigs, newGroupIds), newTemplate)
            Template.TCases guardedTemplates
        | Template.TList elts -> Template.TList (Array.map (substituteIds env substitution) elts)
        | Template.TSet elts -> Template.TSet (Array.map (substituteIds env substitution) elts)
        | Template.TRepeat elts -> Template.TRepeat (Array.map (substituteIds env substitution) elts)
        | Template.TRecord (keys, values) ->
            let keys = Array.map (substituteIds env substitution) keys
            let values = Array.map (substituteIds env substitution) values
            Template.TRecord(keys, values)

    /// Perform a substitution for the ids used in a template.
    let rec private substituteIdsPriced (env : Environment) (substitution : Dictionary<int, int[]>) (pt : PricedTemplate) : PricedTemplate =
        PricedTemplate(pt.Cost, substituteIds env substitution pt.Template)

    /// Extend an environments path with a level in the tree.
    let ExtendPath (path : string) (part : string) : string =
        if path = "/" then path + part else path + "/" + part

    /// Extend an environments path with a level in the tree.
    let extendPathEnv (env : Environment) (part : string) : Environment =
        {env with Path = ExtendPath env.Path part}

    /// Turns a string into an array where each element is a singleton
    /// string representing that character.
    let private stringToPatternChars (s : string) : Strings.Pattern[] =
        s.ToCharArray() |> Array.map Strings.PChar

    /// Turns a a string array into a string template, where nulls
    /// represent the any() templates.
    let private charsToTemplate (tt : Tree[]) (p : Strings.ParameterizedPattern) : Template =
        let convert (x : Strings.Pattern) (paramInfo : Option<Strings.ParameterInfo>) =
            match x, paramInfo with
            | (Strings.PChar c, None) -> TConst (string c)
            | (Strings.PAny, Some pi) ->
                let parameters : Strings.ConfigParameter[] =
                    pi.Parameters 
                    |> Array.map (fun par -> {Config = Tree.Index tt[par.Config]; Value = par.Value})
                parameters |> Array.sortInPlaceBy (fun par -> par.Config)
                TAny parameters
            | (Strings.PToken t, Some pi) ->
                let parameters : Strings.ConfigParameter[] =
                    pi.Parameters 
                    |> Array.map (fun par -> {Config = Tree.Index tt[par.Config]; Value = par.Value})
                parameters |> Array.sortInPlaceBy (fun par -> par.Config)
                TToken (t, parameters)
            | _ -> failwith "unreachable"
        in
        if p.Pattern.Length > 0 then
            let templates = Array.map2 convert p.Pattern p.Parameters
            let result = List(templates.Length)
            for i = 1 to templates.Length - 1 do
                let a = templates[i - 1]
                let b = templates[i]
                match a, b with
                | TConst x, TConst y -> templates[i] <- TConst (x + y)
                | _, _ -> result.Add(a)
            result.Add(templates[templates.Length - 1])
            if result.Count = 1 then result[0] else result.ToArray() |> TConcat
        else TConst ""

    /// Quickly checks if all the trees have equal hashes.
    let inline private allHashesEqual (trees : Tree[]) =
        let hash = Tree.Hash trees[0]
        let mutable allEqual = true
        for tree in trees do
            if Tree.Hash tree <> hash then
                allEqual <- false
        allEqual

    /// Counts the number of elements in nested arrays.
    let inline private minGroups<'t> (elements : 't[][]) : int =
        let maxElt = Array.maxBy (fun (elt : 't[]) -> elt.Length) elements
        maxElt.Length

    /// Counts the number of elements in nested arrays.
    let inline private maxGroups<'t> (elements : 't[][]) : int =
        Array.sumBy (fun (elt : 't[]) -> elt.Length) elements
        
    /// Convert a tree to an exact template.
    let rec private fromTree (env : Environment) (t : Tree) : Template = 
        match t with
        | Null _ -> Template.TNull
        | Leaf (_, s, _) -> Template.TConst s
        | List (_, l) ->
            let templates = Array.map (fromTree env) l
            Template.TList templates
        | Record (_, r) ->
            let keys = List()
            let values = List()
            for (k, v) in r do
                let newEnv = extendPathEnv env k
                let isIncluded = env.Inclusions.Length = 0 || Array.exists (fun (r : System.Text.RegularExpressions.Regex) ->r.IsMatch newEnv.Path) env.Inclusions
                let isExcluded = Array.exists (fun (r : System.Text.RegularExpressions.Regex) ->r.IsMatch newEnv.Path) env.Exclusions
                if isIncluded && not isExcluded then
                    keys.Add(Template.TConst k)
                    values.Add(fromTree newEnv v)
            Template.TRecord(keys.ToArray(), values.ToArray())

    /// Computes the cost of a sequence of templates with cases.
    let private costCases (numConfigs : int) (minGroups : int) (maxGroups : int) (costInfos : list<{|Cost: double; GroupSize: int|}>) =
        if numConfigs = 1 then 0.0 else
        let mutable benefit = 0.0
        for costInfo in costInfos do
            benefit <- benefit + (1.0 - costInfo.Cost) * (double costInfo.GroupSize / double numConfigs)
        let avgBenefit = benefit / double costInfos.Length
        let sizeNumerator = double (costInfos.Length - minGroups)
        let sizeDenominator = double (maxGroups - minGroups)
        let benefit = avgBenefit * (1.0 - (sizeNumerator / sizeDenominator))
        let cost = 1.0 - benefit
        min 1.0 (max 0.0 cost)

    /// Hashes an array of trees.
    let inline private hashTreesIndex (trees : Tree[]) (endIndex : int) : uint64 * uint64 =
        let mutable h1 = 75189925309UL
        let mutable h2 = 75189925309UL
        for i = 0 to endIndex do
            let tree = trees[i]
            h1 <- Common.PerfectHash h1 (Tree.Hash tree)
            h2 <- Common.PerfectHash h2 (uint64 (Tree.Index tree))
        (h1, h2)

    /// Hashes an array of trees.
    let inline private hashTrees (trees : Tree[]) : uint64 * uint64 =
        hashTreesIndex trees (trees.Length - 1)

    /// Hashes an array of tree lists.
    let inline private hashTreeLists (trees : Tree[][]) : uint64 * uint64 =
        let mutable h1 = 189438274895UL
        let mutable h2 = 189438274895UL
        for tree in trees do
            let (h1t, h2t) = hashTrees tree
            h1 <- Common.PerfectHash h1 h1t
            h2 <- Common.PerfectHash h2 h2t
        (h1, h2)

    /// Hashes keys and values in a record.
    let inline private hashTreeRecords (keys : Tree[][], values : Tree[][]) : uint64 * uint64 =
        let (h1a, h2) = hashTreeLists keys
        let (h1b, _) = hashTreeLists values
        let h1 = Common.PerfectHash h1a h1b
        (h1, h2)

    /// Cache a function that takes a collection of trees as input.
    let inline private cache<'t> (env : Environment) (input : 't) (context : CacheContext) (hash : 't -> uint64 * uint64) (f : Environment -> 't -> PricedTemplate) : PricedTemplate =
        let (h1, h2) = hash input
        let key = (h1, h2, context)
        let (present, result) = env.ResultCache.TryGetValue key
        let returnValue, hit =
            if present then (result, true)
            else
                let result = f env input
                env.ResultCache[key] <- result
                (result, false)
        if hit then
            match context with
            | CacheContext.CString -> env.Stats.StringCacheHit <- env.Stats.StringCacheHit + 1
            | CacheContext.CList -> env.Stats.ListCacheHit <- env.Stats.ListCacheHit + 1
            | CacheContext.CSet -> env.Stats.SetCacheHit <- env.Stats.SetCacheHit + 1
            | CacheContext.CRepeat -> env.Stats.RepeatCacheHit <- env.Stats.RepeatCacheHit + 1
            | CacheContext.CRecord -> env.Stats.RecordCacheHit <- env.Stats.RecordCacheHit + 1
            | CacheContext.CInvalid -> ()
        else
            match context with
            | CacheContext.CString -> env.Stats.StringCacheMiss <- env.Stats.StringCacheMiss + 1
            | CacheContext.CList -> env.Stats.ListCacheMiss <- env.Stats.ListCacheMiss + 1
            | CacheContext.CSet -> env.Stats.SetCacheMiss <- env.Stats.SetCacheMiss + 1
            | CacheContext.CRepeat -> env.Stats.RepeatCacheMiss <- env.Stats.RepeatCacheMiss + 1
            | CacheContext.CRecord -> env.Stats.RecordCacheMiss <- env.Stats.RecordCacheMiss + 1
            | CacheContext.CInvalid -> ()
        returnValue

    /// Partition a collection of trees into equality groups by their hash.
    let private getUniqueTrees (env : Environment) (trees : Tree[]) : {| Representatives : Tree[]; Substitution : Dictionary<int, int[]> |} =
        let hashGroups = Array.groupBy Tree.Hash trees
        env.Stats.TreesTotal <- env.Stats.TreesTotal + trees.Length
        env.Stats.TreesRemoved <- env.Stats.TreesRemoved + (trees.Length - hashGroups.Length)
        if hashGroups.Length = trees.Length then
            {| Representatives = trees; Substitution = null |}
        else
            let representatives = List()
            let substitution = Dictionary<int, int[]>(hashGroups.Length)
            for (_, group) in hashGroups do
                representatives.Add(group[0])
                substitution[Tree.Index group[0]] <- Array.map Tree.Index group
            {| Representatives = representatives.ToArray(); Substitution = substitution |}

    /// Infer a template from a collection of configurations represented as
    /// data trees. Finds the minimum cost template using default parameters.
    let rec Infer (trees : Tree[]) : Template = InferWithOptions(trees, DefaultEnvironment())

    /// This is a function to create a template from a collection of
    /// configurations represented as data trees.
    /// The template returned should be the best fit, that minimizes
    /// the difference between
    and InferWithOptions (trees : Tree[], env : Environment) : Template =
        let env = {env with NumConfigs = trees.Length}
        let trees = Array.mapi (fun i t -> Tree.WithIndex i t) trees
        let pt = InferHelper env trees
        pt.Template

    /// This is a function to create a template from a collection of
    /// configurations represented as data trees.
    /// The template returned should be the best fit, that minimizes
    /// the difference between
    and private InferHelper (env : Environment) (trees : Tree[]) : PricedTemplate =
        if (trees.Length = 0) then failwith "can not infer a template for an empty array"
        if env.Debug then printfn "%A" env.Path
        if trees.Length = 1 then PricedTemplate(0.0, fromTree env trees[0]) else

        let mutable numTemplates = 0
        let mutable hasNull = false;
        let mutable hasString = false;
        let mutable hasList = false;
        let mutable hasRecord = false;
        let mutable nullTemplate = None
        let mutable stringTemplate = None
        let mutable listTemplate = None
        let mutable recordTemplate = None
        let mutable nullIds = BitSet(env.NumConfigs)
        let mutable stringIds = BitSet(env.NumConfigs)
        let mutable listIds = BitSet(env.NumConfigs)
        let mutable recordIds = BitSet(env.NumConfigs)
        for tree in trees do
            match tree with
            | Tree.Null _ -> hasNull <- true
            | Tree.Leaf _ -> hasString <- true
            | Tree.List _ -> hasList <- true
            | Tree.Record _ -> hasRecord <- true
        if hasNull then
            let trees = Array.where Tree.IsNull trees
            nullTemplate <- Some (PricedTemplate(0.0, Template.TNull))
            nullIds <- BitSet.OfSeq(env.NumConfigs, Array.map Tree.Index trees)
            numTemplates <- numTemplates + 1
        if hasString then
            let trees = Array.where Tree.IsLeaf trees
            let unique = getUniqueTrees env trees
            let stemplate = InferString env unique.Representatives
            stringTemplate <- Some (if unique.Substitution = null then stemplate else substituteIdsPriced env unique.Substitution stemplate)
            stringIds <- BitSet.OfSeq(env.NumConfigs, Array.map Tree.Index trees)
            numTemplates <- numTemplates + 1
        if hasList then
            let trees = Array.where Tree.IsList trees
            let unique = getUniqueTrees env trees
            let lists = Array.map Tree.GetList unique.Representatives
            let lt = InferList env lists
            listTemplate <- Some (if unique.Substitution = null then lt else substituteIdsPriced env unique.Substitution lt)
            if lt.Cost > env.ListTypeThreshold then
                let st = InferSet env lists
                if st.Cost + env.ListTypeThreshold < lt.Cost then
                    listTemplate <- Some (if unique.Substitution = null then st else substituteIdsPriced env unique.Substitution st)
                if not env.DisableRepeat && lt.Cost > 2.0 * env.ListTypeThreshold && st.Cost > env.ListTypeThreshold then
                    let rt = InferRepeat env lists
                    if rt.Cost + env.ListTypeThreshold < st.Cost && rt.Cost + 2.0 * env.ListTypeThreshold < lt.Cost then
                        listTemplate <- Some (if unique.Substitution = null then rt else substituteIdsPriced env unique.Substitution rt)
            listIds <- BitSet.OfSeq(env.NumConfigs, Array.map Tree.Index trees)
            numTemplates <- numTemplates + 1
        if hasRecord then
            let trees = Array.where Tree.IsRecord trees
            let unique = getUniqueTrees env trees
            let records = Array.map Tree.GetRecord unique.Representatives
            let keys = Array.map (fun m -> m |> Array.map (fun (k, v : Tree) -> Tree.LeafIndex(Tree.Index v, k, typeof<string>))) records
            let values = Array.map (fun m -> m |> Array.map (fun (_, v) -> v)) records
            let rt = InferRecord env keys values
            recordTemplate <- Some (if unique.Substitution = null then rt else substituteIdsPriced env unique.Substitution rt)
            recordIds <- BitSet.OfSeq(env.NumConfigs, Array.map Tree.Index trees)
            numTemplates <- numTemplates + 1
        if numTemplates = 1 then
            match nullTemplate, stringTemplate, listTemplate, recordTemplate with
            | Some t, _, _, _ -> t
            | _, Some t, _, _ -> t
            | _, _, Some t, _ -> t
            | _, _, _, Some t -> t
            | _ -> failwith "unreachable"
        else
            let guardedTemplates = List()
            let mutable costInfos = []
            if hasNull then
                let template = Option.get nullTemplate
                guardedTemplates.Add(GuardedTemplate(nullIds, template.Template))
                costInfos <- {| Cost = template.Cost; GroupSize = nullIds.Count() |} :: costInfos
            if hasString then
                let template = Option.get stringTemplate
                // to avoid nested TCases expressions, we can pull out the cases to the top level here.
                match template.Template with
                | Template.TCases cases -> guardedTemplates.AddRange(cases)
                | _ -> guardedTemplates.Add(GuardedTemplate(stringIds, template.Template))
                costInfos <- {| Cost = template.Cost; GroupSize = stringIds.Count() |} :: costInfos
            if hasList then
                let template = Option.get listTemplate
                guardedTemplates.Add(GuardedTemplate(listIds, template.Template))
                costInfos <- {| Cost = template.Cost; GroupSize = listIds.Count() |} :: costInfos
            if hasRecord then
                let template = Option.get recordTemplate
                guardedTemplates.Add(GuardedTemplate(recordIds, template.Template))
                costInfos <- {| Cost = template.Cost; GroupSize = recordIds.Count() |} :: costInfos
            let template = Template.TCases (guardedTemplates.ToArray())
            PricedTemplate (costCases trees.Length 1 4 costInfos, template)
    
    /// This function is used to create a best template for a collection
    /// of string values. It computes the template with lowest cost.
    and private InferString (env : Environment) (tt : Tree[]) : PricedTemplate =
        cache env tt CacheContext.CString hashTrees InferStringNoCache

    /// This function is used to create a best template for a collection
    /// of string values. It computes the template with lowest cost.
    and private InferStringNoCache (env : Environment) (tt : Tree[]) : PricedTemplate =
        let watch = System.Diagnostics.Stopwatch.StartNew()
        let clusters = InferStringPatterns env tt
        let templates = Array.create clusters.Length (PricedTemplate(0.0, Template.TNull))
        for i = 0 to clusters.Length - 1 do
            let cluster = clusters[i].Item
            let template = charsToTemplate tt cluster
            let cost = cluster.Cost / (double cluster.TotalLength)
            templates[i] <- PricedTemplate(cost, template)
        if templates.Length = 1 then
            env.Stats.InferStringTime <- env.Stats.InferStringTime + watch.ElapsedMilliseconds
            templates[0]
        else
            let guardedTemplates = List()
            let mutable costInfos = []
            for i = 0 to clusters.Length - 1 do
                let pt = templates[i]
                let ids = BitSet.OfSeq(env.NumConfigs, Array.map (fun idx -> Tree.Index tt[idx]) clusters[i].Item.Configs)
                guardedTemplates.Add(GuardedTemplate(ids, pt.Template))
                costInfos <- {|Cost = pt.Cost; GroupSize = clusters[i].Item.Configs.Length|} :: costInfos
            env.Stats.InferStringTime <- env.Stats.InferStringTime + watch.ElapsedMilliseconds
            PricedTemplate(costCases tt.Length 1 tt.Length costInfos, Template.TCases (guardedTemplates.ToArray()))

    /// This function is used to create a best template for a collection
    /// of string values. It computes the template with lowest cost.
    and private InferStringPatterns (env : Environment) (tt : Tree[]) : Common.PricedItem<Strings.ParameterizedPattern>[] =
        let inline combine x y : Common.PricedItem<Strings.ParameterizedPattern> =
            let result = Strings.PatternAlign env.Tokens x y
            { Item = result; Cost = result.Cost / (double)result.TotalLength }
        in
        let ss = Array.map Tree.GetLeaf tt
        let chars = Array.mapi (fun i s -> Strings.pattern (stringToPatternChars s) i) ss
        Clustering.ClusterGreedyWithMerge env.GroupingThreshold combine chars

    /// This function is used to create a best template for an unordered set template.
    and private InferSet (env : Environment) (tt : Tree[][]) : PricedTemplate =
        cache env tt CacheContext.CSet hashTreeLists InferSetNoCache

    /// This function is used to create a best template for an unordered set template.
    and private InferSetNoCache (env : Environment) (tt : Tree[][]) : PricedTemplate =
        let inline costFunction xs =
            let cost = (InferHelper env xs).Cost
            if cost > env.GroupingThreshold then -1.0 else cost
        let inline lookupFunction _ = None
        let templateGroups = Matching.MatchSets costFunction lookupFunction tt
        let result = InferCollectionHelper env tt templateGroups (minGroups tt) (maxGroups tt)
        PricedTemplate(result.Cost, TSet result.Item)

    /// This function creates the parts of a template given
    /// an existing grouping of elements together from a list.
    and private InferCollectionHelper (env : Environment) (input : Tree[][]) (groups : Tree[][]) (minGroups : int)(maxGroups : int) : Common.PricedItem<Template[]> =
        let mutable guardedTemplates = List(groups.Length)
        let mutable costInfo = []
        for templateGroup in groups do
            let pt = InferHelper env templateGroup
            let mutable indices = BitSet(env.NumConfigs)
            for tree in templateGroup do
                indices.Add(Tree.Index tree)
            let needCases = indices.Count() < input.Length
            let gt = (needCases, GuardedTemplate(indices, pt.Template))
            guardedTemplates.Add(gt)
            costInfo <- {|Cost = pt.Cost; GroupSize = indices.Count()|} :: costInfo
        let guardedTemplates = guardedTemplates.ToArray()
        let templates = Array.map (fun (needCases, gt : GuardedTemplate) -> if needCases then Template.TCases [|gt|] else gt.Template) guardedTemplates
        let cost = costCases input.Length minGroups maxGroups costInfo
        { Cost = cost; Item = templates }

    /// This function is used to create a best template for an ordered list template.
    and private InferList (env : Environment) (tt : Tree[][]) : PricedTemplate =
        cache env tt CacheContext.CList hashTreeLists InferListNoCache

    /// This function is used to create a best template for an ordered list template.
    and private InferListNoCache (env : Environment) (tt : Tree[][]) : PricedTemplate =
        let mutable maxSize = 0
        for tree in tt do
            maxSize <- max maxSize tree.Length
        if env.MaxListSize >= 0 && env.MaxListSize < maxSize then
            PricedTemplate(0.0, TNull)
        else
            let mutable templateGroups = Array.map (fun i -> [|i|]) tt[0]
            for i = 1 to tt.Length - 1 do
                templateGroups <- InferListPair env templateGroups tt[i]
            let result = InferCollectionHelper env tt templateGroups (minGroups tt) (maxGroups tt)
            PricedTemplate(result.Cost, TList result.Item)

    /// Combines two ordered lists and returns the best matching result.
    and private InferListPair (env : Environment) (t1 : Tree[][]) (t2 : Tree[]) : Tree[][] =
        let inline getCost (x : Tree[]) (y : Tree[]) : double =
            let pt = InferHelper env (Array.append x y)
            if pt.Cost >= env.GroupingThreshold then System.Double.MaxValue else pt.Cost
        in
        let t2 = Array.map Array.singleton t2
        let costs = Array2D.init t1.Length t2.Length (fun i j -> getCost t1[i] t2[j])
        let (aligned1, aligned2) = Strings.SequenceAlignCost t1 t2 null costs
        Array.zip aligned1 aligned2
        |> Array.map (fun (x, y) -> if x = null then y else if y = null then x else Array.append x y)

    /// This function is used to create a best template for an ordered list template.
    and private InferRepeat (env : Environment) (tt : Tree[][]) : PricedTemplate =
        cache env tt CacheContext.CRepeat hashTreeLists InferRepeatNoCache

    /// This function is used to create a best template for an ordered list template.
    and private InferRepeatNoCache (env : Environment) (tt : Tree[][]) : PricedTemplate =
        let allTrees = Array.concat tt
        let groups = Clustering.ClusterGreedy env.GroupingThreshold (fun xs -> (InferHelper env xs).Cost) allTrees
        let result = InferCollectionHelper env tt groups 1 (maxGroups tt)
        PricedTemplate(result.Cost, TRepeat result.Item)

    /// This function is used to create a best template for a list of records.
    /// We first try to align the keys (without looking at the values), and only
    /// then do we try to align the values given the key alignments.
    and private InferRecord (env : Environment) (keys : Tree[][]) (values : Tree[][]) : PricedTemplate =
        cache env (keys, values) CacheContext.CRecord hashTreeRecords InferRecordNoCache

    /// This function is used to create a best template for a list of records.
    /// We first try to align the keys (without looking at the values), and only
    /// then do we try to align the values given the key alignments.
    and private InferRecordNoCache (env : Environment) (keys : Tree[][], values : Tree[][]) : PricedTemplate =
        // create a mapping from key trees to input indices
        let indexLookup = Dictionary()
        for i = 0 to keys.Length - 1 do
            for j = 0 to keys[i].Length - 1 do
                indexLookup[keys[i][j]] <- (i, j)
        // get all the key ids.
        let mutable keyIds = HashSet()
        for trees in keys do
            for tree in trees do
                keyIds.Add(Tree.Index tree) |> ignore
        // For now we use exact matches for key templates.
        // We could also later treat a record as a set of key value pairs
        // and attempt to build a set template.
        let inferredKeys =
            if env.ExactKeyMatching
            then InferRecordKeysExact keys values
            else InferRecordKeysRelaxed env keys values
        let keyTemplates = List(inferredKeys.Length)
        let valueTemplates = List(inferredKeys.Length)
        let mutable costInfo = []
        for (key, trees) in inferredKeys do
            // get the ids and group the values.
            let ids = BitSet(env.NumConfigs)
            let mutable valueTrees = List()
            for tree in trees do
                ids.Add(Tree.Index  tree)
                let (i, j) = indexLookup[tree]
                valueTrees.Add(values[i][j])
            // compute the value template recursively.
            let newEnv = extendPathEnv env (StringTemplateToString key)
            let isIncluded = env.Inclusions.Length = 0 || env.Inclusions |> Array.exists (fun r -> r.IsMatch newEnv.Path)
            let isExcluded = env.Exclusions |> Array.exists (fun r -> r.IsMatch newEnv.Path)
            let valuePricedTemplate =
                if isIncluded && not isExcluded
                then InferHelper newEnv (valueTrees.ToArray())
                else PricedTemplate(0.0, TNull)
            let valueTemplate = valuePricedTemplate.Template
            let valueTemplate =
                if ids.Count() <> keyIds.Count
                then Template.TCases [|GuardedTemplate(ids, valueTemplate)|]
                else valueTemplate
            // add the key and value templates.
            if not isExcluded then
                keyTemplates.Add(key)
                valueTemplates.Add(valueTemplate)
                costInfo <- {|Cost = valuePricedTemplate.Cost; GroupSize = trees.Count|} :: costInfo
        let cost = costCases keys.Length (minGroups keys) (maxGroups keys) costInfo
        PricedTemplate(cost, TRecord(keyTemplates.ToArray(), valueTemplates.ToArray()))

    /// Infer a template for the record keys based on exact matching
    /// the keys. This is a more efficient function compared to using
    /// just Template.InferSet, which relies on bipartite matching.
    and private InferRecordKeysExact (keys : Tree[][]) (values : Tree[][]) : (Template * List<Tree>)[] =
        let keySets = Dictionary<string, List<Tree>>()
        let result = List<Template * List<Tree>>()
        for i = 0 to keys.Length - 1 do
            for j = 0 to keys[i].Length - 1 do
                let tree = keys[i][j]
                let key = Tree.GetLeaf tree
                let mutable (present, trees) = keySets.TryGetValue key
                if not present then
                    trees <- List()
                    keySets[key] <- trees
                    result.Add((Template.TConst key, trees))
                trees.Add(tree)
        result.ToArray()

    /// Infer a template for the record keys based on relaxed matching.
    and private InferRecordKeysRelaxed (env : Environment) (keys : Tree[][]) (values : Tree[][]) : (Template * List<Tree>)[] =
        let inline costFunction (kvs : (Tree * Tree)[]) =
            let (keys, values) = Array.unzip kvs
            let keyCost = (InferHelper env keys).Cost
            let mutable valueCost = 0.0
            for i = 0 to values.Length - 1 do
                for j = 0 to values.Length - 1 do
                    if i <> j then
                        let cost = Tree.ApproximateSimilarity values[i] values[j]
                        valueCost <- max valueCost cost
            let finalCost = if keyCost = 0.0 then 0.0 else 0.5 * keyCost + 0.5 * valueCost
            if finalCost > env.GroupingThreshold then -1.0 else finalCost
        in
        let combined = Array.mapi (fun i keyList -> Array.zip keyList values[i]) keys
        let inline lookupFunction (k, _) = Some (Tree.GetLeaf k)
        let groups = Matching.MatchSets costFunction lookupFunction combined
        let inline makeTemplate (trees : Tree[]) = ((InferHelper env trees).Template, List(trees))
        Array.map (fun group -> group |> Array.map fst |> makeTemplate) groups

    /// A JSON configuration for converting a template.
    type JsonConfig(names : string[], ?parameters : bool) =
        let useParameters = Option.defaultValue false parameters
        member this.Names = names    
        member this.Parameters = useParameters

    /// Convert a template to a json value.
    let rec ToJsonTree (env : Environment) (t : Template) (config : JsonConfig) : Nodes.JsonNode =
        match t with 
        | Template.TConst _ ->
            Nodes.JsonValue.Create(StringTemplateToString t)
        | Template.TAny pi
        | Template.TToken (_, pi) ->
            createStringJson env config (StringTemplateToString t) [| pi |]
        | Template.TConcat elts ->
            let parameterInfos = elts |> Array.choose (fun x -> match x with TAny pi | TToken (_, pi) -> Some pi | _ -> None)
            createStringJson env config (StringTemplateToString t) parameterInfos
        | Template.TNull -> Nodes.JsonValue.Create(null)
        | Template.TSet elts
        | Template.TList elts
        | Template.TRepeat elts ->
            let annotationStr =
                match t with
                | Template.TSet _ -> "@type:unordered"
                | Template.TList _ -> "@type:ordered"
                | Template.TRepeat _ -> "@type:repeat-one-of"
                | _ -> failwith "unreachable"
            let annotation = Nodes.JsonValue.Create(annotationStr)
            let elements = elts |> Array.map (fun t -> ToJsonTree env t config)
            let elements = if elements.Length = 0 then elements else Array.insertAt 0 annotation elements
            Nodes.JsonArray(elements)
        | Template.TRecord(keys, values) ->
            // add autogenerated labels to distinguish equal keys.
            let seen = HashSet()
            let duplicates = HashSet()
            for key in keys do
                if seen.Contains(key) then
                    duplicates.Add(key) |> ignore
                seen.Add(key) |> ignore
            // create the keys and values.
            let labels = Dictionary()
            for duplicate in duplicates do
                labels[duplicate] <- -1
            let inline createKey (k : Template) : Nodes.JsonNode =
                let json = ToJsonTree env k config
                if duplicates.Contains(k) then
                    labels[k] <- labels[k] + 1
                    Nodes.JsonValue.Create(json.ToString() + "_@" + string labels[k])
                else ToJsonTree env k config
            in
            let ks = Array.map createKey keys
            let vs = Array.map (fun t -> ToJsonTree env t config) values
            let pairs = Array.zip ks vs |> Array.map (fun (k, v) -> KeyValuePair(k.ToString(), v))
            Nodes.JsonObject(pairs)
        | Template.TCases cases ->
            let nodes = Array.map (convert env config) cases
            if cases.Length = 1 then nodes[0] else Nodes.JsonArray(nodes)

    and convert (env : Environment) (config : JsonConfig) (gt : GuardedTemplate) : Nodes.JsonNode =
        let idList = List<Nodes.JsonNode>()
        for id in gt.Ids do
            idList.Add(Nodes.JsonValue.Create(config.Names[id]))
        let ids : Nodes.JsonNode[] = idList.ToArray()
        let idNode : Nodes.JsonNode = 
            if env.CountOnly then Nodes.JsonValue.Create ids.Length else Nodes.JsonArray(ids)
        let keyValuePairs = [
            KeyValuePair("CASES",  idNode)
            KeyValuePair("TEMPLATE", ToJsonTree env gt.Template config)
        ]
        Nodes.JsonObject(keyValuePairs)

    /// Creates parameter informations for JSON.
    and createStringJson (env : Environment) (config : JsonConfig) (pattern : string) (parameters : Strings.ConfigParameter[][]) : Nodes.JsonNode =
        let pattern : Nodes.JsonNode = Nodes.JsonValue.Create(pattern)
        if config.Parameters then
            let parameters : Nodes.JsonNode = Nodes.JsonArray(parameters |> Array.map (createParameterJson env config))
            let keyValuePairs = [
                KeyValuePair("Pattern",  pattern)
                KeyValuePair("Parameters", parameters)
            ]
            Nodes.JsonObject(keyValuePairs)
        else
            pattern

    /// Creates parameter informations for JSON.
    and createParameterJson (env : Environment) (config : JsonConfig) (pi : Strings.ConfigParameter[]) : Nodes.JsonNode =
        let paramToConfigs = Dictionary<string, List<int>>()
        for configParameter in pi do
            let (present, existing) = paramToConfigs.TryGetValue(configParameter.Value)
            if present then
                existing.Add(configParameter.Config)
            else
                let list = List()
                list.Add(configParameter.Config)
                paramToConfigs.Add(configParameter.Value, list)
        let result = SortedDictionary<string, Nodes.JsonNode>()
        for kv in paramToConfigs do
            let value : Nodes.JsonNode = 
                if env.CountOnly then Nodes.JsonValue.Create(kv.Value.Count) 
                else
                    let values : Nodes.JsonNode[] = kv.Value.ToArray() |> Array.map (fun i -> Nodes.JsonValue.Create(config.Names[i]))
                    Nodes.JsonArray(values)
            result.Add(kv.Key, value)
        Nodes.JsonObject(result)

    /// Converts a template to a json-readable format.
    /// Accepts the environment parameter, which may contain any settings.
    let ToJson (t : Template) (config : JsonConfig) : string =
        Common.WriteJson (ToJsonTree {DefaultEnvironment() with CountOnly = false} t config)

    /// Converts a template to a json-readable format.
    /// Accepts the environment parameter, which may contain any settings.
    let ToJsonCountOnly (t : Template) (config : JsonConfig) : string =
        Common.WriteJson (ToJsonTree {DefaultEnvironment() with CountOnly = true} t config)
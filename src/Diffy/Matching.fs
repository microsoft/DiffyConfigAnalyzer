namespace Diffy

open System.Collections.Generic


/// A module for implementing matching algorithms.
module Matching =

    /// An edge in the graph associated with a node.
    type Edge = { Src: int; Tgt: int; mutable Cap : int; mutable Cost : int; mutable Flow : int }

    /// A graph is just an adjacency array per node.
    type Graph = Edge[][]

    /// Make a deep copy of a graph.
    let private copy (g : Graph) : Graph = Array.map (fun es -> Array.map id es) g

    /// Adds a new edge to a graph.
    let private addEdge (g : Graph) (e : Edge) : Graph =
        Array.updateAt e.Src (Array.append g[e.Src] [|e|]) g

    /// Gets an edge from a source and target.
    let private getEdge (g : Graph) (s : int) (t : int) : Edge =
        g[s] |> Array.find (fun e -> e.Tgt = t)

    /// Computes the shortest path from a source to all targets.
    /// The path is stored in the parent array and it returns
    /// the distances from each node to the source.
    let private bellmanFordHelper (g : Graph) (src : int) (parents: int[]) : int[] =
        let infinity = System.Int32.MaxValue
        let distance = Array.create g.Length infinity
        distance[src] <- 0
        parents[src] <- -1
        for _ = 0 to g.Length - 2 do
            for u = 0 to g.Length - 1 do
                if distance[u] <> infinity then
                    for e in g[u] do
                        if e.Cap > 0 then
                            let v = e.Tgt
                            if distance[u] + e.Cost < distance[v] then
                                distance[v] <- distance[u] + e.Cost
                                parents[v] <- u
        distance

    /// Computes the shortest path from a source to target.
    /// The result is stored in the parent array and it returns
    /// a boolean indicating whether or not a path exists.
    let private bellmanFordAux (g : Graph) (src : int) (tgt : int) (parents: int[]) : bool =
        let distances = bellmanFordHelper g src parents
        distances[tgt] <> System.Int32.MaxValue

    /// Computes the shortest path from a source to target.
    /// Returns the distances to the source and the parent array for paths.
    let BellmanFord (g : Graph) (src : int) : int[] * int[] =
        let parents = Array.create g.Length -1
        for s = 0 to g.Length - 1 do 
            for e in g[s] do
                e.Cap <- 1
        let distances = bellmanFordHelper g src parents
        (distances, parents)

    /// Computes the max flow between a source and a target with minimal cost.
    let MinCostMaxFlow (g : Graph) (s : int) (t : int) : unit =
        let mutable residualGraph = copy g
        for u = 0 to g.Length - 1 do
            for v = 0 to g[u].Length - 1 do
                let e = { g[u][v] with Flow = 0 }
                let e' = { Src = e.Tgt; Tgt = e.Src; Cap = 0; Cost = -e.Cost; Flow = 0 }
                residualGraph <- addEdge residualGraph e'

        let mutable parent = Array.create g.Length -1
        while bellmanFordAux residualGraph s t parent do
            let mutable f = System.Int32.MaxValue
            let mutable v = t
            while v <> s do
                let e = getEdge residualGraph parent[v] v
                f <- min f e.Cap
                v <- parent[v]
            let mutable v = t
            while v <> s do
                let e1 = getEdge residualGraph parent[v] v
                let e2 = getEdge residualGraph v parent[v]
                e1.Cap <- e1.Cap - f
                e2.Cap <- e2.Cap + f
                e1.Flow <- e1.Flow + f
                e2.Flow <- e2.Flow - f
                v <- parent[v]

    /// Finds a minimum weight matching given the weights between
    /// two sets of elements given in a 2D array. To exclude elements
    /// from matching, the weight should be set to a negative number.
    let MinWeightMatchingInt (weights : int[,]) : int[] =
        if weights.Length = 0 then Array.create 0 0 else
        let height = weights.GetLength(0)
        let width = weights.GetLength(1)
        let nodes = height + width + 2
        let src = nodes - 2
        let tgt = nodes - 1
        let g = Array.create nodes null
        for i = 0 to height - 1 do
            g[i] <- Array.init width (fun j -> { Src = i; Tgt = height + j; Cap = 0; Cost = 0; Flow = 0 })
            g[src] <- Array.init height (fun i -> { Src = src; Tgt = i; Cap = 0; Cost = 0; Flow = 0 })
        for j = 0 to width - 1 do
            g[height + j] <- [|{Src = height + j; Tgt = tgt; Cap = 0; Cost = 0; Flow = 0}|]
        g[tgt] <- [||]

        // set the costs and capacities in the expanded graph
        for i = 0 to height - 1 do
            let e = g[src][i]
            e.Cap <- 1
            e.Cost <- 0
        for j = 0 to width - 1 do
            let e = getEdge g (j + height) tgt
            e.Cap <- 1
            e.Cost <- 0
        for i = 0 to height - 1 do
            for j = 0 to width - 1 do
                let e = getEdge g i (j + height)
                let w = weights[i, j]
                e.Cap <- if w < 0 then 0 else 1
                e.Cost <- w

        MinCostMaxFlow g src tgt
        let result = Array.create height -1
        for src in g do
            for e in src do
                if e.Src < height && e.Flow = 1 then
                    result[e.Src] <- e.Tgt - height
        result

    /// Finds a minimum weight matching given the weights between
    /// two sets of elements given in a 2D array. Assumes all weights
    /// are between -1.0 and 1.0. To exclude elements from matching,
    /// the weight should be set to a negative number.
    let MinWeightMatching (weights : double[,]) : int[] =
        let maxValue = 100000.0
        let intWeights = Array2D.map (fun x -> (int)(x * maxValue)) weights
        MinWeightMatchingInt intWeights

    /// Uses a greedy approach to try to find a minimum weight matching.
    let MinWeightMatchingGreedy (weights : double[,]) : int[] =
        let height = weights.GetLength(0)
        let width = weights.GetLength(1)
        let values = SortedSet<double * int * int>()
        for i = 0 to height - 1 do
            for j = 0 to width - 1 do
                values.Add((weights[i, j], i, j)) |> ignore
        let result = Array.create height -1
        let unmatched = Array.create width true 
        for (weight, i, j) in values do
            if weight >= 0.0 && result[i] = -1 && unmatched[j] then
                result[i] <- j
                unmatched[j] <- false
        result

    /// Groups a collection of sets together based on a cost function.
    let MatchSets<'t, 'k when 'k:equality> (cost : 't[] -> double) (lookup : 't -> 'k option) (lists : 't[][]) : 't[][] = 
        let groups = List()
        let exactMatch = Dictionary()
        let inline addGroup (elt : 't) =
            match lookup elt with
            | None -> ()
            | Some key -> exactMatch[key] <- groups.Count
            groups.Add([|elt|])
        in
        // create the initial groups.
        for elt in lists[0] do
            addGroup elt
        // repeatedly align two lists together.
        for i = 1 to lists.Length - 1 do
            let list = lists[i]
            // find the best matching.
            let matching = Array.create list.Length -1
            let matchingReverse = Array.create groups.Count -1
            // find the exact matches first and match them.
            for j = 0 to list.Length - 1 do
                match lookup list[j] with
                | None -> ()
                | Some key ->
                    let (present, index) = exactMatch.TryGetValue(key)
                    if present then
                        matching[j] <- index
                        matchingReverse[index] <- j
            // now lets add the costs for all non exact matches
            let values = SortedSet<double * int * int>()
            for j = 0 to list.Length - 1 do
                for k = 0 to groups.Count - 1 do
                    if matching[j] = -1 && matchingReverse[k] = -1 then
                        let cost = cost (Array.append groups[k] [|list[j]|])
                        values.Add((cost, j, k)) |> ignore
            // pick the costs from lowest to highest.
            for (weight, j, k) in values do
                if weight >= 0.0 && matching[j] = -1 && matchingReverse[k] = -1 then
                    matching[j] <- k
                    matchingReverse[k] <- j
            // add the elements to the groups to update them.
            let mutable seen = HashSet()
            for j = 0 to matching.Length - 1 do
                let m = matching[j]
                if m >= 0 then
                    groups[m] <- Array.append groups[m] [|list[j]|]
                    seen.Add(j) |> ignore
            for j = 0 to list.Length - 1 do
                if not (seen.Contains(j)) then
                    addGroup list[j]
        groups.ToArray()
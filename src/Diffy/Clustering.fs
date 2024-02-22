namespace Diffy

open System.Collections.Generic
open System.Runtime.CompilerServices
open Common

/// A module for implementing clustering algorithms.
module Clustering =

    // A potential cluster merge option.
    type ClusterMerge<'t>(cost : double, c1: PricedItem<'t>, c2 : PricedItem<'t>, result: PricedItem<'t>) =
        member this.Cost = cost
        member this.Cluster1 = c1
        member this.Cluster2 = c2
        member this.Result = result
        override this.ToString() = sprintf "ClusterMerge(%A, %A, %A)" this.Cost this.Cluster1 this.Cluster2

    // A custom comparer for the priority queue.
    type ClusterMergeComparer<'t>() = 
        interface IComparer<ClusterMerge<'t>> with
            member this.Compare(x, y) =
                if x.Cost < y.Cost then -1
                elif x.Cost > y.Cost then 1
                else
                    let xAddr = Unsafe.As<ClusterMerge<'t>, System.IntPtr>(ref x);
                    let yAddr = Unsafe.As<ClusterMerge<'t>, System.IntPtr>(ref y);
                    xAddr.CompareTo(yAddr);

    /// Cluster elements into groups based on their cost.
    /// Uses a simple greedy algorithm.
    let ClusterGreedyWithMerge (threshold : double) (combine : 't -> 't -> PricedItem<'t>) (items : 't[]) : PricedItem<'t>[] =
        let groups = List()
        groups.Add({ Item = items[0]; Cost = 0.0 })
        for i = 1 to items.Length - 1 do
            let item = items[i]
            let mutable minCost = System.Double.MaxValue
            let mutable minIndex = -1
            let mutable newGroup = Option.None
            for j = 0 to groups.Count - 1 do
                let result = combine groups[j].Item item
                if result.Cost < minCost then
                    minCost <- result.Cost
                    minIndex <- j
                    newGroup <- Option.Some result
            if minCost > threshold then
                groups.Add({ Item = item; Cost = 0.0 })
            else
                groups[minIndex] <- Option.get newGroup
        groups.ToArray()

    /// Cluster elements into groups based on their cost.
    /// Uses a priority-queue based implementation.
    let ClusterPriorityWithMerge (threshold : double) (combine : 't -> 't -> PricedItem<'t>) (items : 't[]) : PricedItem<'t>[] =
        // track the groups of elements
        let groups = List()
        groups.Add({ Item = items[0]; Cost = 0.0 })
        // track the items that have been matched to a group already
        let matched = Array.create items.Length false
        matched[0] <- true
        // track the active elements of the priority queue for each item
        let outstanding = Array.create items.Length []
        // keep track of the current best pairing
        let pqueue = SortedSet<double * int * int>()
        for i = 1 to items.Length - 1 do
            let result = combine items[0] items[i]
            let element = (result.Cost, 0, i)
            pqueue.Add(element) |> ignore
            outstanding[i] <- element :: outstanding[i]
        // keep pulling off the current next best pairing and adding that group.
        while pqueue.Count > 0 do
            // pick the next best pairing
            let best = pqueue.Min
            pqueue.Remove(best) |> ignore
            let (c, groupIdx, itemIdx) = best
            // mark the chosen element as matched remove its existing priority queue entries
            matched[itemIdx] <- true
            for elt in outstanding[itemIdx] do
                pqueue.Remove(elt) |> ignore
            // if it is a poor match, then create a new group
            if c > threshold then
                groups.Add({ Item = items[itemIdx]; Cost = 0.0 })
                for i = 0 to items.Length - 1 do
                    if not matched[i] then
                        let result = combine items[itemIdx] items[i]
                        let element = (result.Cost, groups.Count - 1, i)
                        pqueue.Add(element) |> ignore
                        outstanding[i] <- element :: outstanding[i]
            // if it is a good match, then add it to the group
            else
                let result = combine groups[groupIdx].Item items[itemIdx]
                groups[groupIdx] <- result
                // go through the queue and recompute costs until the top is better than the next
                let seen = HashSet<int * int>()
                let mutable flag = true
                while flag && pqueue.Count > 0 do
                    let best = pqueue.Min
                    let (_, groupIdx, itemIdx) = best
                    if seen.Contains (groupIdx, itemIdx) then
                        flag <- false
                    else
                        let result = combine groups[groupIdx].Item items[itemIdx]
                        let updated = (result.Cost, groupIdx, itemIdx)
                        seen.Add((groupIdx, itemIdx)) |> ignore
                        pqueue.Remove(best) |> ignore
                        pqueue.Add(updated) |> ignore
                        outstanding[itemIdx] <- updated :: outstanding[itemIdx]
        groups.ToArray()

    /// Cluster a collection of items into groups.
    /// Uses hierarchical clustering.
    let ClusterHierarchicalWithMerge (threshold : double) (combine : 't -> 't -> PricedItem<'t>) (items : 't[]) : PricedItem<'t>[] =
        // Get the initial clusters.
        let initial = Array.init items.Length (fun i -> { Item = items[i]; Cost = 0.0 })        
        // initialize the basic data structures.
        let clusters = HashSet<PricedItem<'t>>()
        let pqueue = SortedSet<ClusterMerge<'t>>(ClusterMergeComparer<'t>())
        let pqueueMap = Dictionary<'t, List<ClusterMerge<'t>>>(EqualityComparer<'t>.Default)
        // initialize the data structures.
        for i = 0 to initial.Length - 1 do
            clusters.Add(initial[i]) |> ignore
            pqueueMap[initial[i].Item] <- List()
        for i = 0 to initial.Length - 1 do
            for j = i + 1 to initial.Length - 1 do
                let result = combine initial[i].Item initial[j].Item
                let merge = ClusterMerge(result.Cost, initial[i], initial[j], result)
                pqueue.Add(merge) |> ignore;
                pqueueMap[initial[i].Item].Add(merge)
                pqueueMap[initial[j].Item].Add(merge)
        // keep selecting the best clusters to merge.
        while pqueue.Count > 0 && pqueue.Min.Cost <= threshold do
            let bestMerge = pqueue.Min
            let newCluster = bestMerge.Result
            // remove the old clusters.
            clusters.Remove(bestMerge.Cluster1) |> ignore
            clusters.Remove(bestMerge.Cluster2) |> ignore
            // remove the now invalid entries in the priority queue.
            for merge in pqueueMap[bestMerge.Cluster1.Item] do
                pqueue.Remove(merge) |> ignore
            for merge in pqueueMap[bestMerge.Cluster2.Item] do
                pqueue.Remove(merge) |> ignore
            // now add new entries for the new costs of every other cluster to this one.
            pqueueMap[newCluster.Item] <- List()
            for cluster in clusters do
                let result = combine newCluster.Item cluster.Item
                let merge = ClusterMerge(result.Cost, newCluster, cluster, result)
                pqueue.Add(merge) |> ignore
                pqueueMap[cluster.Item].Add(merge)
                pqueueMap[newCluster.Item].Add(merge)
            // add the new cluster.
            clusters.Add(newCluster) |> ignore
        Seq.toArray clusters

    /// Cluster elements into groups based on their cost.
    /// Uses a simple greedy algorithm.
    let ClusterGreedy (threshold : double) (cost : 't[] -> double) (items : 't[]) : 't[][] =
        let inline zip x y = let z = Array.append x y in { Item = z; Cost = cost z }
        let inline unzip (x : PricedItem<'t[]>) = x.Item
        let items = Array.map Array.singleton items
        let result = ClusterGreedyWithMerge threshold zip items
        Array.map unzip result

    /// Cluster elements into groups based on their cost.
    /// Uses a simple greedy algorithm.
    let ClusterGreedy2 (threshold : double) (cost : 't[] -> double) (items : 't[]) : 't[][] =
        let inline zip (group1, oldCost, smallerGroup1) (group2, _, _) =
            let newGroup = Array.append smallerGroup1 group2
            let c = cost newGroup
            let bigGroup = Array.append group1 group2
            if abs(c - oldCost) < 0.00001 then
                { Item = (bigGroup, c, smallerGroup1); Cost = c }
            else
                { Item = (bigGroup, c, newGroup); Cost = c }
        let inline unzip (pi : PricedItem<'t[] * double * 't[]>) = 
            let (x, _, _) = pi.Item in x
        let items = Array.map (fun i -> (Array.singleton i, 0.0, Array.singleton i)) items
        let result = ClusterGreedyWithMerge threshold zip items
        Array.map unzip result

    /// Cluster elements into groups based on their cost.
    /// Uses a simple greedy algorithm.
    let ClusterPriority (threshold : double) (cost : 't[] -> double) (items : 't[]) : 't[][] =
        let inline zip x y = let z = Array.append x y in { Item = z; Cost = cost z }
        let inline unzip (x : PricedItem<'t[]>) = x.Item
        let items = Array.map Array.singleton items
        let result = ClusterPriorityWithMerge threshold zip items
        Array.map unzip result

    /// Cluster elements into groups based on their cost.
    /// Uses a simple greedy algorithm.
    let ClusterHierarchical (threshold : double) (cost : 't[] -> double) (items : 't[]) : 't[][] =
        let inline zip x y = let z = Array.append x y in { Item = z; Cost = cost z }
        let inline unzip (x : PricedItem<'t[]>) = x.Item
        let items = Array.map Array.singleton items
        let result = ClusterHierarchicalWithMerge threshold zip items
        Array.map unzip result
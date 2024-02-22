namespace Diffy

open System.Collections.Generic

/// A module dedicated to utility functions.
module Anomalies = 
    
    /// An isolation tree.
    type private IsolationTree =
        | External of {| Size : int |}
        | Internal of {| SplitAttribute : int; SplitValue : double; Left : IsolationTree; Right : IsolationTree |}

    /// Harmonic number approximation.
    let inline private harmonic (i : int) : double = System.Math.Log(i) + 0.5772156649

    /// Approximates the average path length of an unsuccessful search in the tree.
    let inline private cost (n : int) =
        if n = 2 then 1.0
        elif n <= 1 then 0.0
        else 2.0 * (harmonic (n - 1)) - (2.0 * ((double)n - 1.0) / (double)n)

    /// Make an isolation tree.
    let rec private makeTree (rnd : System.Random) (data : double[][]) (height : int) : IsolationTree =
        if data.Length <= 1 then
            External {| Size = data.Length |}
        else
            let splitAttribute = rnd.Next(data[0].Length)
            let mutable min = System.Double.MaxValue
            let mutable max = System.Double.MinValue
            for datum in data do
                let value = datum[splitAttribute]
                if value < min then
                    min <- value
                if value > max then
                    max <- value
            if min = max then
                External {| Size = data.Length |}
            else
                let splitValue = rnd.NextDouble() * (max - min) + min
                let less, greater = data |> Array.partition (fun d -> d[splitAttribute] < splitValue)
                let left = makeTree rnd less (height + 1)
                let right = makeTree rnd greater (height + 1)
                Internal {| SplitAttribute = splitAttribute; SplitValue = splitValue; Left = left; Right = right |}

    /// Gets the path length of a data point in the isolation tree.
    let rec private pathLength (datum : double[]) (tree : IsolationTree) (height : int) : double =
        match tree with
        | External info -> (double)height + cost(info.Size)
        | Internal info ->
            let v = datum[info.SplitAttribute]
            if v < info.SplitValue then
                pathLength datum info.Left (height + 1)
            else
                pathLength datum info.Right (height + 1)

    /// Get anomaly scores from a dataset using isolation forests.
    let private isolationForest (data : double[][]) (numTrees : int) : double[] =
        let rnd = System.Random(0)
        let trees = List()
        for _ = 1 to numTrees do
            let tree = makeTree rnd data 0
            trees.Add(tree)
        let results = Array.create data.Length 0.0
        for i = 0 to data.Length - 1 do
            let averageLen = trees |> Seq.map (fun t -> pathLength data[i] t 0) |> Seq.average
            let anomalyScore = System.Math.Pow(2.0, -(averageLen / cost(data.Length)))
            results[i] <- anomalyScore
        results

    /// Get anomaly scores from a data set using isolation forests.
    let IsolationForest (data : double[][]) : double[] = isolationForest data 100

    /// Provides the anomaly scores given a set of inputs.
    let IsolationForestStrings (inputs : string[]) : double[] =
        let (isdouble, doubles) = inputs |> Array.map (System.Double.TryParse) |> Array.unzip
        if Array.forall id isdouble then 
            let encodings = doubles |> Array.map Array.singleton
            IsolationForest encodings
        else
            let unique = HashSet(inputs) |> Seq.toArray
            Array.sortInPlace unique
            let mapping = Dictionary()
            for i = 0 to unique.Length - 1 do
                mapping[unique[i]] <- i
            let encodings = inputs |> Array.map (fun x -> Array.singleton (double mapping[x]))
            IsolationForest encodings
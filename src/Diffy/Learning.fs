namespace Diffy

open System.Collections.Generic
open System.Linq
open Newtonsoft.Json
open Diffy.DataStructures


/// A module for learning from templates.
module Learning = 

    /// A predicate for the configurations.
    type Transformation =
        | Present
        | IsPattern of System.Object
        | Value
        | IsInteger
        | IsPowerTwo

    /// A learned invariant.
    and Outlier(config : int, path : string, template : Template, transformation : Transformation, value : string, score : double) =

        /// The configuration id.
        member this.Config = config

        /// The path of the invariant.
        member this.Path = path

        // the template that has the outlier.
        member this.Template = template

        /// The value for the outlier.
        member this.Value = value

        /// The anomaly score for the value.
        member this.AnomalyScore = score

        /// The transformation associated with the outlier.
        member this.Transformation = transformation

        /// Gets the function.
        member this.Function() =
            match this.Transformation with
            | Present -> "present"
            | IsPattern _ -> "pattern"
            | IsInteger -> "integer"
            | IsPowerTwo -> "pow2"
            | Value -> "value"

        /// Gets the function.
        member this.Arguments() =
            match this.Transformation with
            | IsPattern k ->
                if k = null then "null" 
                elif k.GetType() = typeof<string> then "'" + k.ToString() + "'"
                else k.ToString()
            | _ -> ""

    /// Learning parameters.
    type LearningParameters = {
        Support : double
        Confidence : double
        NumConfigs : int
        Filters : System.Text.RegularExpressions.Regex[]
    }

    /// An environment for the learning.
    type Environment = {
        Path : string
        Scope : BitSet
        NumConfigs : int
        Support : double
        Confidence : double
        InRepeat : bool
        Filters : System.Text.RegularExpressions.Regex[]
    }

    /// Extend an environment with a path.
    let updatePath (part : string) (env : Environment) : Environment =
        {env with Path = Diffy.Template.ExtendPath env.Path part}

    /// Extend an environment with a path.
    let updateScope (scope : BitSet) (env : Environment) : Environment =
        {env with Scope = scope}

    /// Returns true if there is enough support to continue.
    let isSupported (env : Environment) (scope : BitSet) : bool =
        let support = (double)(scope.Count()) / (double)env.NumConfigs
        support >= env.Support

    /// Provides the anomaly scores given a set of inputs.
    let anomalyScoresBitset (scope : BitSet) (valid : BitSet) : Dictionary<int, double> =
        let values = List()
        for i in scope do
            values.Add(if valid.Contains(i) then [|1.0|] else [|0.0|])
        let scores = Anomalies.IsolationForest (values.ToArray())
        let results = Dictionary()
        let mutable j = 0
        for i in scope do
            results[i] <- scores[j]
            j <- j + 1
        results
        
    /// Determines if a string is an integer.
    let isInteger (s : string) : string = let (success, _) = System.Int64.TryParse s in string success

    /// Determines if a string is a power of two.
    let isPowTwo (s : string) : string =
        let (success, v) = System.Int64.TryParse s
        let result = success && v > 0 && (v &&& (v - 1L) = 0L)
        string result

    /// Add an outlier to the results, if it qualifies.
    let private addOutlier (env : Environment) (outlier : Outlier) (results : List<Outlier>) =
        if outlier.AnomalyScore >= env.Confidence then
            let str = outlier.Function() + " " + outlier.Arguments() + " " + outlier.Value
            if not (env.Filters.Any(fun f -> f.IsMatch str)) then
                results.Add(outlier)

    /// Add the parameter outliers for a given transformation.
    let private addParameterOutliers (env : Environment) (template : Template) (transformation : Transformation) (transform : string -> string) (parameters : Strings.ConfigParameter[]) (results : List<Outlier>) =
        let values = parameters |> Array.map (fun p -> transform p.Value)
        let scores = Anomalies.IsolationForestStrings values
        for i = 0 to scores.Length - 1 do
            let score = scores[i]
            let parameter = parameters[i]
            let outlier = Outlier(parameter.Config, env.Path, template, transformation, transform parameter.Value, score)
            addOutlier env outlier results

    /// Get the base predicates for a string.
    let private addAllParameterOutliers (env : Environment) (template : Template) (parameters : Strings.ConfigParameter[]) (results : List<Outlier>) =
        if env.InRepeat then () else
        addParameterOutliers env template IsInteger isInteger parameters results
        addParameterOutliers env template IsPowerTwo isPowTwo parameters results
        addParameterOutliers env template Value id parameters results

    /// Computes the base predicates from a template.
    let rec private outliers (env : Environment) (t : Diffy.Template) (results : List<Outlier>) =
        if not (isSupported env env.Scope) then () else
        match t with
        | Diffy.Template.TNull -> ()
        | Diffy.Template.TConst _ -> ()
        | Diffy.Template.TAny p
        | Diffy.Template.TToken (_, p) ->
            let str = (Template.StringTemplateToString t)
            let env = env |> updatePath $"param[0]:{str}"
            addAllParameterOutliers env t p results
        | Diffy.Template.TConcat elts ->
            let mutable i = 0
            for elt in elts do
                match elt with
                | Diffy.Template.TAny p
                | Diffy.Template.TToken (_, p) ->
                    let str = Template.StringTemplateToString t
                    let env = env |> updatePath $"param[{i}]:{str}"
                    addAllParameterOutliers env t p results
                    i <- i + 1
                | _ -> ()
        | Diffy.Template.TList elts
        | Diffy.Template.TSet elts
        | Diffy.Template.TRepeat elts ->
            let env = {env with InRepeat = match t with Diffy.Template.TRepeat _ -> true | _ -> false}
            for i = 0 to elts.Length - 1 do
                let elt = elts[i]
                let newEnv = updatePath $"elt[{i}]" env
                match elt with
                | Diffy.Template.TCases cases ->
                    let mutable hasElt = BitSet(env.NumConfigs)
                    for case in cases do
                        hasElt <- hasElt.Union(case.Ids)
                    if cases.Length = 1 then
                        let pattern = 
                            match cases[0].Template with
                            | Diffy.Template.TNull -> "null"
                            | Diffy.Template.TConst _
                            | Diffy.Template.TAny _
                            | Diffy.Template.TToken _
                            | Diffy.Template.TConcat _ -> Template.StringTemplateToString cases[0].Template
                            | _ -> null
                        let str = if pattern = null then $"elt[{i}]" else $"elt[{i}]:{pattern}" 
                        let path = (updatePath str env).Path
                        let scores = anomalyScoresBitset env.Scope hasElt
                        for kv in scores do
                            let outlier = Outlier(kv.Key, path, elt, Present, string (hasElt.Contains(kv.Key)), kv.Value)
                            addOutlier env outlier results
                        outliers (newEnv |> updateScope hasElt) cases[0].Template results
                    else outliers newEnv elt results
                | _ -> outliers newEnv elt results
        | Diffy.Template.TCases cases ->
            let mapping = Dictionary()
            let inputs = List()
            for i = 0 to cases.Length - 1 do
                mapping[i] <- inputs.Count
                for _ in cases[i].Ids do
                    inputs.Add([|double i|])
            let scores = Anomalies.IsolationForest (inputs.ToArray())
            for i = 0 to cases.Length - 1 do
                let case = cases[i]
                let score = scores[mapping[i]]
                let transformation =
                    match case.Template with
                    | Diffy.Template.TNull -> IsPattern null
                    | Diffy.Template.TAny _
                    | Diffy.Template.TConst _
                    | Diffy.Template.TToken _
                    | Diffy.Template.TConcat _ -> IsPattern (Diffy.Template.StringTemplateToString case.Template)
                    | _ -> IsPattern i
                for id in case.Ids do
                    let outlier = Outlier(id, env.Path, t, transformation, "True", score)
                    addOutlier env outlier results
                outliers (updateScope case.Ids env) case.Template results
        | Diffy.Template.TRecord(keys, values) ->
            for i = 0 to keys.Length - 1 do
                let key = Diffy.Template.StringTemplateToString keys[i]
                let value = values[i]
                let newEnv = updatePath key env
                match value with
                | Diffy.Template.TCases cases ->
                    let mutable hasKey = BitSet(env.NumConfigs)
                    for case in cases do
                        hasKey <- hasKey.Union(case.Ids)
                    if cases.Length = 1 then
                        let scores = anomalyScoresBitset env.Scope hasKey
                        for kv in scores do
                            let outlier = Outlier(kv.Key, newEnv.Path, value, Present, string (hasKey.Contains(kv.Key)), kv.Value)
                            addOutlier env outlier results
                        outliers (newEnv |> updateScope hasKey) value results
                    else outliers newEnv value results
                | _ -> outliers newEnv value results

    /// Find outliers from a template.
    let FindOutliers (learningParams : LearningParameters) (t : Diffy.Template) : Outlier[] =
        let issues = List<Outlier>()
        let configs = BitSet(learningParams.NumConfigs)
        for i = 0 to learningParams.NumConfigs - 1 do
            configs.Add(i)
        let environment = {
            Path = "/"
            Scope = configs
            NumConfigs = learningParams.NumConfigs
            Support = learningParams.Support
            Confidence = learningParams.Confidence
            InRepeat = false
            Filters = learningParams.Filters
        }
        outliers environment t issues
        let results = issues.ToArray()
        results |> Array.sortInPlaceBy (fun i -> i.AnomalyScore)
        results |> System.Array.Reverse
        results
        
    /// A function to escape quotation marks and commas for a CSV string.
    let escapeQuotes (str: string) =
        let str = str.Replace("\n", "\\n").Replace("\r", "\\r")
        let escapedQuotes = str.Replace("\"", "\"\"")
        if escapedQuotes.Contains(",") || escapedQuotes.Contains("\"") then
            sprintf "\"%s\"" escapedQuotes
        else
            escapedQuotes

    /// Converts the invariants to the CSV format.
    let ToCsv (names : string[]) (includeTemplates : bool) (outliers : Outlier[]) : string =
        let sb = System.Text.StringBuilder()
        if includeTemplates then
            sb.AppendLine("Name,Expression,Score,Template") |> ignore
        else
            sb.AppendLine("Name,Expression,Score") |> ignore
        for outlier in outliers do
            let name = names[outlier.Config]
            let func = outlier.Function()
            let args = outlier.Arguments()
            let args = if args = "" then args else ", " + args
            let expression = func + "(" + outlier.Path + args + ") = '" + outlier.Value + "'"
            sb.Append(name |> escapeQuotes) |> ignore
            sb.Append(",") |> ignore
            sb.Append(expression |> escapeQuotes) |> ignore
            sb.Append(",") |> ignore
            sb.Append(outlier.AnomalyScore) |> ignore
            if includeTemplates then
                let config = Template.JsonConfig(names, parameters=true)
                let json = Template.ToJsonCountOnly outlier.Template config
                sb.Append(",") |> ignore
                sb.Append(json |> escapeQuotes) |> ignore
            sb.AppendLine() |> ignore
        let ret = sb.ToString()
        ret.TrimEnd([|'\n'; '\r'|])

    // Function to escape single quotes for HTML display
    let escapeForHtml (json: string) : string =
        json.Replace("'", "&apos;")

    // Function to convert your F# Outliers to a JavaScript-readable format
    let convertOutliersToJsData (names : string[]) (outliers : Outlier[]) : string =
        let convertOutlier (outlier: Outlier) =
            let expression = sprintf "%s(%s%s) = '%s'" (outlier.Function()) outlier.Path (if outlier.Arguments() = "" then "" else ", " + outlier.Arguments()) outlier.Value
            let config = Template.JsonConfig(names, parameters=true)
            let json = Template.ToJsonCountOnly outlier.Template config
            let jsonStr = escapeForHtml json
            [names[outlier.Config]; expression; outlier.AnomalyScore.ToString(); sprintf "<pre><code class='language-json'>%s</code></pre>" jsonStr]
        let data = outliers |> Array.map convertOutlier
        JsonConvert.SerializeObject(data, Formatting.None)

    /// Converts the invariants to the HTML format.
    let ToHTML (names : string[]) (outliers : Outlier[]) : string =
        let jsData = convertOutliersToJsData names outliers
        let jsCode = $"""
            var data = {jsData};

            $(document).ready(function () {{
                var table = $('#myTable').DataTable({{
                    "pageLength": 50,
                    "drawCallback": function( settings ) {{
                        Prism.highlightAll();
                    }},
                    data: data
                }});
                Prism.highlightAll();
            }});
        """
        // Template for HTML content, replace your actual HTML content here and leave {{JAVASCRIPT_CODE}} as a placeholder
        let htmlTemplate = $"""
            <!DOCTYPE html>
            <html>
            <head>
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width, initial-scale=1">
                <title>Configuration Anomaly Detection Results</title>
                <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css">
                <link rel="stylesheet" type="text/css" href="https://cdn.datatables.net/1.11.3/css/jquery.dataTables.min.css">
                <link href="https://cdnjs.cloudflare.com/ajax/libs/prism/1.23.0/themes/prism.min.css" rel="stylesheet" />
            </head>
            <body>
                <nav class="navbar navbar-expand-lg navbar-dark bg-dark fixed-top" style="padding: 20px 0;">
                    <a class="navbar-brand" href="#"></a>
                </nav>
                <div class="container-fluid" style="margin-top: 80px; width: 90">
                    <h2 class="mb-4">Anomaly Detection Results</h2>
                    <table id="myTable" class="row-border compact">
                        <thead>
                            <tr>
                                <th>Configuration</th>
                                <th>Reason</th>
                                <th>Anomaly Score</th>
                                <th>Outlier</th>
                            </tr>
                        </thead>
                        <tbody>
                        </tbody>
                    </table>
                </div>
                <script src="https://code.jquery.com/jquery-3.5.1.js"></script>
                <script type="text/javascript" src="https://cdn.datatables.net/1.11.3/js/jquery.dataTables.min.js"></script>
                <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.23.0/prism.min.js"></script>
                <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.23.0/plugins/autoloader/prism-autoloader.min.js"></script>
                <script>{jsCode}</script>
            </body>
            </html>
        """
        htmlTemplate
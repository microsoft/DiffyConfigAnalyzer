open CommandLine
open Diffy
open System.IO

/// Command line arguments for the Templater CLI.
type Options = {

    /// The directory containing the configurations.
    [<Option("dir", Required = true, HelpText = "The directory containing the configurations.")>]
    Directory : string

    /// The input data format (json).
    [<Option("format", Required = false, HelpText = "The configuration file format (default json).")>]
    Format : string

    /// Whether to enable printing.
    [<Option("print", Required = false, HelpText = "Print progress of the tool.")>]
    Printing : bool

    /// Path exclusions, separated by commas and slashes.
    [<Option("exclude", Required = false, HelpText = "Regex patterns for tree paths to exclude (example: /some/.*/path")>]
    Exclusions : seq<string>

    /// Path inclusions, separated by commas and slashes.
    [<Option("include", Required = false, HelpText = "Regex patterns for tree paths to include (Default: .*) (example: /some/.*/path")>]
    Inclusions : seq<string>

    /// Group threshold that determines when to group elements together.
    [<Option("group-threshold", Required = false, HelpText = "A group thresholding value between 0.0 and 1.0.")>]
    GroupThreshold : string

    /// List threshold that determines which list to prefer.
    [<Option("list-threshold", Required = false, HelpText = "A list thresholding value that determines which list to prefer.")>]
    ListTypeThreshold : string

    /// The tokens used to match patterns.
    [<Option("tokens", Required = false, HelpText = "A set of tokens given as regular expressions.")>]
    Tokens : seq<string>

    /// The costs associated with each token.
    [<Option("token-costs", Required = false, HelpText = "The costs for each of the tokens between 0.0 and 1.0.")>]
    TokenCosts : seq<double>

    /// Enable statistics.
    [<Option("stats", Required = false, HelpText = "Show stats only and no output.")>]
    Stats : bool

    /// The maximum number of elements in a list to template.
    [<Option("max-list-size", Required = false, HelpText = "The maximum number of elements in a list to template.")>]
    MaxListSize : string

    /// Whether to use exact key matching for records.
    [<Option("count-only", Required = false, HelpText = "Includes only the group count and not members in the results.")>]
    CountOnly : bool

    /// Whether to use exact key matching for records.
    [<Option("loose-key-match", Required = false, HelpText = "Allows loose key matching in records.")>]
    LooseKeyMatch : bool

    /// Whether to disable the use of repeat templates.
    [<Option("disable-repeat", Required = false, HelpText = "Disables the use of repeat templates.")>]
    DisableRepeat : bool

    /// Whether to dump parameter values.
    [<Option("parameters", Required = false, HelpText = "Dump parameter values in the output.")>]
    Parameters : bool

    /// Support threshold.
    [<Option("support", Required = false, HelpText = "The minimum support needed to flag an outlier (default 0.2).")>]
    Support : string

    /// Confidence threshold.
    [<Option("confidence", Required = false, HelpText = "The minimum confidence needed to flag an outlier (default 0.7).")>]
    Confidence : string

    /// The outliers file to write to
    [<Option("outliers-path", Required = false, HelpText = "The outliers output file path (without extension).")>]
    OutliersFilePath : string

    /// Whether to enable printing.
    [<Option("outliers-include-template", Required = false, HelpText = "Whether to include the template text in the CSV file.")>]
    OutliersIncludeTemplate : bool

    /// Whether to enable printing.
    [<Option("outliers-html", Required = false, HelpText = "Whether to output outliers in HTML format for visual summary instead of CSV.")>]
    OutliersHTML : bool

    /// The template file path.
    [<Option("template-path", Required = false, HelpText = "The output file path (without extension).")>]
    OutputFilePath : string

    /// The filters to apply to outliers.
    [<Option("filter-outliers", Required = false, HelpText = "A set of filters to apply to outlier descriptions.")>]
    OutlierFilters : seq<string>
}

/// An input that does not parse.
let helpText =
    let parser = new CommandLine.Parser(fun settings -> settings.HelpWriter <- null)
    let notParseResult = parser.ParseArguments<Options>([|"--format"; "json"|])
    CommandLine.Text.HelpText.AutoBuild(notParseResult).ToString()

/// Throw an error and exit the program.
let error (msg : string) =
    let msg = helpText.Replace("Required option 'dir' is missing.", msg)
    System.Console.WriteLine(msg);
    exit 0

/// Make a regex from a path.
let makePathPattern (s : string) : System.Text.RegularExpressions.Regex =
    System.Text.RegularExpressions.Regex("^" + s + "$", System.Text.RegularExpressions.RegexOptions.Compiled)

/// Make an inclusion regex from a path with all the prefixes
let makePathInclusionPattern (s : string) : System.Text.RegularExpressions.Regex[] = 
    let elements = s.Split [|'/'|]
    let mutable regexes = [] 
    let mutable str = ""
    for ele in elements do
        if ele.Length <> 0 then
            str <- str + "/" + ele
            regexes <- System.Text.RegularExpressions.Regex("^" + str + "$", System.Text.RegularExpressions.RegexOptions.Compiled) :: regexes
    regexes |> List.toArray

/// Makes a new token from a string.
let makeToken (s : string, cost : double) : Token =
    let index = s.IndexOf(':')
    if index < 0 then
        error $"Invalid token with missing name {s}. Use name:regex pattern."
    let name = s.Substring(0, index)
    let regex = s.Substring(index + 1)
    Token(name, "^" + regex + "$", cost)

/// Parse an integer parameter or fail if invalid.
let parseInteger (s : string) (defaultValue : int) : int =
    let str = if System.String.IsNullOrEmpty s then string defaultValue else s
    let (parse, result) = System.Int32.TryParse str
    if not parse || result < 0 then
        error $"Invalid integer parameter: {s}"
    result

/// Parse an integer parameter or fail if invalid.
let parseIntegerNeg (s : string) (defaultValue : int) : int =
    let str = if System.String.IsNullOrEmpty s then string defaultValue else s
    let (parse, result) = System.Int32.TryParse str
    let result = if result < 0 then -1 else result
    if not parse then
        error $"Invalid integer parameter: {s}"
    result

/// Parse a double parameter in the range 0.0 to 1.0 or fail if invalid.
let parseFloat (s : string) (defaultValue : double) : double =
    let str = if System.String.IsNullOrEmpty s then string defaultValue else s
    let (parse, result) = System.Double.TryParse str
    if not parse || result < 0.0 || result > 1.0 then
        error $"Invalid threshold value {s}"
    result

/// Parse the command line arguments.
let parseCommands (argv : string[]) =
    let parseResult = CommandLine.Parser.Default.ParseArguments<Options>(argv)
    let errors = List.ofSeq parseResult.Errors
    if errors.Length <> 0 then
        exit 0
    parseResult.Value

/// Validate that a directory exists.
let validateDirectory (dir : string) =
    if not (Directory.Exists dir) then
        error $"Invalid directory {dir}"

/// Parse the paths as regexes.
let parseInclusionPaths (paths : seq<string>) = 
    if paths = null then [||] else paths |> Seq.map makePathInclusionPattern |> Seq.toArray |> Array.concat

/// Parse the paths as regexes.
let parseExclusionPaths (paths : seq<string>) = 
    if paths = null then [||] else paths |> Seq.map makePathPattern |> Seq.toArray

/// Parse the paths as regexes.
let parseOutlierFilters (filters : seq<string>) : System.Text.RegularExpressions.Regex[] = 
    filters
    |> Seq.toArray
    |> Array.map (fun f -> System.Text.RegularExpressions.Regex(f, System.Text.RegularExpressions.RegexOptions.Compiled))

/// Parse tokens from their definitions and costs.
let parseTokens (tokens : seq<string>) (costs : seq<double>) : Token[] =
    let tokens = Seq.toArray tokens
    let tokenCosts = Seq.toArray costs
    if tokens.Length <> tokenCosts.Length then
        error "Mismatch in number of tokens and their costs"
    Array.zip tokens tokenCosts |> Array.map makeToken

/// Parse the format to get the formatter functions and file extension.
let parseFormat (format : string) : {| From : string -> Tree; To : System.Text.Json.Nodes.JsonNode -> string; Extension : string |} =
    match format with
    | null
    | ""
    | "json" -> {| From = Tree.FromJson; To = Common.WriteJson; Extension = "json" |}
    | _ -> error $"Invalid format {format}"

/// Main entry point for the CLI program.
/// Templates a collection of configurations and finds
/// any outliers.
[<EntryPoint>]
let main argv =

    // parse the command line arguments.
    let options = parseCommands argv
    let defaultEnv = Template.DefaultEnvironment()
    let directory = options.Directory
    let format = parseFormat options.Format
    let exclusions = parseExclusionPaths options.Exclusions
    let inclusions = parseInclusionPaths options.Inclusions
    let tokens = parseTokens options.Tokens options.TokenCosts
    let maxListSize = parseIntegerNeg options.MaxListSize defaultEnv.MaxListSize
    let groupingThreshold = parseFloat options.GroupThreshold defaultEnv.GroupingThreshold
    let listTypeThreshold = parseFloat options.ListTypeThreshold defaultEnv.ListTypeThreshold
    let support = parseFloat options.Support 0.2
    let confidence = parseFloat options.Confidence 0.7
    let outlierFilters = parseOutlierFilters options.OutlierFilters
    let debug = options.Printing
    let disableRepeat = options.DisableRepeat
    let countOnly = options.CountOnly
    let exactKeyMatching = not options.LooseKeyMatch

    // perform any additional validation on the arguments.
    if not (Directory.Exists directory) then
        error $"Invalid directory {directory}"
    if support < 0.0 || support > 1.0 || confidence < 0.0 || confidence > 1.0 then
        error "Invalid support or confidence: must be between 0.0 and 1.0"

    // Parse all of the configuration files and get their tree representations.
    let watch = System.Diagnostics.Stopwatch.StartNew()
    let files = Directory.GetFiles directory
    let fileNames = Array.map (fun (f : string) -> Path.GetFileName f) files
    let trees = Array.map (fun f -> format.From (File.ReadAllText f)) files

    // Run the templater to get the template for the configurations.
    let env = {
        Template.DefaultEnvironment() with
            Tokens = tokens
            GroupingThreshold = groupingThreshold
            ListTypeThreshold = listTypeThreshold
            ExactKeyMatching = exactKeyMatching
            Inclusions = inclusions
            Exclusions = exclusions
            CountOnly = countOnly
            DisableRepeat = disableRepeat
            MaxListSize = maxListSize
            Debug = debug
    }
    let parsingTime = watch.ElapsedMilliseconds
    let template =  Template.InferWithOptions(trees, env)
    let templateTime = watch.ElapsedMilliseconds - parsingTime
    
    // learn the invariants from the template.
    let learningParams : Learning.LearningParameters = {
        Support = support
        Confidence = confidence
        Filters = outlierFilters
        NumConfigs = fileNames.Length
    }
    let invariants = Learning.FindOutliers learningParams template
    let learningTime = watch.ElapsedMilliseconds - (templateTime + parsingTime)
    let totalTime = templateTime + parsingTime + learningTime    

    // write the stats if requested.
    if options.Stats then
        System.Console.WriteLine(sprintf "+---------------------------------------+")
        System.Console.WriteLine(sprintf "| Parsing time (ms):   %-16d |" parsingTime)
        System.Console.WriteLine(sprintf "| Template time (ms):  %-16d |" templateTime)
        System.Console.WriteLine(sprintf "| Learning time (ms):  %-16d |" learningTime)
        System.Console.WriteLine(sprintf "| String time (ms):    %-16d |" env.Stats.InferStringTime)
        System.Console.WriteLine(sprintf "| Total time (ms):     %-16d |" totalTime)
        System.Console.WriteLine(sprintf "+---------------------------------------+")
        System.Console.WriteLine(sprintf "| String inferences:   %-16d |" (env.Stats.StringCacheHit + env.Stats.StringCacheMiss))
        System.Console.WriteLine(sprintf "| List inferences:     %-16d |" (env.Stats.ListCacheHit + env.Stats.ListCacheMiss))
        System.Console.WriteLine(sprintf "| Set inferences:      %-16d |" (env.Stats.SetCacheHit + env.Stats.SetCacheMiss))
        System.Console.WriteLine(sprintf "| Repeat inferences:   %-16d |" (env.Stats.RepeatCacheHit + env.Stats.RepeatCacheMiss))
        System.Console.WriteLine(sprintf "| Record inferences:   %-16d |" (env.Stats.RecordCacheHit + env.Stats.RecordCacheMiss))
        System.Console.WriteLine(sprintf "+---------------------------------------+")
        System.Console.WriteLine(sprintf "| String cache hits:   %-16d |" env.Stats.StringCacheHit)
        System.Console.WriteLine(sprintf "| List cache hits:     %-16d |" env.Stats.ListCacheHit)
        System.Console.WriteLine(sprintf "| Set cache hits:      %-16d |" env.Stats.SetCacheHit)
        System.Console.WriteLine(sprintf "| Repeat cache hits:   %-16d |" env.Stats.RepeatCacheHit)
        System.Console.WriteLine(sprintf "| Record cache hits:   %-16d |" env.Stats.RecordCacheHit)
        System.Console.WriteLine(sprintf "+---------------------------------------+")
        System.Console.WriteLine(sprintf "| Trees total:         %-16d |" env.Stats.TreesTotal)
        System.Console.WriteLine(sprintf "| Trees removed:       %-16d |" env.Stats.TreesRemoved)
        System.Console.WriteLine(sprintf "+---------------------------------------+")
    
    // write the template to a file.
    if options.OutputFilePath <> null then
        let jsonConfig = Template.JsonConfig(fileNames, parameters = options.Parameters)
        let result = Template.ToJsonTree env template jsonConfig |> format.To
        File.WriteAllText(options.OutputFilePath + "." + format.Extension, result)

    // write the bugs/outliers and their justifications to a file.
    if options.OutliersFilePath <> null then
        if options.OutliersHTML then
            let html = Learning.ToHTML fileNames invariants
            File.WriteAllText(options.OutliersFilePath + ".html", html)
        else
            let csv = Learning.ToCsv fileNames options.OutliersIncludeTemplate invariants
            File.WriteAllText(options.OutliersFilePath + ".csv", csv)

    0
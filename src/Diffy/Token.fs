namespace Diffy

open Diffy.Regex

/// A custom user token for describing patterns.
type Token(name : string, pattern : string, cost : double, ?greedy: bool) =
    
    /// the regex options.
    let options = System.Text.RegularExpressions.RegexOptions.Singleline ||| System.Text.RegularExpressions.RegexOptions.Compiled

    /// the dotnet regex.
    let dotnetRegex = System.Text.RegularExpressions.Regex(pattern, options)
    
    /// The regex that can be turned to a DFA.
    let regex = Regex.Parse(pattern)

    /// The DFA.
    let automaton = regex.ToAutomaton()

    /// if this matches the empty string.
    let matchesEmpty = dotnetRegex.IsMatch ""

    /// whether this is a greedy token.
    let isGreedy = defaultArg greedy true

    /// The name of the token.
    member this.Name = name

    /// The regular expression pattern for the token.
    member this.Regex = regex

    /// The automaton for the regex.
    member this.Automaton = automaton

    /// The dotnet regex.
    member this.DotnetRegex = dotnetRegex

    /// Whether this matches the empty string.
    member this.MatchesEmptyString = matchesEmpty

    /// The per-character cost (between 0.0 and 1.0) for
    /// matching with this regular expression.
    member this.Cost = cost

    // Whether the token will have a greedy match
    member this.Greedy = isGreedy

    /// Force evaluation of a lazy string to create a string.
    /// Uses a string builder to avoid repeated appending.
    override this.ToString() : string = $"Token({this.Name})"
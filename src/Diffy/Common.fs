namespace Diffy

open System.Text.Json

/// A module dedicated to utility functions.
module Common = 
    
    /// A priced item.
    type PricedItem<'t> = { Item: 't; Cost: double }

    /// A perfect hash function for two integers.
    let PerfectHash (x : uint64) (y : uint64) : uint64 =
        let sum = x + y
        sum * (sum + 1UL) / 2UL + x

    /// Write a json node to json.
    let WriteJson (json : Nodes.JsonNode) : string =
        let mutable options = JsonSerializerOptions()
        options.WriteIndented <- true
        options.Encoder <- System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
        json.ToJsonString(options)
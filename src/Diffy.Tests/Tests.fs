namespace Diffy.Tests

open Diffy
open Diffy.Strings
open Diffy.Matching
open Diffy.Regex
open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis


/// Test class for testing the templating logic
/// as well as all the templating helper functions.
[<TestClass>]
[<ExcludeFromCodeCoverage>]
type TemplateTests () =

    /// approximate equality for testing costs
    let approx_eq x y = abs (x - y) < 0.01

    /// Test that the template inference is working for basic strings.
    [<TestMethod>]
    member this.TestStringInference1 () =
        let t = Template.Infer [|Tree.Leaf("iface-1", typeof<string>); Tree.Leaf("iface-2", typeof<string>)|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson "\"iface-[any]\""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic strings.
    [<TestMethod>]
    member this.TestStringInference2 () =
        let t = Template.Infer [|
            Tree.Leaf("iface-1", typeof<string>);
            Tree.Leaf("iface-2", typeof<string>);
            Tree.Leaf("iface-3", typeof<string>)
        |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson "\"iface-[any]\""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic strings.
    [<TestMethod>]
    member this.TestStringInference3 () =
        let t = Template.Infer [|
            Tree.Leaf("iface-1", typeof<string>);
            Tree.Leaf("iface-2", typeof<string>);
            Tree.Leaf("abc-iface", typeof<string>)
        |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson "\"[any]iface[any]\""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic strings.
    [<TestMethod>]
    member this.TestStringInference4 () =
        let t = Template.Infer [|
            Tree.Leaf("iface-1", typeof<string>);
            Tree.Leaf("iface-2", typeof<string>);
            Tree.Leaf("zzzz", typeof<string>)
        |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          {
            "CASES": [
              "C1",
              "C2"
            ],
            "TEMPLATE": "iface-[any]"
          },
          {
            "CASES": [
              "C3"
            ],
            "TEMPLATE": "zzzz"
          }
        ]"""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic strings.
    [<TestMethod>]
    member this.TestStringInference5 () =
        let t = Template.Infer [|
            Tree.Leaf("", typeof<string>);
            Tree.Leaf("", typeof<string>);
        |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson "\"\""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic strings.
    [<TestMethod>]
    member this.TestStringInference6 () =
        let t = Template.Infer [|
            Tree.Leaf("foo-6", typeof<string>);
            Tree.Leaf("foo-6", typeof<string>);
            Tree.Leaf("foo-6", typeof<string>);
            Tree.Leaf("foo-6", typeof<string>);
            Tree.Leaf("foo-6", typeof<string>);
            Tree.Leaf("foo-6", typeof<string>);
        |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3"; "C4"; "C5"; "C6" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson "\"foo-6\""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic strings.
    [<TestMethod>]
    member this.TestStringInference7 () =
        let t = Template.Infer [|
            Tree.Leaf("1000:[1-9][0-9]$", typeof<string>);
            Tree.Leaf("1000:[1-9][0-9]$", typeof<string>);
        |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson "\"1000:[1-9][0-9]$\""
        Assert.IsTrue((actual = expected))
    
    /// Test that the template inference is working for basic strings with cases
    [<TestMethod>]
    member this.TestStringInference8() =
        let t = Template.Infer [|
            Tree.Leaf("iface-1", typeof<string>);
            Tree.Leaf("iface-1", typeof<string>);
            Tree.Leaf("zzzz", typeof<string>)
        |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          {
            "CASES": [
              "C1",
              "C2"
            ],
            "TEMPLATE": "iface-1"
          },
          {
            "CASES": [
              "C3"
            ],
            "TEMPLATE": "zzzz"
          }
        ]"""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic strings with cases
    [<TestMethod>]
    member this.TestStringInference9() =
        let t1 = Tree.FromJson """{"key": "4134"}"""
        let t2 = Tree.FromJson """{"key": "58807"}"""
        let t3 = Tree.FromJson """{"key": "4134"}"""
        let t4 = Tree.FromJson """{"key": "58807"}"""
        let t = Template.Infer [|t1; t2; t3; t4|]
        let jsonConfig = Template.JsonConfig([|"A"; "B"; "C"; "D"|])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        {
          "key": [
            {
              "CASES": [
                "A",
                "C"
              ],
              "TEMPLATE": "4134"
            },
            {
              "CASES": [
                "B",
                "D"
              ],
              "TEMPLATE": "58807"
            }
          ]
        }
        """
        Assert.IsTrue ((actual = expected))
    
    /// Test that the template inference is working with basic tokens.
    [<TestMethod>]
    member this.TestTokenInference1 () =
        let trees = [|
            Tree.Leaf("iface-1", typeof<string>)
            Tree.Leaf("iface-2", typeof<string>)
            Tree.Leaf("iface-3", typeof<string>)
        |]
        let token = Token("number", "^[0-9]+$", 0.3)
        let t = Template.InferWithOptions(trees, {Template.DefaultEnvironment() with Tokens = [| token |]})
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson "\"iface-[number]\""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working with basic tokens.
    [<TestMethod>]
    member this.TestTokenInference2 () =
        let trees = [|
            Tree.Leaf("Ryan", typeof<string>)
            Tree.Leaf("John", typeof<string>)
            Tree.Leaf("Paul", typeof<string>)
        |]
        let token = Token("digit?", "^[0-9]?$", 0.3)
        let t = Template.InferWithOptions(trees, {Template.DefaultEnvironment() with Tokens = [| token |]})
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          {
            "CASES": [
              "C1"
            ],
            "TEMPLATE": "Ryan"
          },
          {
            "CASES": [
              "C2"
            ],
            "TEMPLATE": "John"
          },
          {
            "CASES": [
              "C3"
            ],
            "TEMPLATE": "Paul"
          },
        ]
        """
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working with basic tokens.
    [<TestMethod>]
    member this.TestTokenInference3 () =
        let trees = [|
            Tree.Leaf("10.1.0.255", typeof<string>)
            Tree.Leaf("10.0.0.1", typeof<string>)
            Tree.Leaf("10.3.0.2", typeof<string>)
        |]
        let t1 = Token("digit", "^[0-9]$", 0.29)
        let t2 = Token("digits", "^[0-9]+$", 0.3)
        let t = Template.InferWithOptions(trees, {Template.DefaultEnvironment() with Tokens = [| t1; t2 |]})
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson "\"10.[digit].0.[digits]\""
        Assert.IsTrue((actual = expected))

     /// Test that the template inference is working with basic tokens.
    [<TestMethod>]
    member this.TestTokenInference4 () =
        let trees = [|
            Tree.Leaf("1000", typeof<string>)
            Tree.Leaf("3306", typeof<string>)
            Tree.Leaf("3310", typeof<string>)
        |]
        let tok = Token("digits", "^[0-9]+$", 0.3)
        let t = Template.InferWithOptions(trees, {Template.DefaultEnvironment() with Tokens = [| tok |]})
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson "\"[digits]\""
        Assert.IsTrue((actual = expected))

    /// Test that gets into stackoverflow exception for ToAutomaton
    [<TestMethod>]
    member this.TestTokenInference5 () =
        let trees = [|
            Tree.Leaf("\"AU221AWP3012-slow.log\"", typeof<string>)
            Tree.Leaf("/var/lib/mysql/mysql-slow.log", typeof<string>)
            Tree.Leaf("\"WINGRA-DB01-slow.log\"", typeof<string>)
        |]
        let token = Token("number", "^[0-9]+$", 0.3, greedy=false)
        let t = Template.InferWithOptions(trees, {Template.DefaultEnvironment() with Tokens = [| token |]})
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          {
            "CASES": [
              "C1",
              "C3"
            ],
            "TEMPLATE": "\"[any]A[any][number][any][number]-slow.log\""
          },
          {
            "CASES": [
              "C2"
            ],
            "TEMPLATE": "/var/lib/mysql/mysql-slow.log"
          }
        ]"""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working with basic tokens.
    [<TestMethod>]
    member this.TestTokenInference6 () =
        let trees = [|
            Tree.Leaf("48G", typeof<string>)
            Tree.Leaf("5GB", typeof<string>)
            Tree.Leaf("10", typeof<string>)
        |]
        let token = Token("number", "^[0-9]+$", 0.1)
        let t = Template.InferWithOptions(trees, {Template.DefaultEnvironment() with Tokens = [| token |]})
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |], parameters=false)
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson "\"[number][any]\""
        Assert.IsTrue((actual = expected))
        
    /// Test that the input exclusion paths are not present in the final template
    [<TestMethod>]
    member this.TestExclusionOption () =
        let t1 = Tree.FromJson """{
            "RouteFilterMatches":[
                {
                    "LineNumber": 4012,
                    "Match": "foo"
                }
            ]
        }"""
        let t2 = Tree.FromJson """{
            "RouteFilterMatches":[
                {
                    "LineNumber": 3297,
                    "Match": "foo"
                }
            ]
        }"""
        let t3 = Tree.FromJson """{
            "RouteFilterMatches":[
                {
                    "LineNumber": 3985,
                    "Match": "foo"
                }
            ]
        }"""
        let makePathPattern (s : string) = System.Text.RegularExpressions.Regex("^" + s + "$")
        let exclusions = [|".*LineNumber"|] |> Seq.map makePathPattern |> Seq.toArray
        let inclusions = [|".*RouteFilterMatches.*"|] |> Seq.map makePathPattern |> Seq.toArray
        let env = {Template.DefaultEnvironment() with Exclusions = exclusions; Inclusions = inclusions }
        let t = Template.InferWithOptions ([| t1; t2; t3 |], env )
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """{
          "RouteFilterMatches": [
            "@type:ordered",
            {
              "Match": "foo"
            }
          ]
        }"""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestSetInference1 () =
        let t1 = Tree.FromJson """["iface-1", "baz"]"""
        let t2 = Tree.FromJson """["baz", "iface-3"]"""
        let t3 = Tree.FromJson """["foo"]"""
        let t = Template.Infer [| t1; t2; t3 |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          "@type:unordered",
          {
            "CASES": [
              "C1",
              "C2"
            ],
            "TEMPLATE": "iface-[any]"
          },
          {
            "CASES": [
              "C1",
              "C2"
            ],
            "TEMPLATE": "baz"
          },
          {
            "CASES": [
              "C3"
            ],
            "TEMPLATE": "foo"
          }
        ]"""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestSetInference2 () =
        let t1 = Tree.FromJson """["iface-1", "bar"]"""
        let t2 = Tree.FromJson """["baz", "iface-3"]"""
        let t3 = Tree.FromJson """["iface-2", "bar"]"""
        let t = Template.Infer [| t1; t2; t3 |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          "@type:unordered",
          "iface-[any]",
          "ba[any]"
        ]"""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestSetInference3 () =
        let t1 = Tree.FromJson """[["A"], ["B", "C"]]"""
        let t2 = Tree.FromJson """[["C", "B"], ["A"]]"""
        let t = Template.Infer [|t1; t2|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          "@type:unordered",
          [
            "@type:ordered",
            "A"
          ],
          [
            "@type:unordered",
            "B",
            "C"
          ]
        ]"""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestSetInference4 () =
        let t1 = Tree.FromJson """[
            {
              "class": "foo",
              "tag": "1000:30000"
            },
            {
              "class": "foo",
              "tag": "1000:39999"
            }
        ]"""
        let t2 = Tree.FromJson """[
            {
              "class": "foo",
              "tag": "1000:39999"
            },
            {
              "class": "foo",
              "tag": "1000:30000"
            }
        ]"""
        let t = Template.Infer [|t1; t2|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          "@type:unordered",
          {"class": "foo", "tag": "1000:30000" },
          {"class": "foo", "tag": "1000:39999" }
        ]"""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestSetInference5 () =
        let t = Template.Infer [|
            Tree.List [|
                Tree.Leaf("100:[1][0][0-3,5-9][0-9][0-9]$", typeof<string>)
                Tree.Leaf("100:[1-9]$", typeof<string>)
                Tree.Leaf("100:[1-9][0-9]$", typeof<string>)
                Tree.Leaf("100:[1][1-9][0-9][0-9][0-9]$", typeof<string>)
            |]
            Tree.List [|
                Tree.Leaf("100:[1][1-9][0-9][0-9][0-9]$", typeof<string>)
                Tree.Leaf("100:[1-9][0-9]$", typeof<string>)
                Tree.Leaf("100:[1-9]$", typeof<string>)
                Tree.Leaf("100:[1][0][0-3,5-9][0-9][0-9]$", typeof<string>)
            |]
        |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          "@type:unordered",
          "100:[1][0][0-3,5-9][0-9][0-9]$",
          "100:[1-9]$",
          "100:[1-9][0-9]$",
          "100:[1][1-9][0-9][0-9][0-9]$"
        ]"""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic lists.
    [<TestMethod>]
    member this.TestListInference1 () =
        let t1 = Tree.FromJson """["A", "B", "C", "D", "E", "F", "G", "H", "I", "J"]"""
        let t2 = Tree.FromJson """["A", "B", "Y", "D", "E", "F", "G", "H", "I", "J"]"""
        let t3 = Tree.FromJson """["A", "B", "Z", "D", "E", "F", "G", "H", "I", "J"]"""
        let t = Template.Infer [|t1; t2; t3|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          "@type:ordered",
          "A",
          "B",
          {
            "CASES": [
                "C1"
            ],
            "TEMPLATE": "C"
          },
          {
            "CASES": [
                "C2"
            ],
            "TEMPLATE": "Y"
          },
          {
            "CASES": [
                "C3"
            ],
            "TEMPLATE": "Z"
          },
          "D",
          "E",
          "F",
          "G",
          "H",
          "I",
          "J"
        ]"""
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic lists.
    [<TestMethod>]
    member this.TestListInference2 () =
        let t1 = Tree.FromJson """[]"""
        let t2 = Tree.FromJson """[]"""
        let t = Template.Infer [|t1; t2|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson "[]"
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic repeat.
    [<TestMethod>]
    member this.TestRepeatInference1 () =
        let t1 = Tree.FromJson """["A", "A", "A", "A", "B"]"""
        let t2 = Tree.FromJson """["A", "B", "B", "B"]"""
        let t = Template.Infer [|t1; t2|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          "@type:repeat-one-of",
          "A",
          "B"
        ]
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic repeat.
    [<TestMethod>]
    member this.TestRepeatInference2 () =
        let t1 = Tree.FromJson """["A", "A", "A", "A", "A"]"""
        let t2 = Tree.FromJson """["B", "B", "B", "B"]"""
        let t = Template.Infer [|t1; t2|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          "@type:repeat-one-of",
          {
            "CASES": [
               "C1"
            ],
            "TEMPLATE": "A",
          },
          {
            "CASES": [
               "C2"
            ],
            "TEMPLATE": "B",
          }
        ]
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic repeat.
    [<TestMethod>]
    member this.TestRepeatInference3 () =
        let t1 = Tree.FromJson """["A", "A", "A", "A", "A"]"""
        let t2 = Tree.FromJson """["A", "A"]"""
        let t = Template.Infer [|t1; t2|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          "@type:repeat-one-of",
          "A"
        ]
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic repeat.
    [<TestMethod>]
    member this.TestRepeatInference4 () =
        let t1 = Tree.FromJson """["A", "B"]"""
        let t2 = Tree.FromJson """["C", "D"]"""
        let t = Template.Infer [|t1; t2|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          "@type:ordered",
          {
            "CASES": [
               "C1"
            ],
            "TEMPLATE": "A",
          },
          {
            "CASES": [
               "C1"
            ],
            "TEMPLATE": "B",
          },
          {
            "CASES": [
               "C2"
            ],
            "TEMPLATE": "C",
          },
          {
            "CASES": [
               "C2"
            ],
            "TEMPLATE": "D",
          }
        ]
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestRepeatInference5 () =
        let t1 = Tree.FromJson """["iface-1", "bar"]"""
        let t2 = Tree.FromJson """["baz", "iface-3"]"""
        let t3 = Tree.FromJson """["iface-2"]"""
        let t = Template.Infer [| t1; t2; t3 |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          "@type:unordered",
          "iface-[any]",
          {
            "CASES": [
               "C1",
               "C2"
            ],
            "TEMPLATE": "ba[any]",
          }
        ]
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestRepeatInference6 () =
        let t1 = Tree.FromJson """
        {
            "Name": "Alice",
            "Balance": "$320",
            "ShoppingCart": [
                {
                    "id": "0792",
                    "quantity": 1
                },
                {
                    "id": "5129",
                    "quantity": 1
                },
                {
                    "id": "1145",
                    "quantity": 2
                }
            ]
        }   
        """
        let t2 = Tree.FromJson """
        {
            "Name": "Bob",
            "Balance": "$129",
            "ShoppingCart": [
                {
                    "id": "9034",
                    "quantity": 1
                }
            ]
        }
        """
        let t3 = Tree.FromJson """
        {
            "Name": "Chuck",
            "Balance": "$143",
            "ShoppingCart": [
                {
                    "id": "8051",
                    "quantity": 3
                }
            ]
        }
        """
        let tokens = [|
            Token("digit", "^[0-9]$", 0.2)
        |]
        let t = Template.InferWithOptions([| t1; t2; t3 |], {Template.DefaultEnvironment() with Tokens = tokens} )
        let jsonConfig = Template.JsonConfig([|"sample1.json"; "sample2.json"; "sample3.json"|])
        let json = Template.ToJson t jsonConfig
        let actual = Tree.FromJson json
        let expected = Tree.FromJson """
        {
          "Name": [
            {
              "CASES": [
                "sample1.json"
              ],
              "TEMPLATE": "Alice"
            },
            {
              "CASES": [
                "sample2.json"
              ],
              "TEMPLATE": "Bob"
            },
            {
              "CASES": [
                "sample3.json"
              ],
              "TEMPLATE": "Chuck"
            }
          ],
          "Balance": "$[digit][digit][digit]",
          "ShoppingCart": [
            "@type:repeat-one-of",
            {
              "id": "[digit][digit][digit][digit]",
              "quantity": "[digit]"
            }
          ]
        }
        """
        Assert.IsTrue((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestRepeatInference7 () =
        let t1 = Tree.FromJson """["iface-1", "iface-2", "iface-10"]"""
        let t2 = Tree.FromJson """["iface-12", "iface-13", "iface-14", "iface-15"]"""
        let t3 = Tree.FromJson """["iface-2"]"""
        let t = Template.Infer [| t1; t2; t3 |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
          "@type:repeat-one-of",
          "iface-[any]"
        ]
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestRepeatInference8 () =
        let t1 = Tree.FromJson """
        {
            "RRH_CMPR_BIT_LENGTH": [
                "9",
                "16"
            ]
        } 
        """
        let t2 = Tree.FromJson """
        {
            "RRH_CMPR_BIT_LENGTH": [
                "9",
                "9"
            ]
        }
        """
        let t3 = Tree.FromJson """
        {
            "RRH_CMPR_BIT_LENGTH": [
                "9",
                "16"
            ],
            "RRH_UL_INIT_SYM_ID": "12"
        }
        """
        let tokens = [|
            Token("digit", "^[0-9]$", 0.2)
        |]
        let t = Template.InferWithOptions([| t1; t2; t3 |], {Template.DefaultEnvironment() with Tokens = tokens; GroupingThreshold = 0.2} )
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        printfn "%A" (Template.ToJson t jsonConfig)
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        {
          "RRH_CMPR_BIT_LENGTH": [
            "@type:repeat-one-of",
            "9",
            {
              "CASES": [
                "C1",
                "C3"
              ],
              "TEMPLATE": "16"
            }
          ],
          "RRH_UL_INIT_SYM_ID": {
            "CASES": [
              "C3"
            ],
            "TEMPLATE": "12"
          }
        }
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic records.
    [<TestMethod>]
    member this.TestRecordInference1 () =
        let t1 = Tree.FromJson """
        {
            "key1": ["iface-1", "bar"],
            "key2": ["foo"],
        }
        """
        let t2 = Tree.FromJson """
        {
            "key1": ["iface-2", "baz"],
            "key2": ["foo"],
        }
        """
        let t = Template.Infer [| t1; t2 |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        {
          "key1": [
            "@type:ordered",
            "iface-[any]",
            "ba[any]"
          ],
          "key2": [
            "@type:ordered",
            "foo"
          ]
        }
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic records.
    [<TestMethod>]
    member this.TestRecordInference2 () =
        let t1 = Tree.FromJson """
        {
            "key1": ["iface-1", "bar"],
            "key2": ["foo"],
        }
        """
        let t2 = Tree.FromJson """
        {
            "key1": ["bar", "iface-2"],
            "key2": ["foo"],
        }
        """
        let t3 = Tree.FromJson """
        {
            "key1": ["bar", "iface-3"],
            "key2": ["foo"],
            "key3": ["extra"]
        }
        """
        let t = Template.Infer [| t1; t2; t3 |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        {
          "key1": [
            "@type:unordered",
            "iface-[any]",
            "bar"
          ],
          "key2": [
            "@type:ordered",
            "foo"
          ],
          "key3": {
            "CASES": [
                "C3"
            ],
            "TEMPLATE": [
                "@type:ordered",
                "extra"
            ]
          }
        }
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic records.
    [<TestMethod>]
    member this.TestRecordInference3 () =
        let t1 = Tree.FromJson """{ "key1": ["iface-1", "bar"] }"""
        let t2 = Tree.FromJson """{ "key1": ["iface-2", "bar"] }"""
        let t3 = Tree.FromJson """{ "key1": ["iface-3", "bar"] }"""
        let t = Template.Infer [| t1; t2; t3 |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        {
          "key1": [
            "@type:ordered",
            "iface-[any]",
            "bar"
          ]
        }
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic records.
    [<TestMethod>]
    member this.TestRecordInference4 () =
        let t1 = Tree.FromJson """{ "key1": {"foo": 1, "bar": 2}, "key2": 10 }"""
        let t2 = Tree.FromJson """{ "key1": {"foo": 1, "bar": 2}, "key2": 10 }"""
        let t3 = Tree.FromJson """{ "other": "hello", "key2": 10 }"""
        let t = Template.Infer [| t1; t2; t3 |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        {
          "key1": {
            "CASES": [
                "C1",
                "C2"
            ],
            "TEMPLATE": {
                "foo": "1",
                "bar": "2"
            }
          },
          "key2": "10",
          "other": {
            "CASES": [
                "C3"
            ],
            "TEMPLATE": "hello"
          }
        }
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic records.
    [<TestMethod>]
    member this.TestRecordInference5 () =
        let t1 = Tree.FromJson """
        {
            "10.0.1.45": "iface11",
            "10.0.1.49": "iface12",
            "10.0.1.53": "iface13"
        }"""
        let t2 = Tree.FromJson """
        {
            "10.0.1.47": "iface11",
            "10.0.1.51": "iface12",
            "10.0.1.55": "iface13"
        }"""
        let t = Template.InferWithOptions([| t1; t2 |], {Template.DefaultEnvironment() with ExactKeyMatching = false})
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        {
          "10.0.1.4[any]": "iface11",
          "10.0.1.[any]": "iface12",
          "10.0.1.5[any]": "iface13",
        }
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic records.
    [<TestMethod>]
    member this.TestRecordInference6 () =
        let t1 = Tree.FromJson """{ "admin": true }"""
        let t2 = Tree.FromJson """{ "admin": false }"""
        let t = Template.Infer [| t1; t2 |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        {
          "admin": [
            {
                "CASES": [
                    "C1",
                ],
                "TEMPLATE": "true"
            },
            {
                "CASES": [
                    "C2",
                ],
                "TEMPLATE": "false"
            }
          ]
        }
        """
        Assert.IsTrue ((actual = expected))
    
    /// Test that the template inference is working for basic records.
    [<TestMethod>]
    member this.TestRecordInference7 () =
        let t1 = Tree.FromJson """{
            "key1": {
                "class": "value"
            }
        }"""
        let t2 = Tree.FromJson """{
            "key2": {
              "class": "value"
            },
            "key1": {
              "class": "value"
            }
        }"""
        let t = Template.Infer [| t1; t2 |]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        {
          "key1": {
            "class": "value"
          },
          "key2": {
            "CASES": [
                "C2"
            ],
            "TEMPLATE": {
                "class": "value"
            }
          }
        }
        """
        Assert.IsTrue ((actual = expected))
    
    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestNullInference1 () =
        let t1 = Tree.FromJson """null"""
        let t2 = Tree.FromJson """null"""
        let t = Template.Infer [|t1; t2|]
        Assert.IsTrue((t = Template.TNull))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestNullInference2 () =
        let t1 = Tree.FromJson """{"key": null}"""
        let t2 = Tree.FromJson """{"key": null}"""
        let t = Template.Infer [|t1; t2|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """{ "key": null }"""
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestNullInference3 () =
        let t1 = Tree.FromJson """["hello", "there", null]"""
        let t2 = Tree.FromJson """[null, "there", "hello"]"""
        let t = Template.Infer [|t1; t2|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
            "@type:unordered",
            "hello",
            "there",
            null
        ]
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestTypeMismatch1 () =
        let t1 = Tree.FromJson """[]"""
        let t2 = Tree.FromJson """{}"""
        let t = Template.Infer [|t1; t2|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        [
            {
                "CASES": ["C1"],
                "TEMPLATE": []
            },
            {
                "CASES": ["C2"],
                "TEMPLATE": {}
            },
        ]
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestTypeMismatch2 () =
        let t1 = Tree.FromJson """{"key": "hello"}"""
        let t2 = Tree.FromJson """{"key": null}"""
        let t3 = Tree.FromJson """{"key": []}"""
        let t4 = Tree.FromJson """{"key": {}}"""
        let t = Template.Infer [|t1; t2; t3; t4|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3"; "C4" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        {
          "key": [
            {
              "CASES": [
                "C2"
              ],
              "TEMPLATE": null
            },
            {
              "CASES": [
                "C1"
              ],
              "TEMPLATE": "hello"
            },
            {
              "CASES": [
                "C3"
              ],
              "TEMPLATE": []
            },
            {
              "CASES": [
                "C4"
              ],
              "TEMPLATE": {}
            }
          ]
        }
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic sets.
    [<TestMethod>]
    member this.TestWanInference1 () =
        let t1 = Tree.FromJson """
        {
          "xe-0/0/0:2.0": {
            "name": "xe-0/0/0:2.0",
            "adminUp": true,
            "allPrefixes": [
                "10.0.1.1/30"
            ]
          }
        }"""
        let t2 = Tree.FromJson """
        {
          "xe-0/0/0:2.0": {
            "name": "xe-0/0/0:2.0",
            "adminUp": true,
            "allPrefixes": [
                "10.0.1.9/30"
            ]
          },
          "xe-1/0/0:2.0": {
            "name": "xe-1/0/0:2.0",
            "adminUp": true,
            "allPrefixes": [
                "10.0.1.5/30"
            ]
          },
        }"""
        let t = Template.Infer [|t1; t2|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2" |])
        let actual = Template.ToJson t jsonConfig |> Tree.FromJson
        let expected = Tree.FromJson """
        {
          "xe-0/0/0:2.0": {
            "name": "xe-0/0/0:2.0",
            "adminUp": "true",
            "allPrefixes": [
              "@type:ordered",
              "10.0.1.[any]/30"
            ]
          },
          "xe-1/0/0:2.0": {
            "CASES": [
              "C2"
            ],
            "TEMPLATE": {
              "name": "xe-1/0/0:2.0",
              "adminUp": "true",
              "allPrefixes": [
                "@type:ordered",
                "10.0.1.5/30"
              ]
            }
          }
        }
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference is working for basic strings with cases
    [<TestMethod>]
    member this.TestCacheDifferentIndices() =
        let t1 = Tree.FromJson """
        {
            "snmpSourceInterface": "Management1",
            "tacacsSourceInterface": "Management1",
        }
        """
        let t2 = Tree.FromJson """
        {
            "snmpSourceInterface": "Loopback0",
            "tacacsSourceInterface": "Management1",
        }
        """
        let t3 = Tree.FromJson """
        {
            "snmpSourceInterface": null,
            "tacacsSourceInterface": "Loopback0",
        }
        """
        let env = Template.DefaultEnvironment()
        let t = Template.InferWithOptions([|t1; t2; t3|], env)
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3" |])
        let json = Template.ToJson t jsonConfig
        let actual = json |> Tree.FromJson
        let expected = Tree.FromJson """
        {
          "snmpSourceInterface": [
            {
              "CASES": [
                "C3"
              ],
              "TEMPLATE": null
            },
            {
              "CASES": [
                "C1"
              ],
              "TEMPLATE": "Management1"
            },
            {
              "CASES": [
                "C2"
              ],
              "TEMPLATE": "Loopback0"
            }
          ],
          "tacacsSourceInterface": [
            {
              "CASES": [
                "C1",
                "C2"
              ],
              "TEMPLATE": "Management1"
            },
            {
              "CASES": [
                "C3"
              ],
              "TEMPLATE": "Loopback0"
            }
          ]
        }
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference deduplication is working.
    [<TestMethod>]
    member this.TestDeduplication1() =
        let t1 = Tree.FromJson """["A", "B"]"""
        let t2 = Tree.FromJson """["A", "B"]"""
        let t3 = Tree.FromJson """["A", "C"]"""
        let t4 = Tree.FromJson """["A", "C"]"""
        let t = Template.Infer [|t1; t2; t3; t4|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3"; "C4" |])
        let json = Template.ToJson t jsonConfig
        let actual = Tree.FromJson json
        let expected = Tree.FromJson """
        [
          "@type:ordered",
          "A",
          {
            "CASES": [
              "C1",
              "C2"
            ],
            "TEMPLATE": "B"
          },
          {
            "CASES": [
              "C3",
              "C4"
            ],
            "TEMPLATE": "C"
          }
        ]
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference deduplication is working.
    [<TestMethod>]
    member this.TestDeduplication2() =
        let t1 = Tree.FromJson """["A", "Cat1"]"""
        let t2 = Tree.FromJson """["A", "Cat1"]"""
        let t3 = Tree.FromJson """["A", "Cat2"]"""
        let t4 = Tree.FromJson """["A", "Cat2"]"""
        let t = Template.Infer [|t1; t2; t3; t4|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3"; "C4" |])
        let json = Template.ToJson t jsonConfig
        let actual = Tree.FromJson json
        let expected = Tree.FromJson """
        [
            "@type:ordered",
            "A",
            "Cat[any]"
        ]
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference deduplication is working.
    [<TestMethod>]
    member this.TestDeduplication3() =
        let t1 = Tree.FromJson """{"key1": "A"}"""
        let t2 = Tree.FromJson """{"key1": "A"}"""
        let t3 = Tree.FromJson """{"key1": "B"}"""
        let t4 = Tree.FromJson """{"key1": "B"}"""
        let t = Template.Infer [|t1; t2; t3; t4|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3"; "C4" |])
        let json = Template.ToJson t jsonConfig
        let actual = Tree.FromJson json
        let expected = Tree.FromJson """
        {
          "key1": [
            {
              "CASES": [
                "C1",
                "C2"
              ],
              "TEMPLATE": "A"
            },
            {
              "CASES": [
                "C3",
                "C4"
              ],
              "TEMPLATE": "B"
            }
          ]
        }
        """
        Assert.IsTrue ((actual = expected))

    /// Test that the template inference deduplication is working.
    [<TestMethod>]
    member this.TestDeduplication4() =
        let t1 = Tree.FromJson """{"key1": "A", "key2": "B"}"""
        let t2 = Tree.FromJson """{"key1": "A", "key2": "B"}"""
        let t3 = Tree.FromJson """{"key1": "C", "key2": "B"}"""
        let t4 = Tree.FromJson """{"key1": "C", "key2": "B"}"""
        let t5 = Tree.FromJson """{"key1": "D", "key2": "E"}"""
        let t6 = Tree.FromJson """{"key1": "D", "key2": "E"}"""
        let t = Template.Infer [|t1; t2; t3; t4; t5; t6|]
        let jsonConfig = Template.JsonConfig([| "C1"; "C2"; "C3"; "C4"; "C5"; "C6" |])
        let json = Template.ToJson t jsonConfig
        let actual = Tree.FromJson json
        let expected = Tree.FromJson """
        {
          "key1": [
            {
              "CASES": [
                "C1",
                "C2"
              ],
              "TEMPLATE": "A"
            },
            {
              "CASES": [
                "C3",
                "C4"
              ],
              "TEMPLATE": "C"
            },
            {
              "CASES": [
                "C5",
                "C6"
              ],
              "TEMPLATE": "D"
            }
          ],
          "key2": [
            {
              "CASES": [
                "C1",
                "C2",
                "C3",
                "C4"
              ],
              "TEMPLATE": "B"
            },
            {
              "CASES": [
                "C5",
                "C6"
              ],
              "TEMPLATE": "E"
            }
          ]
        }
        """
        Assert.IsTrue ((actual = expected))


/// Test class data output formtting of results.
[<TestClass>]
[<ExcludeFromCodeCoverage>]
type FormattingTests () =

    /// Test that the json formatter is working as expected.
    [<TestMethod>]
    member this.TestToJson1 () =
        let t1 = Tree.FromJson """
        {
            "key1": ["iface-1", "baz"],
            "key2": ["foo"],
        }
        """
        let t2 = Tree.FromJson """
        {
            "key1": ["baz", "iface-2"],
            "key2": ["foo"],
        }
        """
        let t3 = Tree.FromJson """
        {
            "key1": ["baz", "iface-3"],
            "key2": ["foo"],
            "key3": ["extra"]
        }
        """
        let t = Template.Infer [| t1; t2; t3 |]
        let jsonConfig = Template.JsonConfig([| "A"; "B"; "C" |])
        let json = Template.ToJson t jsonConfig
        let actual = Tree.FromJson json
        let expected = Tree.FromJson """
        {
            "key1": [
                "@type:unordered",
                "iface-[any]",
                "baz"
            ],
            "key2": [
                "@type:ordered",
                "foo"
            ],
            "key3": {
                "CASES": [
                    "C"
                ],
                "TEMPLATE": [
                    "@type:ordered",
                    "extra"
                ]
            }
        }
        """
        Assert.IsTrue((actual = expected))

    /// Test that the json formatter is working as expected.
    [<TestMethod>]
    member this.TestToJson2 () =
        let t1 = Tree.FromJson """[["A"], ["B", "C"]]"""
        let t2 = Tree.FromJson """[["C", "B"], ["A"]]"""
        let t = Template.Infer [| t1; t2 |]
        let jsonConfig = Template.JsonConfig([| "X"; "Y" |])
        let json = Template.ToJson t jsonConfig
        let actual = Tree.FromJson json
        let expected = Tree.FromJson """
        [
          "@type:unordered",
          [
            "@type:ordered",
            "A"
          ],
          [
            "@type:unordered",
            "B",
            "C"
          ]
        ]
        """
        Assert.IsTrue((actual = expected))

    /// Test that the json formatter is working as expected.
    [<TestMethod>]
    member this.TestToJson3 () =
        let t1 = Tree.FromJson """{ "key1": {"foo": 1, "bar": 2}, "key2": 10 }"""
        let t2 = Tree.FromJson """{ "key1": {"foo": 1, "bar": 2}, "key2": 10 }"""
        let t3 = Tree.FromJson """{ "other": "hello", "key2": 10 }"""
        let t = Template.Infer [| t1; t2; t3 |]
        let jsonConfig = Template.JsonConfig([| "X"; "Y"; "Z" |])
        let json = Template.ToJson t jsonConfig
        let actual = Tree.FromJson json
        let expected = Tree.FromJson """
        {
          "key1": {
            "CASES": [
              "X",
              "Y"
            ],
            "TEMPLATE": {
              "foo": "1",
              "bar": "2"
            }
          },
          "key2": "10",
          "other": {
            "CASES": [
              "Z"
            ],
            "TEMPLATE": "hello"
          }
        }
        """
        Assert.IsTrue((actual = expected))

    /// Test that the json formatter is working as expected.
    [<TestMethod>]
    member this.TestToJson4 () =
        let t1 = Tree.FromJson """{"key": null}"""
        let t2 = Tree.FromJson """{"key": "hello"}"""
        let t3 = Tree.FromJson """{"key": []}"""
        let t4 = Tree.FromJson """{"key": {}}"""
        let t = Template.Infer [| t1; t2; t3; t4 |]
        let jsonConfig = Template.JsonConfig([| "A"; "B"; "C"; "D" |])
        let json = Template.ToJson t jsonConfig
        let actual = Tree.FromJson json
        let expected = Tree.FromJson """
        {
          "key": [
            {
              "CASES": [
                "A"
              ],
              "TEMPLATE": null
            },
            {
              "CASES": [
                "B"
              ],
              "TEMPLATE": "hello"
            },
            {
              "CASES": [
                "C"
              ],
              "TEMPLATE": []
            },
            {
              "CASES": [
                "D"
              ],
              "TEMPLATE": {}
            }
          ]
        }
        """
        Assert.IsTrue((actual = expected))

    /// Test that the json formatter is working as expected.
    [<TestMethod>]
    member this.TestToJson5 () =
        let t1 = Tree.FromJson """["A", "A", "B", "B"]"""
        let t2 = Tree.FromJson """["B", "A"]"""
        let t = Template.Infer [| t1; t2 |]
        let jsonConfig = Template.JsonConfig([| "X"; "Y" |])
        let json = Template.ToJson t jsonConfig
        let actual = Tree.FromJson json
        let expected = Tree.FromJson """
        [
          "@type:repeat-one-of",
          "A",
          "B"
        ]
        """
        Assert.IsTrue((actual = expected))

/// Test class for helper algorithms for strings.
[<TestClass>]
[<ExcludeFromCodeCoverage>]
type StringsTests () =

    /// Test that edit distance is working as expected.
    [<TestMethod>]
    member this.TestEditDistance () =
        Assert.AreEqual(1.0, Strings.EditDistance [|"a"; "b"; "c"|] [|"a"; "b"|])
        Assert.AreEqual(1.0, Strings.EditDistance [|"a"; "b"; "c"|] [|"b"; "c"|])
        Assert.AreEqual(3.0, Strings.EditDistance [|"a"; "b"; "c"|] [||])
        Assert.AreEqual(3.0, Strings.EditDistance [||] [|"a"; "b"; "c"|])
        Assert.AreEqual(2.0, Strings.EditDistance [|"a"; "b"; "c"|] [|"b"|])
        Assert.AreEqual(2.0, Strings.EditDistance [|"a"; "b"; "c"; "d"; "e"|] [|"a"; "c"; "d"|])
        Assert.AreEqual(0.0, Strings.EditDistance [|"a"; "b"; "c"|] [|"a"; "b"; "c"|])

    /// Test that sequence alignment is working as expected.
    [<TestMethod>]
    member this.TestSequenceAlignment () =
        Assert.IsTrue(([|null; null|], [|"a"; "b"|]) = (Strings.SequenceAlign [||] [|"a"; "b"|] null))
        Assert.IsTrue(([|"a"; "b"|], [|null; null|]) = (Strings.SequenceAlign [|"a"; "b"|] [||] null))
        Assert.IsTrue(([|"a"; "b"; "c"|], [|"a"; "b"; "c"|]) = (Strings.SequenceAlign [|"a"; "b"; "c"|] [|"a"; "b"; "c"|] null))
        Assert.IsTrue(([|"a"; "b"; "c"|], [|"a"; "b"; null|]) = (Strings.SequenceAlign [|"a"; "b"; "c"|] [|"a"; "b"|] null))
        Assert.IsTrue(([|"a"; "b"; "c"|], [|null; "b"; "c"|]) = (Strings.SequenceAlign [|"a"; "b"; "c"|] [|"b"; "c"|] null))
        Assert.IsTrue(([|"a"; "b"; "c"|], [|"a"; null; "c"|]) = (Strings.SequenceAlign [|"a"; "b"; "c"|] [|"a"; "c"|] null))
        Assert.IsTrue(([|"a"; "b"; "c"; null|], [|null; "b"; "c"; "d"|]) = (Strings.SequenceAlign [|"a"; "b"; "c"|] [|"b"; "c"; "d"|] null))
        Assert.IsTrue(([|null; "a"; "b"; "c"; "d"; null|], [|"x"; "a"; null; null; "d"; "e"|]) = (Strings.SequenceAlign [|"a"; "b"; "c"; "d"|] [|"x"; "a"; "d"; "e"|] null))
        Assert.IsTrue(([|null; "a"; null|], [|null; "a"; "b"|]) = (Strings.SequenceAlign [|null; "a"|] [|"a"; "b"|] null))

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignment1 () =
        let t1 = Token("number", "^[0-9]+$", 0.2)
        let tokens = [| t1 |]
        let p1 = pattern [| PChar 'h'; PChar 'i'; PChar '1'; PChar '2' |] 1
        let p2 = pattern [| PChar 'h'; PChar 'i'; PChar '2' |] 2
        let result = Strings.PatternAlign tokens p1 p2

        Assert.IsTrue((result.Pattern = [| PChar 'h'; PChar 'i'; PToken t1 |]))
        Assert.IsTrue(abs (result.Cost - 0.6) < 0.001)
        Assert.IsTrue(Option.isNone result.Parameters[0])
        Assert.IsTrue(Option.isNone result.Parameters[1])
        Assert.IsTrue(Option.isSome result.Parameters[2])
        Assert.IsTrue((Option.get result.Parameters[2]).TotalLength = 3)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[0].Value = "12")
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[1].Value = "2")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignment2 () =
        let t1 = Token("number", "^[0-9]+$", 0.2)
        let tokens = [| t1 |]
        let p1 = pattern [| PChar 'h'; PChar 'i'; PChar 'l'; PChar 'l' |] 1
        let p2 = pattern [| PChar 'o'; PChar 'h'; PChar 'i'; |] 2
        let result = Strings.PatternAlign tokens p1 p2

        Assert.IsTrue((result.Pattern = [| PAny; PChar 'h'; PChar 'i'; PAny |]))
        Assert.IsTrue(abs (result.Cost - 3.0) < 0.001)
        Assert.IsTrue(Option.isSome result.Parameters[0])
        Assert.IsTrue(Option.isNone result.Parameters[1])
        Assert.IsTrue(Option.isNone result.Parameters[2])
        Assert.IsTrue(Option.isSome result.Parameters[3])
        Assert.IsTrue((Option.get result.Parameters[0]).TotalLength = 1)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Value = "")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Value = "o")
        Assert.IsTrue((Option.get result.Parameters[3]).TotalLength = 2)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[0].Value = "ll")
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[1].Value = "")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignment3 () =
        let t1 = Token("number", "^[0-9]+$", 0.2)
        let tokens = [| t1 |]
        let p1 = pattern [| PChar 'h'; PChar 'i'; PChar '1'; PChar 'o' |] 1
        let p2 = pattern [| PChar 'h'; PChar 'i'; PChar '3'; PChar '4'; PChar 'x' |] 2
        let result = Strings.PatternAlign tokens p1 p2

        Assert.IsTrue((result.Pattern = [| PChar 'h'; PChar 'i'; PToken t1; PAny |]))
        Assert.IsTrue(abs (result.Cost - 2.6) < 0.001)
        Assert.IsTrue(Option.isNone result.Parameters[0])
        Assert.IsTrue(Option.isNone result.Parameters[1])
        Assert.IsTrue(Option.isSome result.Parameters[2])
        Assert.IsTrue(Option.isSome result.Parameters[3])
        Assert.IsTrue((Option.get result.Parameters[2]).TotalLength = 3)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[0].Value = "1")
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[1].Value = "34")
        Assert.IsTrue((Option.get result.Parameters[3]).TotalLength = 2)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[0].Value = "o")
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[1].Value = "x")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignment4 () =
        let t1 = Token("number", "^[0-9]+$", 0.2)
        let tokens = [| t1 |]
        let p1 = pattern [| PChar '0'; PChar '1'; PChar '2'; PChar '3' |] 1
        let p2 = pattern [| PChar '0'; PChar '2'; PChar '4'; PChar '8'; PChar '7' |] 2
        let result = Strings.PatternAlign tokens p1 p2

        Assert.IsTrue((result.Pattern = [| PChar '0'; PToken t1; |]))
        Assert.IsTrue(abs (result.Cost - 1.4) < 0.001)
        Assert.IsTrue(Option.isNone result.Parameters[0])
        Assert.IsTrue(Option.isSome result.Parameters[1])
        Assert.IsTrue((Option.get result.Parameters[1]).TotalLength = 7)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[0].Value = "123")
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[1].Value = "2487")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignment5 () =
        let t1 = Token("number?", "^[0-9]*$", 0.2)
        let tokens = [| t1 |]
        let p1 = pattern [| PChar 'a'; PChar '1'; PChar '2'; PChar 'b' |] 1
        let p2 = pattern [| PChar 'a'; PChar 'b' |] 2
        let result = Strings.PatternAlign tokens p1 p2

        Assert.IsTrue((result.Pattern = [| PChar 'a'; PToken t1; PChar 'b' |]))
        Assert.IsTrue(abs (result.Cost - 0.4) < 0.001)
        Assert.IsTrue(Option.isNone result.Parameters[0])
        Assert.IsTrue(Option.isSome result.Parameters[1])
        Assert.IsTrue(Option.isNone result.Parameters[2])
        Assert.IsTrue((Option.get result.Parameters[1]).TotalLength = 2)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[0].Value = "12")
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[1].Value = "")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignment6 () =
        let t1 = Token("digit", "^[0-9]$", 0.2)
        let tokens = [| t1 |]
        let p1 = pattern [| PChar 'a'; PChar '1'; PChar '2'; PChar 'b' |] 1
        let p2 = pattern [| PChar 'a'; PChar '3' |] 2
        let result = Strings.PatternAlign tokens p1 p2

        Assert.IsTrue((result.Pattern = [| PChar 'a'; PToken t1; PAny |]))
        Assert.IsTrue(abs (result.Cost - 2.4) < 0.001)
        Assert.IsTrue(Option.isNone result.Parameters[0])
        Assert.IsTrue(Option.isSome result.Parameters[1])
        Assert.IsTrue(Option.isSome result.Parameters[2])
        Assert.IsTrue((Option.get result.Parameters[1]).TotalLength = 2)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[0].Value = "1")
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[1].Value = "3")
        Assert.IsTrue((Option.get result.Parameters[2]).TotalLength = 2)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[0].Value = "2b")
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[1].Value = "")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignment7 () =
        let t1 = Token("digit", "^[0-9]$", 0.4)
        let t2 = Token("ip", @"^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}$", 0.1)
        let tokens = [| t1; t2 |]
        let p1 = pattern [| PChar '1'; PChar '.'; PChar '2'; PChar '.'; PChar '3'; PChar '.'; PChar '4' |] 1
        let p2 = pattern [| PChar '5'; PChar '.'; PChar '6'; PChar '.'; PChar '7'; PChar '.'; PChar '8' |] 2
        let result = Strings.PatternAlign tokens p1 p2

        Assert.IsTrue((result.Pattern = [| PToken t2 |]))
        Assert.IsTrue(abs (result.Cost - 1.4) < 0.001)
        Assert.IsTrue((Option.get result.Parameters[0]).TotalLength = 14)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Value = "1.2.3.4")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Value = "5.6.7.8")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignment8 () =
        let t1 = Token("number", "^[0-9]+$", 0.2)
        let tokens = [| t1 |]
        let p1 = pattern [| PChar 'a'; PChar 'b'; PChar '1' |] 1
        let p2 = pattern [| PChar 'a'; PChar 'b'; PChar '2' |] 2
        let p3 = pattern [| PChar 'a'; PChar 'b'; PChar '1'; PChar '2'; PChar '3'; PChar 'c' |] 3
        let result = Strings.PatternAlign tokens p1 p2
        let result = Strings.PatternAlign tokens result p3

        Assert.IsTrue((result.Pattern = [| PChar 'a'; PChar 'b'; PToken t1; PAny |]))
        Assert.IsTrue(abs (result.Cost - 2.0) < 0.001)
        Assert.IsTrue(Option.isNone result.Parameters[0])
        Assert.IsTrue(Option.isNone result.Parameters[1])
        Assert.IsTrue(Option.isSome result.Parameters[2])
        Assert.IsTrue(Option.isSome result.Parameters[3])
        Assert.IsTrue((Option.get result.Parameters[2]).TotalLength = 5)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[0].Value = "1")
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[1].Value = "2")
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[2].Config = 3)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[2].Value = "123")
        Assert.IsTrue((Option.get result.Parameters[3]).TotalLength = 1)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[0].Value = "")
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[1].Value = "")
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[2].Config = 3)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[2].Value = "c")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignment9 () =
        let t1 = Token("number", "^[0-9]+$", 0.2)
        let tokens = [| t1 |]
        let p1 = pattern [| PChar 'm'; PChar 'n'; PChar 'a'; PChar 'b'; PChar '1' |] 1
        let p2 = pattern [| PChar 'w'; PChar 'x'; PChar 'a'; PChar 'b'; PChar '2' |] 2
        let p3 = pattern [| PChar 'y'; PChar 'z'; PChar 'a'; PChar 'b'; PChar 'c' |] 3
        let result = Strings.PatternAlign tokens p1 p2
        let result = Strings.PatternAlign tokens result p3

        Assert.IsTrue((result.Pattern = [| PAny; PChar 'a'; PChar 'b'; PAny |]))
        Assert.IsTrue(abs (result.Cost - 9.0) < 0.001)
        Assert.IsTrue(Option.isSome result.Parameters[0])
        Assert.IsTrue(Option.isNone result.Parameters[1])
        Assert.IsTrue(Option.isNone result.Parameters[2])
        Assert.IsTrue(Option.isSome result.Parameters[3])
        Assert.IsTrue((Option.get result.Parameters[0]).TotalLength = 6)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Value = "mn")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Value = "wx")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[2].Config = 3)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[2].Value = "yz")
        Assert.IsTrue((Option.get result.Parameters[3]).TotalLength = 3)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[0].Value = "1")
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[1].Value = "2")
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[2].Config = 3)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[2].Value = "c")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignment10 () =
        let t1 = Token("number", "^[0-9]+$", 0.2)
        let t2 = Token("name", "^[a-zA-Z]+$", 0.2)
        let tokens = [| t1; t2 |]
        let p1 = pattern [| PChar 'R'; PChar 'y'; PChar 'a'; PChar 'n'; PChar '1' |] 1
        let p2 = pattern [| PChar 'S'; PChar 'i'; PChar 'v'; PChar 'a'; PChar '2'; PChar '3' |] 2
        let result = Strings.PatternAlign tokens p1 p2

        Assert.IsTrue((result.Pattern = [| PToken t2; PToken t1 |]))
        Assert.IsTrue(abs (result.Cost - 2.2) < 0.001)
        Assert.IsTrue(Option.isSome result.Parameters[0])
        Assert.IsTrue(Option.isSome result.Parameters[1])
        Assert.IsTrue((Option.get result.Parameters[0]).TotalLength = 8)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Value = "Ryan")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Value = "Siva")
        Assert.IsTrue((Option.get result.Parameters[1]).TotalLength = 3)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[0].Value = "1")
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[1].Value = "23")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignment11 () =
        let t = Token("number", "^[0-9]+$", 0.2)
        let p1 = pattern [| PChar 'a'; PChar '1'; PChar 'b'; PChar '2' |] 1
        let p2 = pattern [| PChar '3'; PChar 'b'; PChar '4'; PChar 'd' |] 2
        let result = Strings.PatternAlign [| t |] p1 p2

        Assert.IsTrue((result.Pattern = [| PAny; PToken t; PChar 'b'; PToken t; PAny |]))
        Assert.IsTrue(abs (result.Cost - 2.8) < 0.001)
        Assert.IsTrue(Option.isSome result.Parameters[0])
        Assert.IsTrue(Option.isSome result.Parameters[1])
        Assert.IsTrue(Option.isNone result.Parameters[2])
        Assert.IsTrue(Option.isSome result.Parameters[3])
        Assert.IsTrue(Option.isSome result.Parameters[4])
        Assert.IsTrue((Option.get result.Parameters[0]).TotalLength = 1)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Value = "a")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Value = "")
        Assert.IsTrue((Option.get result.Parameters[1]).TotalLength = 2)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[0].Value = "1")
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[1].Value = "3")
        Assert.IsTrue((Option.get result.Parameters[3]).TotalLength = 2)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[0].Value = "2")
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[3]).Parameters[1].Value = "4")
        Assert.IsTrue((Option.get result.Parameters[4]).TotalLength = 1)
        Assert.IsTrue((Option.get result.Parameters[4]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[4]).Parameters[0].Value = "")
        Assert.IsTrue((Option.get result.Parameters[4]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[4]).Parameters[1].Value = "d")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignment12 () =
        let t = Token("number", "^[0-9]+$", 0.2)
        let p1 = pattern [| PChar '1'; PChar '0'; PChar '0'; PChar '0' |] 1
        let p2 = pattern [| PChar '3'; PChar '3'; PChar '0'; PChar '6' |] 2
        let p3 = pattern [| PChar '3'; PChar '3'; PChar '1'; PChar '0' |] 3
        let result = Strings.PatternAlign [| t |] p1 p2
        let result = Strings.PatternAlign [| t |] result p3

        Assert.IsTrue((result.Pattern = [| PToken t |]))
        Assert.IsTrue(abs (result.Cost - 2.4) < 0.001)
        Assert.IsTrue(Option.isSome result.Parameters[0])
        Assert.IsTrue((Option.get result.Parameters[0]).TotalLength = 12)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Value = "1000")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Value = "3306")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[2].Config = 3)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[2].Value = "3310")

    [<TestMethod>]
    member this.TestPatternAlignment13 () =
        let t = Token("number", "^[0-9]+$", 0.25)
        let p1 = pattern [| PChar '4'; PChar '8'; PChar 'G' |] 1
        let p2 = pattern [| PChar '5'; PChar 'G'; PChar 'B' |] 2
        let p3 = pattern [| PChar '1'; PChar '0' |] 3

        let result = Strings.PatternAlign [| t |] p1 p2
        Assert.IsTrue((result.Pattern = [| PToken t; PChar 'G'; PAny |]))
        Assert.IsTrue(Option.isSome result.Parameters[0])
        Assert.IsTrue(Option.isNone result.Parameters[1])
        Assert.IsTrue(Option.isSome result.Parameters[2])
        Assert.IsTrue((Option.get result.Parameters[0]).TotalLength = 3)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Value = "48")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Value = "5")
        Assert.IsTrue((Option.get result.Parameters[2]).TotalLength = 1)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[0].Value = "")
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[2]).Parameters[1].Value = "B")

        let result = Strings.PatternAlign [| t |] result p3
        Assert.IsTrue((result.Pattern = [| PToken t; PAny |]))
        Assert.IsTrue(Option.isSome result.Parameters[0])
        Assert.IsTrue(Option.isSome result.Parameters[1])
        Assert.IsTrue((Option.get result.Parameters[0]).TotalLength = 5)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Value = "48")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Value = "5")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[2].Config = 3)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[2].Value = "10")
        Assert.IsTrue((Option.get result.Parameters[1]).TotalLength = 3)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[0].Value = "G")
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[1].Value = "GB")
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[2].Config = 3)
        Assert.IsTrue((Option.get result.Parameters[1]).Parameters[2].Value = "")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignmentEmpty1 () =
        let p1 = pattern [| |] 1
        let p2 = pattern [| PChar 'S'; PChar 'i'; PChar 'v'; PChar 'a'; |] 2
        let result = Strings.PatternAlign [||] p1 p2

        Assert.IsTrue((result.Pattern = [| PAny |]))
        Assert.IsTrue(Option.isSome result.Parameters[0])
        Assert.IsTrue((Option.get result.Parameters[0]).TotalLength = 4)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Value = "")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Value = "Siva")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignmentEmpty2 () =
        let p1 = pattern [| PChar 'S'; PChar 'i'; PChar 'v'; PChar 'a'; |] 1
        let p2 = pattern [| |] 2
        let result = Strings.PatternAlign [||] p1 p2

        Assert.IsTrue((result.Pattern = [| PAny |]))
        Assert.IsTrue(Option.isSome result.Parameters[0])
        Assert.IsTrue((Option.get result.Parameters[0]).TotalLength = 4)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Config = 1)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[0].Value = "Siva")
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Config = 2)
        Assert.IsTrue((Option.get result.Parameters[0]).Parameters[1].Value = "")

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestPatternAlignmentEmpty3 () =
        let p1 = pattern [| |] 1
        let p2 = pattern [| |] 2
        let result = Strings.PatternAlign [||] p1 p2

        Assert.IsTrue((result.Pattern = [| |]))
        Assert.IsTrue(result.Parameters.Length = 0)

    /// Test that pattern alignment is working as expected.
    [<TestMethod>]
    member this.TestFindStartingIndices () =
        let tnum = Token("number", "^[0-9]+$", 0.2, greedy=false)
        let tlower = Token("lower", "^[a-z]+$", 0.2, greedy=false)
        let talpha = Token("alpha", "^[a-zA-Z]+$", 0.2, greedy=false)

        let indices = Strings.FindMatchingStartIndices tnum [| PChar '2'; PToken tnum; PChar '3' |]
        Assert.IsTrue(indices[0].SetEquals(HashSet<int>([0])))
        Assert.IsTrue(indices[1].SetEquals(HashSet<int>([0; 1])))
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([0; 1; 2])))

        let indices = Strings.FindMatchingStartIndices tnum [| PChar 'a'; PToken tnum; PChar '3' |]
        Assert.IsTrue(indices[0].Count = 0)
        Assert.IsTrue(indices[1].SetEquals(HashSet<int>([1])))
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([1; 2])))

        let indices = Strings.FindMatchingStartIndices tnum [| PChar 'a'; PToken tnum; PToken tnum; PChar 'b' |]
        Assert.IsTrue(indices[0].Count = 0)
        Assert.IsTrue(indices[1].SetEquals(HashSet<int>([1])))
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([1; 2])))
        Assert.IsTrue(indices[3].Count = 0)

        let indices = Strings.FindMatchingStartIndices tnum [| PChar '1'; PToken tnum; PChar 'b'; PToken tnum; PChar '4' |]
        Assert.IsTrue(indices[0].SetEquals(HashSet<int>([0])))
        Assert.IsTrue(indices[1].SetEquals(HashSet<int>([0; 1])))
        Assert.IsTrue(indices[2].Count = 0)
        Assert.IsTrue(indices[3].SetEquals(HashSet<int>([3])))
        Assert.IsTrue(indices[4].SetEquals(HashSet<int>([3; 4])))

        let indices = Strings.FindMatchingStartIndices talpha [| PChar 'a'; PToken tlower; PChar 'b' |]
        Assert.IsTrue(indices[0].SetEquals(HashSet<int>([0])))
        Assert.IsTrue(indices[1].SetEquals(HashSet<int>([0; 1])))
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([0; 1; 2])))

        let indices = Strings.FindMatchingStartIndices talpha [| PChar 'a'; PToken tnum; PChar 'b' |]
        Assert.IsTrue(indices[0].SetEquals(HashSet<int>([0])))
        Assert.IsTrue(indices[1].Count = 0)
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([2])))

        let indices = Strings.FindMatchingStartIndices tlower [| PChar 'a'; PToken talpha; PChar 'b' |]
        Assert.IsTrue(indices[0].SetEquals(HashSet<int>([0])))
        Assert.IsTrue(indices[1].Count = 0)
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([2])))

        let indices = Strings.FindMatchingStartIndices talpha [| PAny; PToken tlower; PToken tlower |]
        Assert.IsTrue(indices[0].Count = 0)
        Assert.IsTrue(indices[1].SetEquals(HashSet<int>([1])))
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([1; 2])))

    /// Test that pattern alignment is working as expected with greedy tokens
    [<TestMethod>]
    member this.TestFindStartingIndicesGreedy () =
        let tnum = Token("number", "^[0-9]+$", 0.2)
        let tlower = Token("lower", "^[a-z]+$", 0.2)
        let talpha = Token("alpha", "^[a-zA-Z]+$", 0.2)
        let tdigit = Token("digit", "^[0-9]$", 0.2)
        let tfinite = Token("digit", "^a.b$", 0.2)

        let indices = Strings.FindMatchingStartIndices tfinite [| PChar 'a'; PChar 'a'; PChar 'b'; PChar 'b' |]
        Assert.IsTrue(indices[0].Count = 0)
        Assert.IsTrue(indices[1].Count = 0)
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([0])))
        Assert.IsTrue(indices[3].SetEquals(HashSet<int>([1])))

        let indices = Strings.FindMatchingStartIndices tdigit [| PChar '2'; PToken tnum; PChar '3'; PChar '4' |]
        Assert.IsTrue(indices[0].SetEquals(HashSet<int>([0])))
        Assert.IsTrue(indices[1].Count = 0)
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([2])))
        Assert.IsTrue(indices[3].SetEquals(HashSet<int>([3])))

        let indices = Strings.FindMatchingStartIndices tnum [| PChar 'a'; PToken tnum; PChar '3' |]
        Assert.IsTrue(indices[0].Count = 0)
        Assert.IsTrue(indices[1].Count = 0)
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([1; 2])))

        let indices = Strings.FindMatchingStartIndices tnum [| PChar 'a'; PToken tnum; PToken tnum; PChar 'b' |]
        Assert.IsTrue(indices[0].Count = 0)
        Assert.IsTrue(indices[1].Count = 0)
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([1; 2])))
        Assert.IsTrue(indices[3].Count = 0)

        let indices = Strings.FindMatchingStartIndices tnum [| PChar '1'; PToken tnum; PChar 'b'; PToken tnum; PChar '4' |]
        Assert.IsTrue(indices[0].Count = 0)
        Assert.IsTrue(indices[1].SetEquals(HashSet<int>([0; 1])))
        Assert.IsTrue(indices[2].Count = 0)
        Assert.IsTrue(indices[3].Count = 0)
        Assert.IsTrue(indices[4].SetEquals(HashSet<int>([3; 4])))

        let indices = Strings.FindMatchingStartIndices talpha [| PChar 'a'; PToken tlower; PChar 'b' |]
        Assert.IsTrue(indices[0].Count = 0)
        Assert.IsTrue(indices[1].Count = 0)
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([0; 1; 2])))

        let indices = Strings.FindMatchingStartIndices talpha [| PChar 'a'; PToken tnum; PChar 'b' |]
        Assert.IsTrue(indices[0].SetEquals(HashSet<int>([0])))
        Assert.IsTrue(indices[1].Count = 0)
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([2])))

        let indices = Strings.FindMatchingStartIndices tlower [| PChar 'a'; PToken talpha; PChar 'b' |]
        Assert.IsTrue(indices[0].SetEquals(HashSet<int>([0])))
        Assert.IsTrue(indices[1].Count = 0)
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([2])))

        let indices = Strings.FindMatchingStartIndices talpha [| PAny; PToken tlower; PToken tlower |]
        Assert.IsTrue(indices[0].Count = 0)
        Assert.IsTrue(indices[1].Count = 0)
        Assert.IsTrue(indices[2].SetEquals(HashSet<int>([1; 2])))


/// Test class for learning outliers/bugs.
[<TestClass>]
[<ExcludeFromCodeCoverage>]
type LearningTests () =

    /// Test that the learning is summarizing correctly.
    [<TestMethod>]
    member this.TestLearning1 () =
        let t1 = Tree.FromJson """{"key1": ["A", "B", "C"], "key2": null}"""
        let t2 = Tree.FromJson """{"key1": ["A", "B", "D"], "key3": "hi"}"""
        let names = [|"C1"; "C2"|]
        let t = Template.Infer [|t1; t2|]
        let lparams : Learning.LearningParameters = {
            Support = 0.0
            Confidence = 0.0
            NumConfigs = names.Length
            Filters = [||]
        }
        let actual = Learning.FindOutliers lparams t |> Learning.ToCsv names false
        printfn "%A" actual
        let expected = """Name,Expression,Score
C2,present(/key3) = 'True',0.5
C1,present(/key3) = 'False',0.5
C2,present(/key2) = 'False',0.5
C1,present(/key2) = 'True',0.5
C2,present(/key1/elt[3]:D) = 'True',0.5
C1,present(/key1/elt[3]:D) = 'False',0.5
C2,present(/key1/elt[2]:C) = 'False',0.5
C1,present(/key1/elt[2]:C) = 'True',0.5"""
        Assert.IsTrue((actual = expected))

    /// Test that the learning is summarizing correctly.
    [<TestMethod>]
    member this.TestLearning2 () =
        let t1 = Tree.FromJson """{"key1": ["A", "B", "C"], "key2": null}"""
        let t2 = Tree.FromJson """{"key1": ["A", "B", "D"], "key2": "hi"}"""
        let names = [|"C1"; "C2"|]
        let t = Template.Infer [|t1; t2|]
        let lparams : Learning.LearningParameters = {
            Support = 0.0
            Confidence = 0.0
            NumConfigs = names.Length
            Filters = [||]
        }
        let actual = Learning.FindOutliers lparams t |> Learning.ToCsv names false
        printfn "%A" actual
        let expected = """Name,Expression,Score
C2,"pattern(/key2, 'hi') = 'True'",0.5
C1,"pattern(/key2, null) = 'True'",0.5
C2,present(/key1/elt[3]:D) = 'True',0.5
C1,present(/key1/elt[3]:D) = 'False',0.5
C2,present(/key1/elt[2]:C) = 'False',0.5
C1,present(/key1/elt[2]:C) = 'True',0.5"""
        Assert.IsTrue((actual = expected))

    /// Test that the learning is summarizing correctly.
    [<TestMethod>]
    member this.TestLearning3 () =
        let t1 = Tree.FromJson """{"key": "abc123"}"""
        let t2 = Tree.FromJson """{"key": "bc12"}"""
        let names = [|"C1"; "C2"|]
        let t = Template.Infer [|t1; t2|]
        let lparams : Learning.LearningParameters = {
            Support = 0.0
            Confidence = 0.0
            NumConfigs = names.Length
            Filters = [||]
        }
        let actual = Learning.FindOutliers lparams t |> Learning.ToCsv names false
        printfn "%A" actual
        let expected = """Name,Expression,Score
C2,value(/key/param[1]:[any]bc12[any]) = '',0.5
C1,value(/key/param[1]:[any]bc12[any]) = '3',0.5
C2,pow2(/key/param[1]:[any]bc12[any]) = 'False',0.5
C1,pow2(/key/param[1]:[any]bc12[any]) = 'False',0.5
C2,integer(/key/param[1]:[any]bc12[any]) = 'False',0.5
C1,integer(/key/param[1]:[any]bc12[any]) = 'True',0.5
C2,value(/key/param[0]:[any]bc12[any]) = '',0.5
C1,value(/key/param[0]:[any]bc12[any]) = 'a',0.5
C2,pow2(/key/param[0]:[any]bc12[any]) = 'False',0.5
C1,pow2(/key/param[0]:[any]bc12[any]) = 'False',0.5
C2,integer(/key/param[0]:[any]bc12[any]) = 'False',0.5
C1,integer(/key/param[0]:[any]bc12[any]) = 'False',0.5"""
        Assert.IsTrue((actual = expected))

    /// Test that the learning is summarizing correctly.
    [<TestMethod>]
    member this.TestLearning4 () =
        let t1 = Tree.FromJson """{"key1": "1.0", "key2": null}"""
        let t2 = Tree.FromJson """{"key1": "1.0", "key2": "foo"}"""
        let t3 = Tree.FromJson """{"key1": "1.0", "key2": "bar"}"""
        let t4 = Tree.FromJson """{"key1": "2.0", "key2": "baz"}"""
        let names = [|"C1"; "C2"; "C3"; "C4"|]
        let t = Template.Infer [|t1; t2; t3; t4|]
        let lparams : Learning.LearningParameters = {
            Support = 0.0
            Confidence = 0.51
            NumConfigs = names.Length
            Filters = [||]
        }
        let actual = Learning.FindOutliers lparams t |> Learning.ToCsv names false
        printfn "%A" actual
        let expected = """Name,Expression,Score
C4,value(/key1/param[0]:[any].0) = '2',0.6877436677784063
C1,"pattern(/key2, null) = 'True'",0.5703479706665715"""
        Assert.IsTrue((actual = expected))

    /// Test that the learning is summarizing correctly.
    [<TestMethod>]
    member this.TestLearning5 () =
        let t1 = Tree.FromJson """{"key1": "1.0"}"""
        let t2 = Tree.FromJson """{"key1": "1.0"}"""
        let t3 = Tree.FromJson """{"key1": "1.0"}"""
        let t4 = Tree.FromJson """{"key1": "2.0"}"""
        let t5 = Tree.FromJson """{"key1": "2.0"}"""
        let t6 = Tree.FromJson """{"key1": "100.0"}"""
        let names = [|"C1"; "C2"; "C3"; "C4"; "C5"; "C6"|]
        let t = Template.Infer [|t1; t2; t3; t4; t5; t6|]
        let lparams : Learning.LearningParameters = {
            Support = 0.0
            Confidence = 0.51
            NumConfigs = names.Length
            Filters = [||]
        }
        let actual = Learning.FindOutliers lparams t |> Learning.ToCsv names false
        printfn "%A" actual
        let expected = """Name,Expression,Score
C6,value(/key1/param[0]:[any].0) = '100',0.7740712644423979
C6,pow2(/key1/param[0]:[any].0) = 'False',0.7740712644423979"""
        Assert.IsTrue((actual = expected))

/// Test class for helper algorithms for clustering.
[<TestClass>]
[<ExcludeFromCodeCoverage>]
type ClusteringTests () =

    /// Test that clustering works as expected.
    [<TestMethod>]
    member this.TestClusteringHierarchical () =
        let maxDistance (values : double[]) : double =
            Array.allPairs values values
            |> Array.map (fun (x, y) -> abs (x - y))
            |> Array.max
        in
        Assert.IsTrue((Clustering.ClusterHierarchical 3.0 maxDistance [|1.0; 2.0; 10.0; 11.0; 20.0; 21.0|] = [|[|1.0; 2.0|]; [|10.0; 11.0|]; [|20.0; 21.0|]|]))
        Assert.IsTrue((Clustering.ClusterHierarchical 4.0 maxDistance [|1.0; 2.0; 4.9; 10.0; 13.0; 21.0|] = [|[|1.0; 2.0; 4.9|]; [|10.0; 13.0|]; [|21.0|]|]))
        Assert.IsTrue((Clustering.ClusterHierarchical 3.9 maxDistance [|1.0; 2.0; 5.0; 10.0; 13.0; 21.0|] = [|[|1.0; 2.0|]; [|5.0|]; [|10.0; 13.0|]; [|21.0|]|]))
        Assert.IsTrue((Clustering.ClusterHierarchical 3.9 maxDistance [|1.0; 2.0; 4.5; 5.0|] = [|[|1.0; 2.0|]; [|4.5; 5.0|]|]))
        Assert.IsTrue((Clustering.ClusterHierarchical 4.0 maxDistance [|1.0|] = [|[|1.0|]|]))
        Assert.IsTrue((Clustering.ClusterHierarchical 2.0 maxDistance [|1.0; 2.9|] = [|[|1.0; 2.9|]|]))
        Assert.IsTrue((Clustering.ClusterHierarchical 1.9 maxDistance [|1.0; 3.0|] = [|[|1.0|]; [|3.0|]|]))
        Assert.IsTrue((Clustering.ClusterHierarchical 2.0 maxDistance [|1.0; 1.0|] = [|[|1.0; 1.0|]|]))

    /// Test that clustering works as expected.
    [<TestMethod>]
    member this.TestClusteringGreedy () =
        let maxDistance (values : double[]) : double =
            Array.allPairs values values
            |> Array.map (fun (x, y) -> abs (x - y))
            |> Array.max
        in
        Assert.IsTrue((Clustering.ClusterGreedy 3.0 maxDistance [|1.0; 2.0; 10.0; 11.0; 20.0; 21.0|] = [|[|1.0; 2.0|]; [|10.0; 11.0|]; [|20.0; 21.0|]|]))
        Assert.IsTrue((Clustering.ClusterGreedy 4.0 maxDistance [|1.0; 2.0; 4.9; 10.0; 13.0; 21.0|] = [|[|1.0; 2.0; 4.9|]; [|10.0; 13.0|]; [|21.0|]|]))
        Assert.IsTrue((Clustering.ClusterGreedy 3.9 maxDistance [|1.0; 2.0; 5.0; 10.0; 13.0; 21.0|] = [|[|1.0; 2.0|]; [|5.0|]; [|10.0; 13.0|]; [|21.0|]|]))
        Assert.IsTrue((Clustering.ClusterGreedy 3.9 maxDistance [|1.0; 2.0; 4.5; 5.0|] = [|[|1.0; 2.0; 4.5|]; [|5.0|]|]))
        Assert.IsTrue((Clustering.ClusterGreedy 4.0 maxDistance [|1.0|] = [|[|1.0|]|]))
        Assert.IsTrue((Clustering.ClusterGreedy 2.0 maxDistance [|1.0; 2.9|] = [|[|1.0; 2.9|]|]))
        Assert.IsTrue((Clustering.ClusterGreedy 1.9 maxDistance [|1.0; 3.0|] = [|[|1.0|]; [|3.0|]|]))
        Assert.IsTrue((Clustering.ClusterGreedy 2.0 maxDistance [|1.0; 1.0|] = [|[|1.0; 1.0|]|]))

    /// Test that clustering works as expected.
    [<TestMethod>]
    member this.TestClusteringPriority () =
        let maxDistance (values : double[]) : double =
            Array.allPairs values values
            |> Array.map (fun (x, y) -> abs (x - y))
            |> Array.max
        in
        Assert.IsTrue((Clustering.ClusterPriority 3.0 maxDistance [|1.0; 2.0; 10.0; 11.0; 20.0; 21.0|] = [|[|1.0; 2.0|]; [|10.0; 11.0|]; [|20.0; 21.0|]|]))
        Assert.IsTrue((Clustering.ClusterPriority 4.0 maxDistance [|1.0; 2.0; 4.9; 10.0; 13.0; 21.0|] = [|[|1.0; 2.0; 4.9|]; [|10.0; 13.0|]; [|21.0|]|]))
        Assert.IsTrue((Clustering.ClusterPriority 3.9 maxDistance [|1.0; 2.0; 5.0; 10.0; 13.0; 21.0|] = [|[|1.0; 2.0|]; [|5.0|]; [|10.0; 13.0|]; [|21.0|]|]))
        Assert.IsTrue((Clustering.ClusterPriority 3.9 maxDistance [|1.0; 2.0; 4.5; 5.0|] = [|[|1.0; 2.0; 4.5|]; [|5.0|]|]))
        Assert.IsTrue((Clustering.ClusterPriority 4.0 maxDistance [|1.0|] = [|[|1.0|]|]))
        Assert.IsTrue((Clustering.ClusterPriority 2.0 maxDistance [|1.0; 2.9|] = [|[|1.0; 2.9|]|]))
        Assert.IsTrue((Clustering.ClusterPriority 1.9 maxDistance [|1.0; 3.0|] = [|[|1.0|]; [|3.0|]|]))
        Assert.IsTrue((Clustering.ClusterPriority 2.0 maxDistance [|1.0; 1.0|] = [|[|1.0; 1.0|]|]))


/// Test class for helper algorithms for strings
/// and clustering and more.
[<TestClass>]
[<ExcludeFromCodeCoverage>]
type BipartiteMatchingTests () =

    /// Test that bellman ford is working as expected.
    [<TestMethod>]
    member this.TestBellmanFord1 () =
        let e1 = { Src = 0; Tgt = 1; Cap = 1; Cost = 1; Flow = 0 }
        let g : Graph = [| [|e1|]; [||] |]
        let (distance, parent) = Matching.BellmanFord g 0
        Assert.IsTrue (distance[1] = 1)
        Assert.IsTrue (parent[1] = 0)
        Assert.IsTrue (parent[0] = -1)

    /// Test that bellman ford is working as expected.
    [<TestMethod>]
    member this.TestBellmanFord2 () =
        let e1 = { Src = 0; Tgt = 1; Cap = 1; Cost = 1; Flow = 0 }
        let e2 = { Src = 0; Tgt = 2; Cap = 1; Cost = 2; Flow = 0 }
        let e3 = { Src = 1; Tgt = 3; Cap = 1; Cost = 5; Flow = 0 }
        let e4 = { Src = 2; Tgt = 3; Cap = 1; Cost = 1; Flow = 0 }
        let g : Graph = [| [|e1; e2|]; [|e3|]; [|e4|]; [||] |]
        let (distance, parent) = Matching.BellmanFord g 0
        Assert.IsTrue (distance[3] = 3)
        Assert.IsTrue (parent[3] = 2)
        Assert.IsTrue (parent[2] = 0)
        Assert.IsTrue (parent[0] = -1)

    /// Test that bellman ford is working as expected.
    [<TestMethod>]
    member this.TestBellmanFord3 () =
        let e1 = { Src = 0; Tgt = 1; Cap = 1; Cost = 1; Flow = 0 }
        let e2 = { Src = 1; Tgt = 0; Cap = 1; Cost = 1; Flow = 0 }
        let g : Graph = [| [|e1|]; [|e2|] |]
        let (distance, parent) = Matching.BellmanFord g 0
        Assert.IsTrue (distance[1] = 1)
        Assert.IsTrue (distance[0] = 0)
        Assert.IsTrue (parent[1] = 0)
        Assert.IsTrue (parent[0] = -1)

    /// Test that bellman ford is working as expected.
    [<TestMethod>]
    member this.TestBellmanFord4 () =
        let e1 = { Src = 0; Tgt = 1; Cap = 1; Cost = 1; Flow = 0 }
        let e2 = { Src = 0; Tgt = 2; Cap = 1; Cost = 100; Flow = 0 }
        let e3 = { Src = 1; Tgt = 3; Cap = 1; Cost = 20; Flow = 0 }
        let e4 = { Src = 2; Tgt = 3; Cap = 1; Cost = -99; Flow = 0 }
        let g : Graph = [| [|e1; e2|]; [|e3|]; [|e4|]; [||] |]
        let (distance, parent) = Matching.BellmanFord g 0
        Assert.IsTrue (distance[3] = 1)
        Assert.IsTrue (parent[3] = 2)
        Assert.IsTrue (parent[2] = 0)
        Assert.IsTrue (parent[0] = -1)

    /// Test that min cost max flow is working as expected.
    [<TestMethod>]
    member this.TestMinCostMaxFlow1 () =
        let e1 = { Src = 0; Tgt = 1; Cap = 1; Cost = 0; Flow = 0 }
        let e2 = { Src = 0; Tgt = 2; Cap = 1; Cost = 0; Flow = 0 }
        let e3 = { Src = 1; Tgt = 3; Cap = 1; Cost = 2; Flow = 0 }
        let e4 = { Src = 1; Tgt = 4; Cap = 1; Cost = 5; Flow = 0 }
        let e5 = { Src = 2; Tgt = 3; Cap = 1; Cost = 5; Flow = 0 }
        let e6 = { Src = 2; Tgt = 4; Cap = 1; Cost = 10; Flow = 0 }
        let e7 = { Src = 3; Tgt = 5; Cap = 1; Cost = 0; Flow = 0 }
        let e8 = { Src = 4; Tgt = 5; Cap = 1; Cost = 0; Flow = 0 }
        let g : Graph = [| [|e1; e2|]; [|e3; e4|]; [|e5; e6|]; [|e7|]; [|e8|]; [||] |]
        Matching.MinCostMaxFlow g 0 5
        Assert.IsTrue(e1.Flow = 1)
        Assert.IsTrue(e2.Flow = 1)
        Assert.IsTrue(e3.Flow = 0)
        Assert.IsTrue(e4.Flow = 1)
        Assert.IsTrue(e5.Flow = 1)
        Assert.IsTrue(e6.Flow = 0)
        Assert.IsTrue(e7.Flow = 1)
        Assert.IsTrue(e8.Flow = 1)

    /// Test that min cost max flow is working as expected.
    [<TestMethod>]
    member this.TestMinCostMaxFlow2 () =
        let e1 = { Src = 0; Tgt = 1; Cap = 1; Cost = 0; Flow = 0 }
        let e2 = { Src = 0; Tgt = 2; Cap = 1; Cost = 0; Flow = 0 }
        let e3 = { Src = 1; Tgt = 3; Cap = 1; Cost = 2; Flow = 0 }
        let e4 = { Src = 1; Tgt = 4; Cap = 1; Cost = 7; Flow = 0 }
        let e5 = { Src = 2; Tgt = 3; Cap = 1; Cost = 7; Flow = 0 }
        let e6 = { Src = 2; Tgt = 4; Cap = 1; Cost = 10; Flow = 0 }
        let e7 = { Src = 3; Tgt = 5; Cap = 1; Cost = 0; Flow = 0 }
        let e8 = { Src = 4; Tgt = 5; Cap = 1; Cost = 0; Flow = 0 }
        let g : Graph = [| [|e1; e2|]; [|e3; e4|]; [|e5; e6|]; [|e7|]; [|e8|]; [||] |]
        let flow = Matching.MinCostMaxFlow g 0 5
        Assert.IsTrue(e1.Flow = 1)
        Assert.IsTrue(e2.Flow = 1)
        Assert.IsTrue(e3.Flow = 1)
        Assert.IsTrue(e4.Flow = 0)
        Assert.IsTrue(e5.Flow = 0)
        Assert.IsTrue(e6.Flow = 1)
        Assert.IsTrue(e7.Flow = 1)
        Assert.IsTrue(e8.Flow = 1)

    /// Test that min cost max flow is working as expected.
    [<TestMethod>]
    member this.TestBipartiteMatch () =
        let w0 = array2D []
        let w1 = array2D [[2; 5]; [5; 10]]
        let w2 = array2D [[2; 7]; [7; 10]]
        let w3 = array2D [[1; 2; 3]; [4; 6; 2]; [9; 3; 6]]
        let w4 = array2D [[1; 100]]
        let w5 = array2D [[2]; [1]]
        let w6 = array2D [[5; -1; 3; -1]; [4; -1; -1; 1]; [-1; 7; -1; 2]]
        let w7 = array2D [[10; -1]; [-1; -1]]
        let w8 = array2D [[2; 5; 6]; [5; 10; 6]; [6; 1000; 1000]]
        let w9 = array2D [[-1]; [-1]]
        Assert.IsTrue (Matching.MinWeightMatchingInt w0 = [||])
        Assert.IsTrue (Matching.MinWeightMatchingInt w1 = [|1; 0|])
        Assert.IsTrue (Matching.MinWeightMatchingInt w2 = [|0; 1|])
        Assert.IsTrue (Matching.MinWeightMatchingInt w3 = [|0; 2; 1|])
        Assert.IsTrue (Matching.MinWeightMatchingInt w4 = [|0|])
        Assert.IsTrue (Matching.MinWeightMatchingInt w5 = [|-1; 0|])
        Assert.IsTrue (Matching.MinWeightMatchingInt w6 = [|2; 0; 3|])
        Assert.IsTrue (Matching.MinWeightMatchingInt w7 = [|0; -1|])
        Assert.IsTrue (Matching.MinWeightMatchingInt w8 = [|1; 2; 0|])
        Assert.IsTrue (Matching.MinWeightMatchingInt w9 = [|-1; -1|])

    /// Test that min cost max flow is working as expected.
    [<TestMethod>]
    member this.TestBipartiteMatchDouble () =
        let w0 = array2D []
        let w1 = array2D [[2.0; 5.0]; [5.0; 10.0]]
        let w2 = array2D [[2.0; 7.0]; [7.0; 10.0]]
        let w3 = array2D [[1.0; 2.0; 3.0]; [4.0; 6.0; 2.0]; [9.0; 3.0; 6.0]]
        let w4 = array2D [[1.0; 100.0]]
        let w5 = array2D [[2.0]; [1.0]]
        let w6 = array2D [[5.0; -1.0; 3.0; -1.0]; [4.0; -1.0; -1.0; 1.0]; [-1.0; 7.0; -1.0; 2.0]]
        let w7 = array2D [[10.0; -1.0]; [-1.0; -1.0]]
        let w8 = array2D [[2.0; 5.0; 6.0]; [5.0; 10.0; 6.0]; [6.0; 1000.0; 1000.0]]
        let w9 = array2D [[-1.0]; [-1.0]]
        Assert.IsTrue (Matching.MinWeightMatching w0 = [||])
        Assert.IsTrue (Matching.MinWeightMatching w1 = [|1; 0|])
        Assert.IsTrue (Matching.MinWeightMatching w2 = [|0; 1|])
        Assert.IsTrue (Matching.MinWeightMatching w3 = [|0; 2; 1|])
        Assert.IsTrue (Matching.MinWeightMatching w4 = [|0|])
        Assert.IsTrue (Matching.MinWeightMatching w5 = [|-1; 0|])
        Assert.IsTrue (Matching.MinWeightMatching w6 = [|2; 0; 3|])
        Assert.IsTrue (Matching.MinWeightMatching w7 = [|0; -1|])
        Assert.IsTrue (Matching.MinWeightMatching w8 = [|1; 2; 0|])
        Assert.IsTrue (Matching.MinWeightMatching w9 = [|-1; -1|])

    /// Test that min cost max flow is working as expected.
    [<TestMethod>]
    member this.TestBipartiteMatchGreedy () =
        let w0 = array2D []
        let w1 = array2D [[2.0; 5.0]; [5.0; 10.0]]
        let w2 = array2D [[2.0; 7.0]; [7.0; 10.0]]
        let w3 = array2D [[1.0; 2.0; 3.0]; [4.0; 6.0; 2.0]; [9.0; 3.0; 6.0]]
        let w4 = array2D [[1.0; 100.0]]
        let w5 = array2D [[2.0]; [1.0]]
        let w6 = array2D [[5.0; -1.0; 3.0; -1.0]; [4.0; -1.0; -1.0; 1.0]; [-1.0; 7.0; -1.0; 2.0]]
        let w7 = array2D [[10.0; -1.0]; [-1.0; -1.0]]
        let w8 = array2D [[2.0; 5.0; 6.0]; [5.0; 10.0; 6.0]; [6.0; 1000.0; 1000.0]]
        let w9 = array2D [[-1.0]; [-1.0]]
        Assert.IsTrue (Matching.MinWeightMatchingGreedy w0 = [||])
        Assert.IsTrue (Matching.MinWeightMatchingGreedy w1 = [|0; 1|])
        Assert.IsTrue (Matching.MinWeightMatchingGreedy w2 = [|0; 1|])
        Assert.IsTrue (Matching.MinWeightMatchingGreedy w3 = [|0; 2; 1|])
        Assert.IsTrue (Matching.MinWeightMatchingGreedy w4 = [|0|])
        Assert.IsTrue (Matching.MinWeightMatchingGreedy w5 = [|-1; 0|])
        Assert.IsTrue (Matching.MinWeightMatchingGreedy w6 = [|2; 3; 1|])
        Assert.IsTrue (Matching.MinWeightMatchingGreedy w7 = [|0; -1|])
        Assert.IsTrue (Matching.MinWeightMatchingGreedy w8 = [|0; 2; 1|])
        Assert.IsTrue (Matching.MinWeightMatchingGreedy w9 = [|-1; -1|])


/// Test class for learning outliers/bugs.
[<TestClass>]
[<ExcludeFromCodeCoverage>]
type AnomalyTests () =

    /// Approximate equality for scores.
    let approxEq (expected : double[]) (actual : double[]) : bool =
        if expected.Length <> actual.Length then false
        else
            let mutable result = true
            for i = 0 to expected.Length - 1 do
                if abs (expected[i] - actual[i]) > 0.02 then
                    result <- false
            result

    /// Test that the isolation forest implementation is working.
    [<TestMethod>]
    member this.TestIsolationForest1 () =
        let data = [| [|1.0|]; [|2.0|]; [| 3.0 |] |]
        let scores = Anomalies.IsolationForest data
        Assert.IsTrue (approxEq scores [|0.41; 0.32; 0.43|])

    /// Test that the isolation forest implementation is working.
    [<TestMethod>]
    member this.TestIsolationForest2 () =
        let data = [| [|1.0|]; [|2.0|]; [| 3.0 |]; [|4.0|]; [|100.0|] |]
        let scores = Anomalies.IsolationForest data
        Assert.IsTrue (approxEq scores [|0.43; 0.35; 0.35; 0.44; 0.73|])

    /// Test that the isolation forest implementation is working.
    [<TestMethod>]
    member this.TestIsolationForest3 () =
        let data = [| [|1.0|]; [|1.0|]; [|1.0|]; [|1.0|]; [|1.0|]; [|2.0|]; [|2.0|]; [|100.0|] |]
        let scores = Anomalies.IsolationForest data
        Assert.IsTrue (approxEq scores [|0.40; 0.40; 0.40; 0.40; 0.40; 0.53; 0.53; 0.81|])

    /// Test that the isolation forest implementation is working.
    [<TestMethod>]
    member this.TestIsolationForest4 () =
        let data = [| [|1.0|]; [|1.0|]; [|1.0|] |]
        let scores = Anomalies.IsolationForest data
        Assert.IsTrue (approxEq scores [|0.5; 0.5; 0.5|])

    /// Test that the isolation forest implementation is working.
    [<TestMethod>]
    member this.TestIsolationForest5 () =
        let data = [| [|1.0|]; [|1.0|]; [|1.0|]; [|1.0|]; [|100.0|] |]
        let scores = Anomalies.IsolationForest data
        Assert.IsTrue (approxEq scores [|0.43; 0.43; 0.43; 0.43; 0.74|])

    /// Test that the isolation forest implementation is working.
    [<TestMethod>]
    member this.TestIsolationForest6 () =
        let data = [| [|-100.0|]; [|0.0|]; [|0.0|]; [|0.0|]; [|100.0|] |]
        let scores = Anomalies.IsolationForest data
        Assert.IsTrue (approxEq scores [|0.63; 0.38; 0.38; 0.38; 0.65|])

    /// Test that the anomalies work with strings.
    [<TestMethod>]
    member this.TestLearningAnomalies1 () =
        let scores = Anomalies.IsolationForestStrings [|"1.0"; "2"; "3"; "100"|]
        Assert.IsTrue (approxEq scores [|0.4; 0.32; 0.39; 0.69|])

    /// Test that the anomalies work with strings.
    [<TestMethod>]
    member this.TestLearningAnomalies2 () =
        let scores = Anomalies.IsolationForestStrings [|"1"; "1"; "1"|]
        Assert.IsTrue (approxEq scores [|0.5; 0.5; 0.5|])

    /// Test that the anomalies work with strings.
    [<TestMethod>]
    member this.TestLearningAnomalies3 () =
        let scores = Anomalies.IsolationForestStrings [|"a"; "b"; "c"|]
        printfn "%A" scores
        Assert.IsTrue (approxEq scores [|0.41; 0.31; 0.43|])

    /// Test that the anomalies work with strings.
    [<TestMethod>]
    member this.TestLearningAnomalies4 () =
        let scores = Anomalies.IsolationForestStrings [|"a"; "a"; "a"; "a"; "a"; "b"|]
        Assert.IsTrue (approxEq scores [|0.43; 0.43; 0.43; 0.43; 0.43; 0.77|])

    /// Test that the anomalies work with strings.
    [<TestMethod>]
    member this.TestLearningAnomalies5 () =
        let scores = Anomalies.IsolationForestStrings [|"a"; "a"; "a"; "a"; "a"; "c"; "b"; "b"|]
        Assert.IsTrue (approxEq scores [|0.44; 0.44; 0.44; 0.44; 0.44; 0.74; 0.53; 0.53|])

    /// Test that the anomalies work with strings.
    [<TestMethod>]
    member this.TestLearningAnomalies6 () =
        let scores = Anomalies.IsolationForestStrings [|"1"; "a"; "2"|]
        Assert.IsTrue (approxEq scores [|0.41; 0.43; 0.31|])

/// Test class for testing the templating logic
/// as well as all the templating helper functions.
[<TestClass>]
[<ExcludeFromCodeCoverage>]
type TreeTests () =

    /// Test that the template inference is working for basic strings.
    [<TestMethod>]
    member this.TestJsonConversion1 () =
        let tree = Tree.FromJson """{"key1": ["iface-1", "bar"], "key2": 20}"""
        let t1 = Tree.Leaf("iface-1", typeof<string>)
        let t2 = Tree.Leaf("bar", typeof<string>)
        let t3 = Tree.Leaf("20", typeof<int>)
        let t4 = Tree.List [| t1; t2 |]
        let t5 = Tree.Record [| ("key1", t4); ("key2", t3) |]
        Assert.IsTrue((tree = t5))

    /// Test that approximate similarity works.
    [<TestMethod>]
    member this.TestApproximateSimilarity () =
        let t1 = Tree.FromJson """{"key1": ["iface-1", "bar"], "key2": 20}"""
        let t2 = Tree.FromJson """{"key1": ["iface-1", "bar"], "key2": 20}"""
        let t3 = Tree.FromJson """{"key1": ["iface-1", "baz"], "key2": 20}"""
        Assert.IsTrue((0.0 = Tree.ApproximateSimilarity t1 t2))
        Assert.IsTrue((0.6 = Tree.ApproximateSimilarity t1 t3))

/// Test class for the regular expression implementation.
[<TestClass>]
[<ExcludeFromCodeCoverage>]
type RegexTests () =

    /// Test that char operations work.
    [<TestMethod>]
    member this.TestCharRangeOperations () =
        let r1 = new CharRange((char)1, (char)10);
        let r2 = new CharRange((char)0, (char)11);
        let r3 = new CharRange((char)100, System.Char.MaxValue);
        let r4 = new CharRange((char)0, System.Char.MaxValue);
        let r5 = new CharRange((char)10, (char)1);

        // test isempty
        Assert.IsFalse(r1.IsEmpty());
        Assert.IsFalse(r2.IsEmpty());
        Assert.IsFalse(r3.IsEmpty());
        Assert.IsFalse(r4.IsEmpty());
        Assert.IsTrue(r5.IsEmpty());

        // test isfull
        Assert.IsFalse(r1.IsFull());
        Assert.IsFalse(r2.IsFull());
        Assert.IsFalse(r3.IsFull());
        Assert.IsTrue(r4.IsFull());
        Assert.IsFalse(r5.IsFull());

        // test intersect
        Assert.AreEqual(r1, r1.Intersect(r2));
        Assert.AreEqual(r1, r1.Intersect(r4));
        Assert.IsTrue(r1.Intersect(r3).IsEmpty());
        Assert.AreEqual(r3, r3.Intersect(r4));
        Assert.AreEqual(r2, r2.Intersect(r2));

        // test complement
        Assert.AreEqual(0, r4.Complement().Length);
        Assert.AreEqual(1, r2.Complement().Length);
        Assert.AreEqual(new CharRange((char)12, System.Char.MaxValue), r2.Complement()[0]);
        Assert.AreEqual(1, r3.Complement().Length);
        Assert.AreEqual(new CharRange((char)0, (char)99), r3.Complement()[0]);
        Assert.AreEqual(2, r1.Complement().Length);
        Assert.AreEqual(new CharRange((char)0, (char)0), r1.Complement()[0]);
        Assert.AreEqual(new CharRange((char)11, System.Char.MaxValue), r1.Complement()[1]);

        // test contains
        Assert.IsTrue(r1.Contains((char)1));
        Assert.IsTrue(r1.Contains((char)10));
        Assert.IsTrue(r1.Contains((char)9));
        Assert.IsFalse(r1.Contains((char)0));
        Assert.IsFalse(r1.Contains((char)11));
        Assert.IsFalse(r1.Contains((char)255));

        // equals, hashcode
        Assert.IsTrue(r1.Equals(r1));
        Assert.IsFalse(r1.Equals(10));
        Assert.IsFalse(r1.Equals(r2));
        Assert.IsFalse(r2.Equals(r4));

    /// Test that regex simplification works.
    [<TestMethod>]
    member this.TestRegexSimplification () =
        let r = Regex.Char((char)1);
        let s = Regex.Char((char)2);
        let t = Regex.Char((char)3);
        let range = Regex.Range(System.Char.MinValue, System.Char.MaxValue);

        // range simplifications
        Assert.AreEqual(Regex.Range((char)5, (char)4), Regex.Empty());
        // unary simplifications
        Assert.AreEqual(Regex.Star(Regex.Star(r)), Regex.Star(r));
        Assert.AreEqual(Regex.Star(Regex.Epsilon()), Regex.Epsilon());
        Assert.AreEqual(Regex.Star(Regex.Empty()), Regex.Epsilon());
        Assert.AreEqual(Regex.Star(Regex.Dot()), Regex.Negation(Regex.Empty()));
        Assert.AreEqual(Regex.Negation(Regex.Negation(r)), r);
        Assert.AreEqual(Regex.Negation(range), Regex.Empty());
        // concat simplifications
        Assert.AreEqual(Regex.Concat(Regex.Empty(), r), Regex.Empty());
        Assert.AreEqual(Regex.Empty(), Regex.Concat(Regex.Empty(), r));
        Assert.AreEqual(Regex.Concat(Regex.Epsilon(), r), r);
        Assert.AreEqual(Regex.Concat(r, Regex.Epsilon()), r);
        Assert.AreEqual(Regex.Concat(r, Regex.Concat(s, t)), Regex.Concat(Regex.Concat(r, s), t));
        Assert.AreEqual(Regex.Concat(r, Regex.AnchorBegin()), Regex.Empty());
        Assert.AreEqual(Regex.Concat(Regex.AnchorEnd(), r), Regex.Empty());
        // intersection simplifications
        Assert.AreEqual(Regex.Intersect(r, r), r);
        Assert.AreEqual(Regex.Intersect(s, r), Regex.Intersect(r, s));
        Assert.AreEqual(Regex.Intersect(Regex.Intersect(r, s), t), Regex.Intersect(r, Regex.Intersect(s, t)));
        Assert.AreEqual(Regex.Intersect(Regex.Empty(), r), Regex.Empty());
        Assert.AreEqual(Regex.Intersect(r, Regex.Empty()), Regex.Empty());
        Assert.AreEqual(Regex.Intersect(Regex.Negation(Regex.Empty()), r), r);
        Assert.AreEqual(Regex.Intersect(r, Regex.Negation(Regex.Empty())), r);
        Assert.AreEqual(Regex.Intersect(r, Regex.Intersect(r, s)), Regex.Intersect(r, s));
        // union simplifications
        Assert.AreEqual(Regex.Union(r, r), r);
        Assert.AreEqual(Regex.Union(r, Regex.Empty()), r);
        Assert.AreEqual(Regex.Union(Regex.Empty(), r), r);
        Assert.AreEqual(Regex.Union(r, Regex.Negation(Regex.Empty())), Regex.Negation(Regex.Empty()));
        Assert.AreEqual(Regex.Union(Regex.Negation(Regex.Empty()), r), Regex.Negation(Regex.Empty()));
        Assert.AreEqual(Regex.Union(Regex.Dot(), Regex.Char((char)1)), Regex.Dot());
        Assert.AreEqual(Regex.Union(Regex.Char((char)1), Regex.Dot()), Regex.Dot());
        Assert.AreEqual(Regex.Union(s, r), Regex.Union(r, s));
        Assert.AreEqual(Regex.Union(Regex.Union(r, s), t), Regex.Union(r, Regex.Union(s, t)));
        Assert.AreEqual(Regex.Union(r, Regex.Union(r, s)), Regex.Union(r, s));

    /// Test that automata are working.
    [<TestMethod>]
    member this.TestAutomatonEmptiness1 () =
        let r1 = Regex.Concat(Regex.Star(Regex.Dot()), Regex.Char((char)1));
        let r2 = Regex.Concat(Regex.Star(Regex.Dot()), Regex.Char((char)2));
        let a = Regex.Intersect(r1, r2).ToAutomaton();
        Assert.IsTrue(a.FinalStates.Count = 0);    
        
    /// Test that automata are working.
    [<TestMethod>]
    member this.TestAutomatonEmptiness2 () =
        let r1 = Regex.Char((char)1);
        let r2 = Regex.Char((char)2);
        let a = Regex.Intersect(r1, r2).ToAutomaton();
        Assert.IsTrue(a.FinalStates.Count = 0);

    /// Test that automata are working.
    [<TestMethod>]
    member this.TestAutomatonEmptiness3 () =
        let r1 = Regex.Char((char)1);
        let r2 = Regex.Char((char)2);
        let a = Regex.Intersect(r1, r2).ToAutomaton();
        let r3 = Regex.Parse("[0-1]");
        let r4 = Regex.Parse("^a");
        let r5 = Regex.Parse(" ");
        let r6 = Regex.Parse("^[0-9]+$");
        let r7 = Regex.Parse("^01$");
        let r8 = Regex.Concat(r6, r7);
        let digits_complement = Regex.Negation(r6);
        let intersect = Regex.Intersect(r8, digits_complement);
        let automata = intersect.ToAutomaton();
          
        Assert.IsTrue(automata.IsEmpty());
        Assert.IsFalse(r1.ToAutomaton().IsEmpty());
        Assert.IsFalse(r2.ToAutomaton().IsEmpty());
        Assert.IsFalse(r3.ToAutomaton().IsEmpty());
        Assert.IsFalse(r4.ToAutomaton().IsEmpty());
        Assert.IsFalse(r5.ToAutomaton().IsEmpty());
        Assert.IsTrue(a.IsEmpty());
        Assert.IsTrue(Regex.Empty().ToAutomaton().IsEmpty());

    /// Test that regex parsing works.
    [<TestMethod>]
    [<DataRow("abc", true)>]
    [<DataRow(".bc", true)>]
    [<DataRow("(ab", false)>]
    [<DataRow("(abc)", true)>]
    [<DataRow("[abc]", true)>]
    [<DataRow("[abc", false)>]
    [<DataRow("[abc)", false)>]
    [<DataRow("a+", true)>]
    [<DataRow("(ab)*", true)>]
    [<DataRow("a|b", true)>]
    [<DataRow("ab|c", true)>]
    [<DataRow("(a|b)?", true)>]
    [<DataRow("a|", false)>]
    [<DataRow("?", false)>]
    [<DataRow("*", false)>]
    [<DataRow("(abcd*)**", true)>]
    [<DataRow("[abc]+", true)>]
    [<DataRow("[a[bc]]", false)>]
    [<DataRow("[0-9a-z]", true)>]
    [<DataRow("\\l", true)>]
    [<DataRow("\\(\\)", true)>]
    [<DataRow("[^a-zA-Z]", true)>]
    [<DataRow("[a\\]", false)>]
    [<DataRow("[9-0]", false)>]
    [<DataRow("[a-", false)>]
    [<DataRow(" ", true)>]
    [<DataRow("s ", true)>]
    [<DataRow("s s", true)>]
    [<DataRow("[ab ]", true)>]
    [<DataRow("a{3}", true)>]
    [<DataRow("a{3,}", true)>]
    [<DataRow("a{2,3}", true)>]
    [<DataRow("a{3,2}", false)>]
    [<DataRow("a{-1}", false)>]
    [<DataRow("a{2,b}", false)>]
    [<DataRow("a{b}", false)>]
    [<DataRow("a{10}", true)>]
    [<DataRow("a{10d}", false)>]
    [<DataRow("", true)>]
    [<DataRow("\\e", true)>]
    [<DataRow("\\", false)>]
    [<DataRow("BE\\", false)>]
    member this.TestRegexParsing (input : string, expected : bool) =
        try
            Regex.Parse(input) |> ignore
            Assert.IsTrue(expected);
        with _ -> Assert.IsFalse(expected);

    /// Test that regex matching works.
    [<TestMethod>]
    [<DataRow("abc", "abc", true)>]
    [<DataRow("abc", "ab", false)>]
    [<DataRow("abc", "abcd", true)>]
    [<DataRow("^abc$", "abcd", false)>]
    [<DataRow("^abc$", "abc", true)>]
    [<DataRow("^.bc", "xbc", true)>]
    [<DataRow("^.bc", "xbcd", true)>]
    [<DataRow("^.bc", "dxbc", false)>]
    [<DataRow("(abc)", "abc", true)>]
    [<DataRow("(abc)", "abcd", true)>]
    [<DataRow("[abc]", "a", true)>]
    [<DataRow("[abc]", "b", true)>]
    [<DataRow("[abc]", "c", true)>]
    [<DataRow("^[abc]$", "ab", false)>]
    [<DataRow("[0-9a-z]", "1", true)>]
    [<DataRow("[0-9a-z]", "g", true)>]
    [<DataRow("[0-9a-z]", "\n", false)>]
    [<DataRow("[0-9a-z]", "A", false)>]
    [<DataRow("^[0-9a-z]$", "01", false)>]
    [<DataRow("ab|c", "ab", true)>]
    [<DataRow("ab|c", "c", true)>]
    [<DataRow("ab|c", "a", false)>]
    [<DataRow("^(a|b)?$", "", true)>]
    [<DataRow("^(a|b)?$", "a", true)>]
    [<DataRow("^(a|b)?$", "b", true)>]
    [<DataRow("^(a|b)?$", "ab", false)>]
    [<DataRow("^(a|b)+$", "", false)>]
    [<DataRow("^(a|b)+$", "a", true)>]
    [<DataRow("^(a|b)+$", "aa", true)>]
    [<DataRow("^(a|b)+$", "abba", true)>]
    [<DataRow("[abc]+", "", false)>]
    [<DataRow("[abc]+", "ccba", true)>]
    [<DataRow("[abc]+$", "aabd", false)>]
    [<DataRow(@"\(\)", "()", true)>]
    [<DataRow("\\(\\)", "()", true)>]
    [<DataRow(@"\n", "n", true)>]
    [<DataRow("\n", "\n", true)>]
    [<DataRow("[ab\\+]", "+", true)>]
    [<DataRow("^[ab\\+]", "\\+", false)>]
    [<DataRow("\\\\", "\\", true)>]
    [<DataRow("[^a-zA-Z]", "g", false)>]
    [<DataRow("[^a-zA-Z]", "2", true)>]
    [<DataRow("abcd\\||bc", "bc", true)>]
    [<DataRow("abcd\\||bc$", "abcd", false)>]
    [<DataRow("abcd\\||bc", "abcd|", true)>]
    [<DataRow("[a-]+", "---", true)>]
    [<DataRow("[a*]+", "a*a", true)>]
    [<DataRow("[*-\\\\]+", "\\\\", true)>]
    [<DataRow("(a|b|c|d)", "a", true)>]
    [<DataRow("(a|b|c|d)", "b", true)>]
    [<DataRow("(a|b|c|d)", "c", true)>]
    [<DataRow("(a|b|c|d)", "d", true)>]
    [<DataRow(" ", " ", true)>]
    [<DataRow("s ", "s ", true)>]
    [<DataRow("s s", "s s", true)>]
    [<DataRow("[ab ]", " ", true)>]
    [<DataRow("a{3}", "aa", false)>]
    [<DataRow("a{3}", "aaa", true)>]
    [<DataRow("a{2,}", "", false)>]
    [<DataRow("a{2,}", "a", false)>]
    [<DataRow("a{2,}", "aa", true)>]
    [<DataRow("a{2,}", "aaa", true)>]
    [<DataRow("(ab){1,2}", "", false)>]
    [<DataRow("(ab){1,2}", "ab", true)>]
    [<DataRow("(ab){1,2}", "abab", true)>]
    [<DataRow("^(ab){1,2}$", "ababab", false)>]
    [<DataRow("(ab){1,2}", "bb", false)>]
    [<DataRow("a{2}{3}", "aaaaaa", true)>]
    [<DataRow("a{2}{3}", "aa", false)>]
    [<DataRow("a{10}", "aaaaaaaaaa", true)>]
    [<DataRow("", "a", false)>]
    [<DataRow("", "", true)>]
    [<DataRow("^\\e$", "", true)>]
    [<DataRow("^\\e$", "e", false)>]
    [<DataRow("(\\e|a)b", "b", true)>]
    [<DataRow("(\\e|a)b", "ab", true)>]
    [<DataRow("(^x|y)", "xz", true)>]
    [<DataRow("(^x|y)", "zy", true)>]
    [<DataRow("(^x|y)", "zx", false)>]
    [<DataRow("$ab", "ab", false)>]
    [<DataRow("ab^", "ab", false)>]
    member this.TestRegexMatching (regex : string, input : string, expected : bool) =
        let r = Regex.Parse(regex);
        let a = r.ToAutomaton();
        let bytes = input.ToCharArray();
        Assert.AreEqual(expected, r.IsMatch(bytes));
        Assert.AreEqual(expected, a.IsMatch(bytes));

        let r2 = Regex.Parse(regex);
        let a2 = r2.ToAutomaton();
        let bytes2 = input.ToCharArray();
        Assert.AreEqual(expected, r2.IsMatch(bytes2));
        Assert.AreEqual(expected, a2.IsMatch(bytes2));

        let r3 = Regex.Parse(regex);
        let a3 = r3.ToAutomaton();
        let bytes3 = input.ToCharArray();
        Assert.AreEqual(expected, r3.IsMatch(bytes3));
        Assert.AreEqual(expected, a3.IsMatch(bytes3));

    /// Test that moving a set of states is working as expected.
    [<TestMethod>]
    member this.TestMoveStates1 () =
        let digit = Regex.Parse("^[0-9]+$")
        let a = digit.ToAutomaton()
        let result = a.MoveState(a.InitialState, '0')
        Assert.IsTrue(a.FinalStates.Contains(result))

    /// Test that moving a set of states is working as expected.
    [<TestMethod>]
    member this.TestMoveStates2 () =
        let digit = Regex.Parse("^[0-9]+$")
        let a = digit.ToAutomaton()

        let result = a.MoveStates(new List<Regex>([a.InitialState]), '1')
        Assert.IsTrue(HashSet<Regex>(result).SetEquals(a.FinalStates))

        let result = a.MoveStates(new List<Regex>([a.InitialState]), 'a')
        Assert.IsTrue(result.Count = 1)
        Assert.IsFalse(HashSet<Regex>(result).SetEquals(a.FinalStates))

    /// Test that moving a set of states is working as expected.
    [<TestMethod>]
    member this.TestMoveStates3 () =
        let digit = Regex.Parse("^[0-9]+$")
        let a = digit.ToAutomaton()
        let result = a.MoveStates(List(a.FinalStates), '1')
        Assert.IsTrue(HashSet<Regex>(result).SetEquals(a.FinalStates))

    /// Test that moving a set of states is working as expected.
    [<TestMethod>]
    member this.TestMoveStates4 () =
        let digit = Regex.Parse("^[0-9]+$")
        let a = digit.ToAutomaton()

        let result = a.MoveStates(new List<Regex>([a.InitialState]), a)
        Assert.IsTrue(HashSet<Regex>(result).SetEquals(a.FinalStates))

        let result = a.MoveStates(List(a.FinalStates), a)
        Assert.IsTrue(HashSet<Regex>(result).SetEquals(a.FinalStates))

    /// Test that moving a set of states is working as expected.
    [<TestMethod>]
    member this.TestMoveStates5 () =
        let digit = Regex.Parse("^[0-9]+$")
        let lower = Regex.Parse("^[a-z]+$")
        let a1 = digit.ToAutomaton()
        let a2 = lower.ToAutomaton()

        let result = a1.MoveStates(new List<Regex>([a1.InitialState]), a2)
        Assert.IsTrue(result.Count = 1)
        Assert.IsFalse(result.Contains(a1.InitialState))
        Assert.IsFalse(HashSet<Regex>(result).SetEquals(a1.FinalStates))

        let result = a1.MoveStates(List(a1.FinalStates), a2)
        Assert.IsTrue(result.Count = 1)
        Assert.IsFalse(result.Contains(a1.InitialState))
        Assert.IsFalse(HashSet<Regex>(result).SetEquals(a1.FinalStates))

    /// Test that moving a set of states is working as expected.
    [<TestMethod>]
    member this.TestMoveStates6 () =
        let digit = Regex.Parse("^[0-9]+$")
        let any = Regex.All()
        let a1 = digit.ToAutomaton()
        let a2 = any.ToAutomaton()

        let trashState = a1.States |> Seq.find (fun s -> (not (a1.FinalStates.Contains(s))) && s <> a1.InitialState)

        let result = a1.MoveStates(new List<Regex>([trashState]), a2)
        Assert.IsTrue(result.Count = 1)
        Assert.IsTrue(result[0] = trashState)
namespace Diffy

open System.Collections.Generic
open System.Text.Json

/// Tree metadata common to all nodes.
[<Struct>]
type TreeMetadata = {

    /// Which json configuration this node belongs to.
    Index : int

    /// A semi-unique hash value identifying the tree.
    Hash : uint64

    /// Hashes for all sub trees.
    Hashes : uint64[]
}

/// A general tree for data formats like JSON, XML, and more.
type Tree =
    | Null of TreeMetadata
    | Leaf of TreeMetadata * string * System.Type
    | List of TreeMetadata * Tree[]
    | Record of TreeMetadata * (string * Tree)[]


/// A module with tree functions.
module Tree =

    /// Gets the tree index.
    let inline Metadata (this : Tree) : TreeMetadata = 
        match this with
        | Null m
        | Leaf (m, _, _)
        | List (m, _)
        | Record (m, _) -> m

    /// Gets the tree index.
    let Index (this : Tree) : int =  (Metadata this).Index

    /// Gets the hash from a tree.
    let Hash (this : Tree) : uint64 = (Metadata this).Hash

    /// Gets the hash from a tree.
    let Hashes (this : Tree) : uint64[] = (Metadata this).Hashes

    /// Create a null node.
    let Null = Tree.Null { Index = -1; Hash = 0UL; Hashes = [|0UL|] }

    /// Create a leaf.
    let Leaf (value : string, dataType : System.Type) : Tree =
        let h = (uint64)(hash value)
        Tree.Leaf({ Index = -1; Hash = h; Hashes = [|h|]}, value, dataType)

    /// Create a leaf with an index.
    let LeafIndex (index : int, value : string, dataType : System.Type) : Tree =
        let h = (uint64)(hash value)
        Tree.Leaf({ Index = index; Hash = h; Hashes = [|h|]}, value, dataType)

    /// Create a list.
    let List (list : Tree[]) : Tree =
        let mutable h = 16777619UL
        let hashes = System.Collections.Generic.List()
        for l in list do
            h <- Common.PerfectHash h (Hash l)
            for hash in Hashes l do
                hashes.Add(hash) |> ignore
        hashes.Add(h) |> ignore
        let hashes = Array.ofSeq hashes
        Array.sortInPlace hashes
        Tree.List({ Index = -1; Hash = h; Hashes = hashes}, list)

    /// Create a record.
    let Record (records : (string * Tree)[]) : Tree =
        let mutable h = 1099511628211UL
        let hashes = System.Collections.Generic.List()
        for (k, v) in records do
            h <- Common.PerfectHash h ((uint64)(hash k))
            h <- Common.PerfectHash h (Hash v)
            for hash in Hashes v do
                hashes.Add(hash) |> ignore
        hashes.Add(h) |> ignore
        let hashes = Array.ofSeq hashes
        Array.sortInPlace hashes
        Tree.Record({ Index = -1; Hash = h; Hashes = hashes}, records)

    /// Checks if this tree is a null value.
    let IsNull (this : Tree) = match this with Null _ -> true | _ -> false

    /// Checks if this tree is a leaf.
    let IsLeaf (this : Tree) = match this with Leaf _ -> true | _ -> false

    /// Checks if this tree is a list.
    let IsList (this : Tree) = match this with List _ -> true | _ -> false

    /// Checks if this tree is a record.
    let IsRecord (this : Tree) = match this with Record _ -> true | _ -> false

    /// Gets the leaf value from a leaf tree type.
    let GetLeaf (this : Tree) : string =
        match this with Leaf (_, s, _) -> s | _ -> failwith "failed in get_leaf"
    
    /// Gets the list value from a list tree type.
    let GetList (this : Tree) : Tree[] =
        match this with List (_, l) -> l | _ -> failwith "failed in get_list"

    /// Gets the record value from a record tree type.
    let GetRecord (this : Tree) : (string * Tree)[] =
        match this with Record (_, r) -> r | _ -> failwith "failed in get_record"

    /// Create a new tree with an index assigned for each element of the tree.
    let rec WithIndex (i : int) (t : Tree) =
        match t with
        | Null meta -> Tree.Null {meta with Index = i}
        | Leaf(meta, l, typ) -> Tree.Leaf({meta with Index = i}, l, typ)
        | List(meta, l) -> Tree.List({meta with Index = i}, Array.map (WithIndex i) l)
        | Record(meta, r) -> Tree.Record({meta with Index = i}, FSharp.Collections.Array.map (fun (k, v) -> (k, WithIndex i v)) r)

    /// Converts a json string into our tree format.
    let FromJson(s : string) : Tree =
        let rec json_element_to_tree (n : System.Text.Json.JsonElement) : Tree =
            match n.ValueKind with 
            | JsonValueKind.String -> Leaf(n.GetString(), typeof<string>)
            | JsonValueKind.False -> Leaf("false", typeof<bool>)
            | JsonValueKind.True -> Leaf("true", typeof<bool>)
            | JsonValueKind.Number -> Leaf(string n, typeof<int>)
            | JsonValueKind.Null -> Null
            | JsonValueKind.Array -> List [| for elt in n.EnumerateArray() do json_element_to_tree elt |]
            | JsonValueKind.Object -> Record [| for property in n.EnumerateObject() do (property.Name, json_element_to_tree property.Value) |]
            | _ -> failwith "undefined json element"
        let mutable options = new JsonDocumentOptions()
        options.AllowTrailingCommas <- true
        options.CommentHandling <- JsonCommentHandling.Skip
        let json = System.Text.Json.JsonDocument.Parse(s, options)
        json_element_to_tree json.RootElement

    /// Returns an approximation of the similarity of two trees
    /// based on how many shared substructures they have.
    let ApproximateSimilarity (t1 : Tree) (t2 : Tree) : double =
        let hashes1 = Hashes t1
        let hashes2 = Hashes t2
        let total = (double)(hashes1.Length + hashes2.Length)
        let mutable common = 0.0
        let mutable i = 0
        let mutable j = 0
        while i < hashes1.Length || j < hashes2.Length do
            if i = hashes1.Length then j <- hashes2.Length
            elif j = hashes2.Length then i <- hashes1.Length
            else
                let h1 = hashes1[i]
                let h2 = hashes2[j]
                if h1 = h2 then
                    common <- common + 2.0
                    i <- i + 1
                    j <- j + 1
                elif h1 < h2 then
                    i <- i + 1
                else
                    j <- j + 1
        1.0 - (common / total)
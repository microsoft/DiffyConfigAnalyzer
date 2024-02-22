namespace Diffy

open System.Collections.Generic

/// A module dedicated to utility functions.
module DataStructures = 
    
    /// A custom fast bitset datatype.
    type BitSet(bitarray : System.Collections.BitArray) =

        /// Create a bitset from a size.
        new(size : int) =
            let ba = System.Collections.BitArray(size, false)
            BitSet(ba)

        /// Create a bitset from a sequence.
        static member OfSeq(size : int, s : seq<int>) : BitSet =
            let result = BitSet(size)
            for elt in s do
                result.Add(elt)
            result

        /// The underlying bitarray.
        member this.BitArray = bitarray

        /// Get the number of elements set in the bitarray.
        member this.Count() =
            let mutable count = 0
            for i = 0 to bitarray.Count - 1 do
                if this.Contains(i) then
                    count <- count + 1
            count

        /// Add an element to the set.
        member this.Add(x : int) = bitarray.Set(x, true)

        /// Remove an element from the set.
        member this.Remove(x : int) = bitarray.Set(x, false)

        /// Remove an element from the set.
        member this.Contains(x : int) = bitarray.Get(x)

        /// Intersect this bitset with another.
        member this.Intersect(other : BitSet) : BitSet =
            let result = System.Collections.BitArray(bitarray)
            let result = result.And(other.BitArray)
            BitSet(result)

        /// Union this bitset with another.
        member this.Union(other : BitSet) : BitSet =
            let result = System.Collections.BitArray(bitarray)
            let result = result.Or(other.BitArray)
            BitSet(result)

        /// Difference this bitset with another.
        member this.Difference(other : BitSet) : BitSet =
            let other = System.Collections.BitArray(other.BitArray)
            let result = System.Collections.BitArray(bitarray)
            let result = result.And(other.Not())
            BitSet(result)

        /// The get enumerator method.
        member this.GetEnumerator() : IEnumerator<int> =
            let s = seq { 
                for i = 0 to bitarray.Count - 1 do
                    if bitarray.Get(i) then
                        yield i
            }
            s.GetEnumerator()

        /// The IEnumerable interface.
        interface System.Collections.IEnumerable with
            member this.GetEnumerator() = this.GetEnumerator()
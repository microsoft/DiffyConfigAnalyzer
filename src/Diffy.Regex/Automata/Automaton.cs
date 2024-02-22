// <copyright file="Automaton.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics.CodeAnalysis;
    using System.Linq;

    /// <summary>
    /// A deterministic automaton with respect to a character type T.
    /// </summary>
    /// <typeparam name="T">The type of characters for the automaton.</typeparam>
    public class Automaton
    {
        /// <summary>
        /// The initial state of the automaton.
        /// </summary>
        public Regex InitialState { get; }

        /// <summary>
        /// The states of the automaton.
        /// </summary>
        public ISet<Regex> States { get; }

        /// <summary>
        /// The final states of the automaton.
        /// </summary>
        public ISet<Regex> FinalStates { get; }

        /// <summary>
        /// The transitions as an array for faster enumeration.
        /// </summary>
        public Dictionary<Regex, List<(CharRange, Regex)>> Transitions { get; }

        /// <summary>
        /// Creates a new instance of the <see cref="Automaton"/> class.
        /// </summary>
        /// <param name="initialState">The initial automaton state.</param>
        public Automaton(Regex initialState)
        {
            this.InitialState = initialState;
            this.Transitions = new Dictionary<Regex, List<(CharRange, Regex)>>();
            this.States = new HashSet<Regex>();
            this.FinalStates = new HashSet<Regex>();
        }

        /// <summary>
        /// Add a transition to the automaton.
        /// </summary>
        /// <param name="sourceState">The source state.</param>
        /// <param name="targetState">The target state.</param>
        /// <param name="characterClass">The character class.</param>
        public void AddTransition(Regex sourceState, Regex targetState, CharRange characterClass)
        {
            if (!this.Transitions.TryGetValue(sourceState, out var pairs))
            {
                pairs = new List<(CharRange, Regex)>();
                this.Transitions[sourceState] = pairs;
            }

            pairs.Add((characterClass, targetState));
        }

        /// <summary>
        /// Determines if this automaton matches a sequence.
        /// </summary>
        /// <param name="sequence">The sequence.</param>
        /// <returns>True if the sequence leads to an accepting state.</returns>
        public bool IsMatch(IEnumerable<char> sequence)
        {
            var state = this.InitialState;
            foreach (var item in sequence)
            {
                state = this.MoveState(state, item);
            }

            return this.FinalStates.Contains(state);
        }

        /// <summary>
        /// Move from one state to another given a character value.
        /// </summary>
        /// <param name="state">The current state.</param>
        /// <param name="value">The character value.</param>
        /// <returns></returns>
        public Regex MoveState(Regex state, char value)
        {
            foreach (var (charRange, newState) in this.Transitions[state])
            {
                if (charRange.Contains(value))
                {
                    return newState;
                }
            }

            throw new UnreachableException();
        }

        /// <summary>
        /// Move from a set of states to others given a character value.
        /// </summary>
        /// <param name="state">The current state.</param>
        /// <param name="value">The character value.</param>
        /// <returns></returns>
        public List<Regex> MoveStates(List<Regex> states, char value)
        {
            var newStates = new List<Regex>(states.Count);
            foreach (var state in states)
            {
                var newState = this.MoveState(state, value);
                if (!newStates.Contains(newState))
                {
                    newStates.Add(newState);
                }
            }

            return newStates;
        }

        /// <summary>
        /// Enumerates the intersection of all possible transitions from a set of states.
        /// </summary>
        private List<(CharRange, List<Regex>)> CrossProduct(CharRange range, Regex[] states)
        {
            var nextStates = new Regex[states.Length];
            var results = new List<(CharRange, List<Regex>)>();
            CrossProduct(range, states, 0, nextStates, results);
            return results;
        }

        /// <summary>
        /// Helper function for enumerating transition combinations.
        /// </summary>
        private void CrossProduct(CharRange range, Regex[] states, int i, Regex[] nextStates, List<(CharRange, List<Regex>)> results)
        {
            if (i >= states.Length)
            {
                results.Add((range, new List<Regex>(new HashSet<Regex>(nextStates))));
                return;
            }

            var state = states[i];
            foreach (var transition in this.Transitions[state])
            {
                var inter = range.Intersect(transition.Item1);
                if (inter.IsEmpty())
                {
                    continue;
                }

                nextStates[i] = transition.Item2;
                CrossProduct(inter, states, i + 1, nextStates, results);
            }
        }

        /// <summary>
        /// Custom comparer for tracking sets of states.
        /// </summary>
        internal class SetComparer : IEqualityComparer<(Regex, List<Regex>)>
        {
            /// <summary>
            /// Equality for sets of states.
            /// </summary>
            public bool Equals((Regex, List<Regex>) x, (Regex, List<Regex>) y)
            {
                if (x.Item1 != y.Item1)
                {
                    return false;
                }

                if (x.Item2.Count != y.Item2.Count)
                {
                    return false;
                }

                foreach (var elt in x.Item2)
                {
                    if (!y.Item2.Contains(elt))
                    {
                        return false;
                    }
                }

                return true;
            }

            /// <summary>
            /// Hashcode for sets of states.
            /// </summary>
            public int GetHashCode((Regex, List<Regex>) x)
            {
                var hash = x.Item1.GetHashCode();
                foreach (var r in x.Item2)
                {
                    hash = 31 * hash + r.GetHashCode();
                }

                return hash;
            }
        }

        /// <summary>
        /// Custom comparator for tracking the atomic ranges and the corresponding transitions.
        /// </summary>
        Comparison<(char, char, Regex, Regex, bool)> compareByRange = (x, y) =>
        {
            if (x.Item1 < y.Item1)
            {
                return -1;
            }
            if (x.Item1 > y.Item1)
            {
                return 1;
            }

            return x.Item2 - y.Item2;
        };

        /// <summary>
        /// Helper function computing atomic ranges to compute the next state combinations.
        /// </summary>
        public List<(Regex, List<Regex>)> NextStateCombinationsHelper(List<(char, char, Regex, Regex, bool)> ranges)
        {
            var currentRanges = new List<CharRange> { new CharRange(ranges[0].Item1, ranges[0].Item2) };
            var nextStates = new Dictionary<CharRange, List<(Regex, Regex, bool)>> {
                { currentRanges[0], new List<(Regex, Regex, bool)> {(ranges[0].Item3, ranges[0].Item4, ranges[0].Item5)} }
            };
            for (int i = 1; i < ranges.Count; i++)
            {
                var toRemoveIndicies = new List<int>();
                var myCharRange = new CharRange(ranges[i].Item1, ranges[i].Item2);
                for (int j = 0; j < currentRanges.Count; j++)
                {
                    var intersect = currentRanges[j].Intersect(myCharRange);
                    if (!intersect.IsEmpty())
                    {
                        break;

                    }
                    toRemoveIndicies.Add(j);
                }
                foreach (var k in toRemoveIndicies)
                {
                    currentRanges.RemoveAt(k);
                }
                for (int k = 0; k < currentRanges.Count; k++)
                {
                    var common = myCharRange.Intersect(currentRanges[k]);
                    if (common.IsEmpty())
                    {
                        break;
                    }
                    if (currentRanges[k].High <= myCharRange.High)
                    {
                        if (nextStates.ContainsKey(currentRanges[k]))
                        {
                            nextStates[currentRanges[k]].Add((ranges[i].Item3, ranges[i].Item4, ranges[i].Item5));
                        }
                        else
                        {
                            throw new UnreachableException();
                        }
                    }
                    else
                    {
                        var small1 = new CharRange(currentRanges[k].Low, myCharRange.High);
                        var small2 = new CharRange((char)(myCharRange.High + 1), currentRanges[k].High);
                        nextStates.Add(small1, nextStates[currentRanges[k]]);
                        nextStates.Add(small2, new List<(Regex, Regex, bool)>(nextStates[currentRanges[k]]));
                        nextStates[small1].Add((ranges[i].Item3, ranges[i].Item4, ranges[i].Item5));
                        nextStates.Remove(currentRanges[k]);
                        currentRanges.Insert(k + 1, small1);
                        currentRanges.Insert(k + 2, small2);
                        currentRanges.RemoveAt(k);
                        break;
                    }

                }
                var remainingRangeStart = ranges[i].Item1;
                var skip = false;
                if (currentRanges.Count > 0)
                {
                    if (currentRanges.Last().High == char.MaxValue)
                    {
                        skip = true;
                    }
                    remainingRangeStart = (char)(currentRanges.Last().High + 1);
                }
                if (!skip && remainingRangeStart <= ranges[i].Item2)
                {
                    var newAtomicRange = new CharRange(remainingRangeStart, ranges[i].Item2);
                    var tmp = new List<(Regex, Regex, bool)> { (ranges[i].Item3, ranges[i].Item4, ranges[i].Item5) };
                    currentRanges.Add(newAtomicRange);
                    nextStates.Add(newAtomicRange, tmp);
                }
            }
            var newStateCombinations = new List<(Regex, List<Regex>)>();
            foreach (var pair in nextStates)
            {
                Regex automataNextState = null;
                var myNextStates = new List<Regex>();
                foreach (var tuple in pair.Value)
                {
                    if (tuple.Item3)
                    {
                        automataNextState = tuple.Item1;
                    }
                    else
                    {
                        myNextStates.Add(tuple.Item1);
                    }
                }
                if (automataNextState == null)
                {
                    throw new UnreachableException();
                }
                newStateCombinations.Add((automataNextState, myNextStates));
            }
            return newStateCombinations;
        }

        /// <summary>
        /// Computes the next state combinations using sorting and by computing atomic ranges.
        /// </summary>
        public List<(Regex, List<Regex>)> NextStateCombinationsAtomic(Regex automatonState, Automaton automaton, List<Regex> myStates)
        {
            var atomic = new List<(char, char, Regex, Regex, bool)>();
            foreach (var transition1 in automaton.Transitions[automatonState])
            {
                atomic.Add((transition1.Item1.Low, transition1.Item1.High, transition1.Item2, automatonState, true));
            }
            foreach (var mystate in myStates)
            {
                foreach (var mytransition in this.Transitions[mystate])
                {
                    atomic.Add((mytransition.Item1.Low, mytransition.Item1.High, mytransition.Item2, mystate, false));
                }
            }
            atomic.Sort(compareByRange);
            return NextStateCombinationsHelper(atomic);
        }

        /// <summary>
        /// Computes the next state combinations using n-way product.
        /// </summary>
        public List<(Regex, List<Regex>)> NextStateCombinations(Regex automatonState, Automaton automaton, List<Regex> myStates)
        {
            var nextStateCombinations = new List<(Regex, List<Regex>)>();
            foreach (var transition1 in automaton.Transitions[automatonState])
            {
                var validTransitions = CrossProduct(transition1.Item1, myStates.ToArray());
                foreach (var (_, nextStates) in validTransitions)
                {
                    nextStateCombinations.Add((transition1.Item2, nextStates));
                }
            }
            return nextStateCombinations;
        }

        /// <summary>
        /// Move a set of states given a string that matches another automaton.
        /// </summary>
        /// <param name="state">The current state.</param>
        /// <param name="value">The automaton that was matched.</param>
        /// <returns>The possible next states.</returns>
        public List<Regex> MoveStates(List<Regex> states, Automaton automaton)
        {
            var newStates = new List<Regex>(this.States.Count);

            var seen = new HashSet<(Regex, List<Regex>)>(new SetComparer());
            var queue = new Queue<(Regex, List<Regex>)>();
            var initial = (automaton.InitialState, states);
            seen.Add(initial);
            queue.Enqueue(initial);

            while (queue.Count > 0)
            {
                var (state1, states2) = queue.Dequeue();

                var newStateCombinations = NextStateCombinationsAtomic(state1, automaton, states2);
                foreach (var (automataNextState, nextStates) in newStateCombinations)
                {
                    var pair = (automataNextState, nextStates);
                    if (automaton.FinalStates.Contains(automataNextState))
                    {
                        foreach (var next in nextStates)
                        {
                            if (!newStates.Contains(next))
                            {
                                newStates.Add(next);
                            }
                        }
                    }
                    if (!seen.Contains(pair))
                    {
                        seen.Add(pair);
                        queue.Enqueue(pair);
                    }
                }
            }

            return newStates;
        }

        /// <summary>
        /// Determines if this automaton does not accept any string using BFS
        /// </summary>
        public bool IsEmpty()
        {
            if (this.FinalStates.Count == 0) return true;
            var initial = this.InitialState;
            Queue<Regex> queue = new Queue<Regex>();
            queue.Enqueue(initial);
            var visited = new HashSet<Regex>();
            while (queue.Count > 0)
            {
                var curr = queue.Dequeue();
                if (this.FinalStates.Contains(curr))
                {
                    return false;
                }
                foreach (var x in this.Transitions[curr])
                {
                    if (!visited.Contains(x.Item2))
                    {
                        visited.Add(x.Item2);
                        queue.Enqueue(x.Item2);
                    }
                }
            }
            return true;
        }

        /// <summary>
        /// Convert the automaton to a string.
        /// </summary>
        /// <returns>The automaton as a string.</returns>
        [ExcludeFromCodeCoverage]
        public override string ToString()
        {
            var states = "{" + string.Join(",", this.States.Select(x => x.Id)) + "}";
            var finalStates = "{" + string.Join(",", this.FinalStates.Select(x => x.Id)) + "}";

            var transitions = new List<string>();
            foreach (var kv in this.Transitions)
            {
                foreach (var (cr, targetState) in kv.Value)
                {
                    transitions.Add($"({kv.Key.Id},{cr}) -> {targetState.Id}");
                }
            }
            var transitionString = "{" + string.Join(", ", transitions) + "}";

            return $"Automaton(init={this.InitialState.Id}, states={states}, final={finalStates}, transitions={transitionString})";
        }
    }
}
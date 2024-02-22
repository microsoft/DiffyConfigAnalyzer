// <copyright file="Regex.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System;
    using System.Collections.Generic;
    using System.Collections.Immutable;
    using System.Linq;
    using System.Threading;

    /// <summary>
    /// A regex object parameterized over a C# character type.
    /// </summary>
    /// <typeparam name="T">The type of characters for the regex.</typeparam>
    public abstract class Regex
    {
        /// <summary>
        /// The next unique id.
        /// </summary>
        private static long nextId = 0;

        /// <summary>
        /// The unique id for the given Zen expression.
        /// </summary>
        public long Id = Interlocked.Increment(ref nextId);

        /// <summary>
        /// Accept a visitor for the ZenExpr object.
        /// </summary>
        /// <returns>A value of the return type.</returns>
        internal abstract TReturn Accept<TParam, TReturn>(IRegexExprVisitor<TParam, TReturn> visitor, TParam parameter);

        /// <summary>
        /// Determines if a regular expression accepts 'Epsilon'.
        /// </summary>
        /// <returns>True if the regex accepts the empty sequence.</returns>
        public bool IsNullable()
        {
            var visitor = new RegexNullableVisitor();
            return visitor.Compute(this).Equals(Regex.Epsilon());
        }

        /// <summary>
        /// Computes the character classes for the regex for the next character.
        /// </summary>
        /// <returns>True if the regex accepts the empty sequence.</returns>
        public ImmutableHashSet<CharRange> CharacterClasses()
        {
            var visitor = new RegexCharacterClassVisitor();
            return visitor.Compute(this);
        }

        /// <summary>
        /// Computes the derivative of a regex with respect to a character.
        /// </summary>
        /// <param name="value">The character value.</param>
        /// <returns>True if the regex accepts the empty sequence.</returns>
        public Regex Derivative(char value)
        {
            var visitor = new RegexDerivativeVisitor();
            return visitor.Compute(this, value);
        }

        /// <summary>
        /// Checks if a Regex matches a sequence.
        /// </summary>
        /// <param name="sequence">The sequence of characters.</param>
        /// <returns>True if the sequence is in the language of the Regex.</returns>
        public bool IsMatch(IEnumerable<char> sequence)
        {
            var regex = this;
            foreach (var item in sequence)
            {
                regex = regex.Derivative(item);
            }

            return regex.IsNullable();
        }

        /// <summary>
        /// Convert this regex to a deterministic automaton.
        /// </summary>
        /// <returns>A determinisic automaton.</returns>
        public Automaton ToAutomaton()
        {
            var automaton = new Automaton(this);

            automaton.States.Add(this);
            var states = new List<Regex> { this };

            for (int i = 0; i < states.Count; i++)
            {
                var state = states[i];
                foreach (var characterClass in state.CharacterClasses())
                {
                    var character = characterClass.Low;
                    var newState = state.Derivative(character);
                    automaton.AddTransition(state, newState, characterClass);
                    if (!automaton.States.Contains(newState))
                    {
                        states.Add(newState);
                        automaton.States.Add(newState);
                    }
                }
            }

            foreach (var state in states)
            {
                if (state.IsNullable())
                {
                    automaton.FinalStates.Add(state);
                }
            }

            return automaton;
        }

        /// <summary>
        /// Reference equality for Zen objects.
        /// </summary>
        /// <param name="obj">Other object.</param>
        /// <returns>True or false.</returns>
        public override bool Equals(object obj)
        {
            return base.Equals(obj);
        }

        /// <summary>
        /// Hash code for a Zen object.
        /// </summary>
        /// <returns>Hash code.</returns>
        public override int GetHashCode()
        {
            return base.GetHashCode();
        }

        /// <summary>
        /// Parse a regex from a unicode string describing the pattern.
        /// </summary>
        /// <param name="regex">The regex pattern.</param>
        /// <returns>A regex recognizing bytes.</returns>
        public static Regex Parse(string regex)
        {
            return new RegexParser(regex).Parse();
        }

        /// <summary>
        /// The 'Empty' regular expression.
        /// </summary>
        /// <returns>A regular expression that accepts no strings.</returns>
        public static Regex Empty()
        {
            return RegexEmptyExpr.Instance;
        }

        /// <summary>
        /// The 'Dot' regular expression for any single character.
        /// </summary>
        /// <returns>A regular expression that accepts any single character.</returns>
        public static Regex Dot()
        {
            return Regex.Range(char.MinValue, char.MaxValue);
        }

        /// <summary>
        /// The 'Optional' regular expression.
        /// </summary>
        /// <returns>A regular expression matches zero or one occurance of another.</returns>
        public static Regex Opt(Regex expr)
        {
            return Regex.Union(Regex.Epsilon(), expr);
        }

        /// <summary>
        /// The 'All' regular expression.
        /// </summary>
        /// <returns>A regular expression that accepts all strings.</returns>
        public static Regex All()
        {
            return Regex.Negation(Regex.Empty());
        }

        /// <summary>
        /// The 'Epsilon' regular expression.
        /// </summary>
        /// <returns>A regular expression that accepts a single empty string.</returns>
        public static Regex Epsilon()
        {
            return RegexEpsilonExpr.Instance;
        }

        /// <summary>
        /// The 'AnchorBegin' regular expression.
        /// </summary>
        /// <returns>A regular expression for the begin anchor.</returns>
        public static Regex AnchorBegin()
        {
            return RegexAnchorExpr.BeginInstance;
        }

        /// <summary>
        /// The 'AnchorEnd' regular expression.
        /// </summary>
        /// <returns>A regular expression for the end anchor.</returns>
        public static Regex AnchorEnd()
        {
            return RegexAnchorExpr.EndInstance;
        }

        /// <summary>
        /// The 'Range' regular expression.
        /// </summary>
        /// <param name="low">The low character value.</param>
        /// <param name="high">The high character value.</param>
        /// <returns>A regular expression that accepts a single character.</returns>
        public static Regex Range(char low, char high)
        {
            return RegexRangeExpr.Create(low, high);
        }

        /// <summary>
        /// The 'Char' regular expression.
        /// </summary>
        /// <param name="value">The character value.</param>
        /// <returns>A regular expression that accepts a single character.</returns>
        public static Regex Char(char value)
        {
            return RegexRangeExpr.Create(value, value);
        }

        /// <summary>
        /// The 'Union' regular expression.
        /// </summary>
        /// <param name="expr1">The first Regex expr.</param>
        /// <param name="expr2">The second Regex expr.</param>
        /// <returns>A regular expression that accepts the union of two others.</returns>
        public static Regex Union(Regex expr1, Regex expr2)
        {
            return RegexBinopExpr.Create(expr1, expr2, RegexBinopExprType.Union);
        }

        /// <summary>
        /// The 'Intersect' regular expression.
        /// </summary>
        /// <param name="expr1">The first Regex expr.</param>
        /// <param name="expr2">The second Regex expr.</param>
        /// <returns>A regular expression that accepts the intersection of two others.</returns>
        public static Regex Intersect(Regex expr1, Regex expr2)
        {
            return RegexBinopExpr.Create(expr1, expr2, RegexBinopExprType.Intersection);
        }

        /// <summary>
        /// The 'Concat' regular expression.
        /// </summary>
        /// <param name="expr1">The first Regex expr.</param>
        /// <param name="expr2">The second Regex expr.</param>
        /// <returns>A regular expression that accepts the concatenation two others.</returns>
        public static Regex Concat(Regex expr1, Regex expr2)
        {
            return RegexBinopExpr.Create(expr1, expr2, RegexBinopExprType.Concatenation);
        }

        /// <summary>
        /// The 'Star' regular expression.
        /// </summary>
        /// <param name="expr">The Regex expr.</param>
        /// <returns>A regular expression that accepts zero or more iterations of another.</returns>
        public static Regex Star(Regex expr)
        {
            return RegexUnopExpr.Create(expr, RegexUnopExprType.Star);
        }

        /// <summary>
        /// The 'Negation' regular expression.
        /// </summary>
        /// <param name="expr">The Regex expr.</param>
        /// <returns>A regular expression that accepts any strings another doesn't.</returns>
        public static Regex Negation(Regex expr)
        {
            return RegexUnopExpr.Create(expr, RegexUnopExprType.Negation);
        }

        /// <summary>
        /// The 'Repeat' regular expression.
        /// </summary>
        /// <param name="expr">The Regex expr.</param>
        /// <param name="times">The number of times to repeat it.</param>
        /// <returns>A regular expression matches zero or one occurance of another.</returns>
        public static Regex Repeat(Regex expr, int times)
        {
            return Repeat(expr, times, times);
        }

        /// <summary>
        /// The 'Repeat' regular expression.
        /// </summary>
        /// <param name="expr">The Regex expr.</param>
        /// <param name="lo">The minimum number of times to match it.</param>
        /// <param name="hi">The maximum number of times to match it.</param>
        /// <returns>A regular expression repeated betwen lo and hi number of times.</returns>
        public static Regex Repeat(Regex expr, int lo, int hi)
        {
            Contract.Assert(lo >= 0, "Repeat lower bound must be non-negative");
            Contract.Assert(hi >= lo, "Repeat upper bound must not be less than lower bound.");

            var r = Regex.Epsilon();
            for (int i = 0; i < lo; i++)
            {
                r = Regex.Concat(r, expr);
            }

            for (int i = lo; i < hi; i++)
            {
                r = Regex.Concat(r, Regex.Opt(expr));
            }

            return r;
        }
    }
}
// <copyright file="RegexRangeExpr.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System;
    using System.Diagnostics.CodeAnalysis;

    /// <summary>
    /// Class representing a Regex single char expression.
    /// </summary>
    internal sealed class RegexRangeExpr : Regex
    {
        /// <summary>
        /// Static creation function for hash consing.
        /// </summary>
        private static Func<(char, char), Regex> createFunc = (v) => Simplify(v.Item1, v.Item2);

        /// <summary>
        /// Hash cons table for Regex terms.
        /// </summary>
        private static Flyweight<(char, char), Regex> hashConsTable = new Flyweight<(char, char), Regex>();

        /// <summary>
        /// Gets the first Regex expression.
        /// </summary>
        internal CharRange CharacterRange { get; }

        /// <summary>
        /// Simplify a new RegexRangeExpr.
        /// </summary>
        /// <param name="low">The low character value.</param>
        /// <param name="high">The high character value.</param>
        /// <returns>The new Regex expr.</returns>
        private static Regex Simplify(char low, char high)
        {
            var range = new CharRange(low, high);

            if (range.IsEmpty())
            {
                return RegexEmptyExpr.Instance;
            }

            return new RegexRangeExpr(range);
        }

        /// <summary>
        /// Creates a new RegexRangeExpr.
        /// </summary>
        /// <param name="low">The low character value.</param>
        /// <param name="high">The high character value.</param>
        /// <returns>The new Regex expr.</returns>
        public static Regex Create(char low, char high)
        {
            var key = (low, high);
            hashConsTable.GetOrAdd(key, key, createFunc, out var v);
            return v;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="RegexRangeExpr{T}"/> class.
        /// </summary>
        /// <param name="range">The character range.</param>
        private RegexRangeExpr(CharRange range)
        {
            this.CharacterRange = range;
        }

        /// <summary>
        /// Convert the expression to a string.
        /// </summary>
        /// <returns>The string representation.</returns>
        [ExcludeFromCodeCoverage]
        public override string ToString()
        {
            return $"Char({this.CharacterRange})";
        }

        /// <summary>
        /// Implementing the visitor interface.
        /// </summary>
        /// <param name="visitor">The visitor object.</param>
        /// <param name="parameter">The visitor parameter.</param>
        /// <typeparam name="TParam">The visitor parameter type.</typeparam>
        /// <typeparam name="TReturn">The visitor return type.</typeparam>
        /// <returns>A return value.</returns>
        internal override TReturn Accept<TParam, TReturn>(IRegexExprVisitor<TParam, TReturn> visitor, TParam parameter)
        {
            return visitor.Visit(this, parameter);
        }
    }
}
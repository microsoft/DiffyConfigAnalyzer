// <copyright file="RegexEmptyExpr.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System;
    using System.Diagnostics.CodeAnalysis;

    /// <summary>
    /// Class representing a Regex empty expression.
    /// </summary>
    internal sealed class RegexEmptyExpr : Regex
    {
        /// <summary>
        /// The empty Regex instance.
        /// </summary>
        public static Regex Instance = new RegexEmptyExpr();

        /// <summary>
        /// Initializes a new instance of the <see cref="RegexEmptyExpr{T}"/> class.
        /// </summary>
        private RegexEmptyExpr()
        {
        }

        /// <summary>
        /// Convert the expression to a string.
        /// </summary>
        /// <returns>The string representation.</returns>
        [ExcludeFromCodeCoverage]
        public override string ToString()
        {
            return $"Empty";
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
// <copyright file="RegexUnopExpr.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System;
    using System.Diagnostics.CodeAnalysis;

    /// <summary>
    /// Class representing a Regex unary operation expression.
    /// </summary>
    internal sealed class RegexUnopExpr : Regex
    {
        /// <summary>
        /// Static creation function for hash consing.
        /// </summary>
        private static Func<(Regex, RegexUnopExprType), Regex> createFunc = (v) => Simplify(v.Item1, v.Item2);

        /// <summary>
        /// Hash cons table for Regex terms.
        /// </summary>
        private static Flyweight<(long, int), Regex> hashConsTable = new Flyweight<(long, int), Regex>();

        /// <summary>
        /// Gets the first Regex expression.
        /// </summary>
        internal Regex Expr { get; }

        /// <summary>
        /// Gets the Regex operation type.
        /// </summary>
        internal RegexUnopExprType OpType { get; }

        /// <summary>
        /// Simplify a new RegexUnopExpr.
        /// </summary>
        /// <param name="expr">The Regex expr.</param>
        /// <param name="opType">The regex operation type.</param>
        /// <returns>The new Regex expr.</returns>
        private static Regex Simplify(Regex expr, RegexUnopExprType opType)
        {
            if (opType == RegexUnopExprType.Star)
            {
                // simplify (r*)* = r*
                if (expr is RegexUnopExpr x && x.OpType == RegexUnopExprType.Star)
                {
                    return expr;
                }

                // simplify \epsilon* = \epsilon
                if (expr is RegexEpsilonExpr)
                {
                    return expr;
                }

                // simplify \empty* = \epsilon
                if (expr is RegexEmptyExpr)
                {
                    return Regex.Epsilon();
                }

                // simplify .* = \not \empty
                if (expr is RegexRangeExpr r && r.CharacterRange.IsFull())
                {
                    return Regex.Negation(Regex.Empty());
                }
            }

            if (opType == RegexUnopExprType.Negation)
            {
                // simplify not(not(r)) = r
                if (expr is RegexUnopExpr y && y.OpType == RegexUnopExprType.Negation)
                {
                    return y.Expr;
                }

                // simplify not(range(min, max)) = \empty
                if (expr is RegexRangeExpr e && e.CharacterRange.IsFull())
                {
                    return Regex.Empty();
                }
            }

            return new RegexUnopExpr(expr, opType);
        }

        /// <summary>
        /// Creates a new RegexUnopExpr.
        /// </summary>
        /// <param name="expr">The Regex expr.</param>
        /// <param name="opType">The operation type.</param>
        /// <returns>The new Regex expr.</returns>
        public static Regex Create(Regex expr, RegexUnopExprType opType)
        {
            Contract.AssertNotNull(expr);

            var key = (expr.Id, (int)opType);
            hashConsTable.GetOrAdd(key, (expr, opType), createFunc, out var value);
            return value;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="RegexUnopExpr{T}"/> class.
        /// </summary>
        /// <param name="expr">The Regex expression.</param>
        /// <param name="opType">The Regex operation type.</param>
        private RegexUnopExpr(Regex expr, RegexUnopExprType opType)
        {
            this.Expr = expr;
            this.OpType = opType;
        }

        /// <summary>
        /// Convert the expression to a string.
        /// </summary>
        /// <returns>The string representation.</returns>
        [ExcludeFromCodeCoverage]
        public override string ToString()
        {
            switch (this.OpType)
            {
                case RegexUnopExprType.Star:
                    return $"Star({this.Expr})";
                case RegexUnopExprType.Negation:
                    return $"Not({this.Expr})";
                default:
                    throw new UnreachableException();
            }
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

    /// <summary>
    /// The regex binary operation type.
    /// </summary>
    internal enum RegexUnopExprType
    {
        /// <summary>
        /// A Kleene star of an expression.
        /// </summary>
        Star,

        /// <summary>
        /// The negation of a regular expression.
        /// </summary>
        Negation,
    }
}
﻿// <copyright file="RegexNullableVisitor.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System.Diagnostics.CodeAnalysis;

    /// <summary>
    /// A class to check if a regex is nullable.
    /// </summary>
    internal class RegexNullableVisitor : IRegexExprVisitor<Unit, Regex>
    {
        /// <summary>
        /// Compute whether a regular expression is nullable.
        /// </summary>
        /// <param name="regex">The regular expression.</param>
        /// <returns>A derivative as a regex.</returns>
        public Regex Compute(Regex regex)
        {
            return regex.Accept(this, Unit.Instance);
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexBinopExpr expression, Unit parameter)
        {
            var d1 = Compute(expression.Expr1);
            var d2 = Compute(expression.Expr2);
            switch (expression.OpType)
            {
                case RegexBinopExprType.Union:
                    return Regex.Union(d1, d2);
                case RegexBinopExprType.Intersection:
                    return Regex.Intersect(d1, d2);
                default:
                    Contract.Assert(expression.OpType == RegexBinopExprType.Concatenation);
                    return Regex.Intersect(d1, d2);
            }
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexRangeExpr expression, Unit parameter)
        {
            return Regex.Empty();
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexEmptyExpr expression, Unit parameter)
        {
            return Regex.Empty();
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexEpsilonExpr expression, Unit parameter)
        {
            return expression;
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexUnopExpr expression, Unit parameter)
        {
            switch (expression.OpType)
            {
                case RegexUnopExprType.Star:
                    return Regex.Epsilon();
                default:
                    Contract.Assert(expression.OpType == RegexUnopExprType.Negation);
                    return Compute(expression.Expr) is RegexEpsilonExpr ? Regex.Empty() : Regex.Epsilon();
            }
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        [ExcludeFromCodeCoverage]
        public Regex Visit(RegexAnchorExpr expression, Unit parameter)
        {
            throw new UnreachableException();
        }
    }
}
// <copyright file="RegexDerivativeVisitor.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System.Diagnostics.CodeAnalysis;
    using System.Diagnostics.Contracts;

    /// <summary>
    /// A class to compute a regex derivative.
    /// </summary>
    internal class RegexDerivativeVisitor : IRegexExprVisitor<char, Regex>
    {
        /// <summary>
        /// Visitor to check if a regex is nullable.
        /// </summary>
        private RegexNullableVisitor nullableVisitor = new RegexNullableVisitor();

        /// <summary>
        /// Compute a derivative of a regular expression.
        /// </summary>
        /// <param name="regex">The regular expression.</param>
        /// <param name="value">The value for the derivative.</param>
        /// <returns>A derivative as a regex.</returns>
        public Regex Compute(Regex regex, char value)
        {
            return regex.Accept(this, value);
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexBinopExpr expression, char parameter)
        {
            var r = expression.Expr1;
            var s = expression.Expr2;
            var dr = Compute(r, parameter);
            var ds = Compute(s, parameter);

            switch (expression.OpType)
            {
                case RegexBinopExprType.Union:
                    return Regex.Union(dr, ds);
                case RegexBinopExprType.Intersection:
                    return Regex.Intersect(dr, ds);
                default:
                    Contract.Assert(expression.OpType == RegexBinopExprType.Concatenation);
                    var left = Regex.Concat(dr, s);
                    var right = Regex.Concat(nullableVisitor.Compute(r), ds);
                    return Regex.Union(left, right);
            }
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexRangeExpr expression, char parameter)
        {
            if (expression.CharacterRange.Contains(parameter))
            {
                return Regex.Epsilon();
            }
            else
            {
                return Regex.Empty();
            }
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexEmptyExpr expression, char parameter)
        {
            return expression;
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexEpsilonExpr expression, char parameter)
        {
            return Regex.Empty();
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexUnopExpr expression, char parameter)
        {
            switch (expression.OpType)
            {
                case RegexUnopExprType.Star:
                    return Regex.Concat(Compute(expression.Expr, parameter), expression);
                default:
                    Contract.Assert(expression.OpType == RegexUnopExprType.Negation);
                    return Regex.Negation(Compute(expression.Expr, parameter));
            }
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        [ExcludeFromCodeCoverage]
        public Regex Visit(RegexAnchorExpr expression, char parameter)
        {
            throw new UnreachableException();
        }
    }
}
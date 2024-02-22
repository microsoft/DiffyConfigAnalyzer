// <copyright file="RegexRemoveAnchorVisitor.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System.Diagnostics.CodeAnalysis;
    using System.Diagnostics.Contracts;

    /// <summary>
    /// A class to remove anchors from a regular expression.
    /// </summary>
    internal class RegexRemoveAnchorVisitor : IRegexExprVisitor<(Regex, Regex), Regex>
    {
        /// <summary>
        /// Remove anchors from a regular expression.
        /// </summary>
        /// <param name="regex">The regular expression.</param>
        /// <returns>A derivative as a regex.</returns>
        public Regex Compute(Regex regex)
        {
            return regex.Accept(this, (Regex.All(), Regex.All()));
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        [ExcludeFromCodeCoverage]
        public Regex Visit(RegexEmptyExpr expression, (Regex, Regex) parameter)
        {
            return expression;
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexEpsilonExpr expression, (Regex, Regex) parameter)
        {
            return Regex.Concat(parameter.Item1, parameter.Item2);
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexAnchorExpr expression, (Regex, Regex) parameter)
        {
            return Regex.Epsilon();
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexRangeExpr expression, (Regex, Regex) parameter)
        {
            return Regex.Concat(parameter.Item1, Regex.Concat(expression, parameter.Item2));
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexUnopExpr expression, (Regex, Regex) parameter)
        {
            return Regex.Concat(parameter.Item1, Regex.Concat(expression, parameter.Item2));
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A regex.</returns>
        public Regex Visit(RegexBinopExpr expression, (Regex, Regex) parameter)
        {
            switch (expression.OpType)
            {
                case RegexBinopExprType.Union:
                    return Regex.Union(expression.Expr1.Accept(this, parameter), expression.Expr2.Accept(this, parameter));
                case RegexBinopExprType.Intersection:
                    return Regex.Concat(parameter.Item1, Regex.Concat(expression, parameter.Item2));
                default:
                    Contract.Assert(expression.OpType == RegexBinopExprType.Concatenation);
                    if (parameter.Item1.Equals(Regex.All()) && parameter.Item2.Equals(Regex.All()))
                    {
                        var param1 = (Regex.All(), Regex.Epsilon());
                        var param2 = (Regex.Epsilon(), Regex.All());
                        return Regex.Concat(expression.Expr1.Accept(this, param1), expression.Expr2.Accept(this, param2));
                    }
                    else
                    {
                        Contract.Assert(parameter.Item1.Equals(Regex.Epsilon()));
                        Contract.Assert(parameter.Item2.Equals(Regex.All()));
                        return Regex.Concat(expression.Expr1, expression.Expr2.Accept(this, parameter));
                    }
            }
        }
    }
}
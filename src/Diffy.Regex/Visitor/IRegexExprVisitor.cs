// <copyright file="IRegexExprVisitor.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    /// <summary>
    /// Visitor interface for Regex.
    /// </summary>
    /// <typeparam name="TParam">The parameter type.</typeparam>
    /// <typeparam name="TReturn">The return type.</typeparam>
    internal interface IRegexExprVisitor<TParam, TReturn>
    {
        /// <summary>
        /// Visit a RegexEmptyExpr.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A return value.</returns>
        TReturn Visit(RegexEmptyExpr expression, TParam parameter);

        /// <summary>
        /// Visit a RegexEpsilonExpr.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A return value.</returns>
        TReturn Visit(RegexEpsilonExpr expression, TParam parameter);

        /// <summary>
        /// Visit a RegexAnchorExpr.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A return value.</returns>
        TReturn Visit(RegexAnchorExpr expression, TParam parameter);

        /// <summary>
        /// Visit a RegexCharExpr.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A return value.</returns>
        TReturn Visit(RegexRangeExpr expression, TParam parameter);

        /// <summary>
        /// Visit a RegexUnopExpr.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A return value.</returns>
        TReturn Visit(RegexUnopExpr expression, TParam parameter);

        /// <summary>
        /// Visit a RegexBinopExpr.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>A return value.</returns>
        TReturn Visit(RegexBinopExpr expression, TParam parameter);
    }
}
// <copyright file="RegexBinopExpr.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System;
    using System.Diagnostics.CodeAnalysis;
    using System.Diagnostics.Contracts;

    /// <summary>
    /// Class representing a Regex binary operation expression.
    /// </summary>
    internal sealed class RegexBinopExpr : Regex
    {
        /// <summary>
        /// Static creation function for hash consing.
        /// </summary>
        private static Func<(Regex, Regex, RegexBinopExprType), Regex> createFunc = (v) => Simplify(v.Item1, v.Item2, v.Item3);

        /// <summary>
        /// Hash cons table for Regex terms.
        /// </summary>
        private static Flyweight<(long, long, int), Regex> hashConsTable = new Flyweight<(long, long, int), Regex>();

        /// <summary>
        /// Gets the first Regex expression.
        /// </summary>
        internal Regex Expr1 { get; }

        /// <summary>
        /// Gets the second Regex expression.
        /// </summary>
        internal Regex Expr2 { get; }

        /// <summary>
        /// Gets the Regex operation type.
        /// </summary>
        internal RegexBinopExprType OpType { get; }

        /// <summary>
        /// Simplify a new RegexBinopExpr.
        /// </summary>
        /// <param name="e1">The first expr.</param>
        /// <param name="e2">The second expr.</param>
        /// <param name="opType">The regex operation type.</param>
        /// <returns>The new Regex expr.</returns>
        private static Regex Simplify(Regex e1, Regex e2, RegexBinopExprType opType)
        {
            if (opType == RegexBinopExprType.Intersection)
            {
                // simplify (r & r) = r
                if (ReferenceEquals(e1, e2))
                {
                    return e1;
                }

                // simplify \empty & r = \empty
                if (e1 is RegexEmptyExpr)
                {
                    return e1;
                }

                // simplify r & \empty = \empty
                if (e2 is RegexEmptyExpr)
                {
                    return e2;
                }

                // simplify not(\empty) & r = r
                if (e1 is RegexUnopExpr x && x.OpType == RegexUnopExprType.Negation && x.Expr is RegexEmptyExpr)
                {
                    return e2;
                }

                // simplilfy r & not(\empty) = r
                if (e2 is RegexUnopExpr y && y.OpType == RegexUnopExprType.Negation && y.Expr is RegexEmptyExpr)
                {
                    return e1;
                }

                // simplify (a + (a + b)) = a + b
                if (e2 is RegexBinopExpr w && w.OpType == RegexBinopExprType.Intersection && w.Expr1.Equals(e1))
                {
                    return e2;
                }

                // simplify (r & s) & t = r & (s & t)
                if (e1 is RegexBinopExpr z && z.OpType == RegexBinopExprType.Intersection)
                {
                    return Regex.Intersect(z.Expr1, Regex.Intersect(z.Expr2, e2));
                }

                // simplify r & s = s & r when s < r
                if (e2.Id < e1.Id)
                {
                    return Regex.Intersect(e2, e1);
                }
            }

            if (opType == RegexBinopExprType.Union)
            {
                // simplify (r + r) = r
                if (ReferenceEquals(e1, e2))
                {
                    return e1;
                }

                // simplify \empty + r = r
                if (e1 is RegexEmptyExpr)
                {
                    return e2;
                }

                // simplify r + \empty = r
                if (e2 is RegexEmptyExpr)
                {
                    return e1;
                }

                // simplify (a + (a + b)) = a + b
                if (e2 is RegexBinopExpr w && w.OpType == RegexBinopExprType.Union && w.Expr1.Equals(e1))
                {
                    return e2;
                }

                // simplify not(\empty) + r = not(\empty)
                if (e1 is RegexUnopExpr x && x.OpType == RegexUnopExprType.Negation && x.Expr is RegexEmptyExpr)
                {
                    return e1;
                }

                // simplify r + not(\empty) = not(\empty)
                if (e2 is RegexUnopExpr y && y.OpType == RegexUnopExprType.Negation && y.Expr is RegexEmptyExpr)
                {
                    return e2;
                }

                // simplify [a-b] + [c-d] = [a-b] where [a-b] contains [c-d]
                if (e1 is RegexRangeExpr rng1 && e2 is RegexRangeExpr rng2 &&
                    rng1.CharacterRange.Contains(rng2.CharacterRange.Low) &&
                    rng1.CharacterRange.Contains(rng2.CharacterRange.High))
                {
                    return e1;
                }

                // simplify [a-b] + [c-d] = [c-d] where [a-b] is contained by [c-d]
                if (e1 is RegexRangeExpr rng3 && e2 is RegexRangeExpr rng4 &&
                    rng4.CharacterRange.Contains(rng3.CharacterRange.Low) &&
                    rng4.CharacterRange.Contains(rng3.CharacterRange.High))
                {
                    return e2;
                }

                // simplify (r + s) + t = r + (s + t)
                if (e1 is RegexBinopExpr z && z.OpType == RegexBinopExprType.Union)
                {
                    return Regex.Union(z.Expr1, Regex.Union(z.Expr2, e2));
                }

                // simplify r + s = s + r when s < r
                if (e2.Id < e1.Id)
                {
                    return Regex.Union(e2, e1);
                }
            }

            if (opType == RegexBinopExprType.Concatenation)
            {
                // simplify \empty . r = \empty
                if (e1 is RegexEmptyExpr || e2 is RegexEmptyExpr)
                {
                    return Regex.Empty();
                }

                // simplify \epsilon . r = r
                if (e1 is RegexEpsilonExpr)
                {
                    return e2;
                }

                // simplify r . \epsilon = r
                if (e2 is RegexEpsilonExpr)
                {
                    return e1;
                }

                // simplify $ . a = \empty
                if (e1 is RegexAnchorExpr a && !a.IsBegin && e2 is RegexRangeExpr)
                {
                    return Regex.Empty();
                }

                // simplify a . ^ = \empty
                if (e2 is RegexAnchorExpr b && b.IsBegin && e1 is RegexRangeExpr)
                {
                    return Regex.Empty();
                }

                // simplify (r . s) . t = r . (s . t)
                if (e1 is RegexBinopExpr z && z.OpType == RegexBinopExprType.Concatenation)
                {
                    return Regex.Concat(z.Expr1, Regex.Concat(z.Expr2, e2));
                }
            }

            return new RegexBinopExpr(e1, e2, opType);
        }

        /// <summary>
        /// Creates a new RegexBinopExpr.
        /// </summary>
        /// <param name="expr1">The first expr.</param>
        /// <param name="expr2">The second expr.</param>
        /// <param name="opType">The operation type.</param>
        /// <returns>The new Regex expr.</returns>
        public static Regex Create(Regex expr1, Regex expr2, RegexBinopExprType opType)
        {
            Contract.AssertNotNull(expr1);
            Contract.AssertNotNull(expr2);

            var key = (expr1.Id, expr2.Id, (int)opType);
            hashConsTable.GetOrAdd(key, (expr1, expr2, opType), createFunc, out var value);
            return value;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="RegexBinopExpr{T}"/> class.
        /// </summary>
        /// <param name="expr1">First Regex expression.</param>
        /// <param name="expr2">Second Regex expression.</param>
        /// <param name="opType">The Regex operation type.</param>
        private RegexBinopExpr(Regex expr1, Regex expr2, RegexBinopExprType opType)
        {
            this.Expr1 = expr1;
            this.Expr2 = expr2;
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
                case RegexBinopExprType.Union:
                    return $"Union({this.Expr1}, {this.Expr2})";
                case RegexBinopExprType.Intersection:
                    return $"Inter({this.Expr1}, {this.Expr2})";
                case RegexBinopExprType.Concatenation:
                    return $"Concat({this.Expr1}, {this.Expr2})";
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
    internal enum RegexBinopExprType
    {
        /// <summary>
        /// A union of two regular expressions.
        /// </summary>
        Union,

        /// <summary>
        /// An intersection of two regular expressions.
        /// </summary>
        Intersection,

        /// <summary>
        /// A concatentation of two regular expressions.
        /// </summary>
        Concatenation,
    }
}
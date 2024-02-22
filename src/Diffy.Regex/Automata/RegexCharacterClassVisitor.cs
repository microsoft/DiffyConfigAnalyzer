// <copyright file="RegexCharacterClassVisitor.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System.Collections.Immutable;
    using System.Diagnostics.CodeAnalysis;
    using Diffy.Regex;

    /// <summary>
    /// A class to compute character equivalence classes.
    /// </summary>
    internal class RegexCharacterClassVisitor : IRegexExprVisitor<Unit, ImmutableHashSet<CharRange>>
    {
        /// <summary>
        /// Compute the character classes.
        /// </summary>
        /// <param name="regex">The regex.</param>
        /// <returns>The character classes.</returns>
        public ImmutableHashSet<CharRange> Compute(Regex regex)
        {
            return regex.Accept(this, Unit.Instance);
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>Character sets.</returns>
        public ImmutableHashSet<CharRange> Visit(RegexBinopExpr expression, Unit parameter)
        {
            if (expression.OpType == RegexBinopExprType.Concatenation && !expression.Expr1.IsNullable())
            {
                return Compute(expression.Expr1);
            }
            else
            {
                return MakeDisjoint(Compute(expression.Expr1), Compute(expression.Expr2));
            }
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>Character sets.</returns>
        public ImmutableHashSet<CharRange> Visit(RegexEmptyExpr expression, Unit parameter)
        {
            var allRange = new CharRange();
            return ImmutableHashSet<CharRange>.Empty.Add(allRange);
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>Character sets.</returns>
        public ImmutableHashSet<CharRange> Visit(RegexEpsilonExpr expression, Unit parameter)
        {
            var allRange = new CharRange();
            return ImmutableHashSet<CharRange>.Empty.Add(allRange);
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>Character sets.</returns>
        public ImmutableHashSet<CharRange> Visit(RegexRangeExpr expression, Unit parameter)
        {
            var result = ImmutableHashSet<CharRange>.Empty.Add(expression.CharacterRange);
            var complements = expression.CharacterRange.Complement();
            foreach (var complement in complements)
            {
                result = result.Add(complement);
            }

            return result;
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>Character sets.</returns>
        public ImmutableHashSet<CharRange> Visit(RegexUnopExpr expression, Unit parameter)
        {
            return Compute(expression.Expr);
        }

        /// <summary>
        /// Visit a regex.
        /// </summary>
        /// <param name="expression">The expression.</param>
        /// <param name="parameter">The parameter.</param>
        /// <returns>Character sets.</returns>
        [ExcludeFromCodeCoverage]
        public ImmutableHashSet<CharRange> Visit(RegexAnchorExpr expression, Unit parameter)
        {
            throw new UnreachableException();
        }

        /// <summary>
        /// Make two sets disjoint.
        /// </summary>
        /// <param name="set1">The first set of character classes.</param>
        /// <param name="set2">The second set of character classes.</param>
        /// <returns></returns>
        private ImmutableHashSet<CharRange> MakeDisjoint(ImmutableHashSet<CharRange> set1, ImmutableHashSet<CharRange> set2)
        {
            var result = ImmutableHashSet<CharRange>.Empty;
            foreach (var item1 in set1)
            {
                foreach (var item2 in set2)
                {
                    var inter = item1.Intersect(item2);
                    if (!inter.IsEmpty())
                    {
                        result = result.Add(inter);
                    }
                }
            }

            return result;
        }
    }
}
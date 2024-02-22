// <copyright file="CharRange.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System;
    using System.Diagnostics.CodeAnalysis;
    using System.Diagnostics.Contracts;

    /// <summary>
    /// A simple representation of a range.
    /// </summary>
    public class CharRange : IEquatable<CharRange>
    {
        /// <summary>
        /// The low value of the range.
        /// </summary>
        public char Low { get; private set; }

        /// <summary>
        /// The high value of the range.
        /// </summary>
        public char High { get; private set; }

        /// <summary>
        /// Creates a new instance of the <see cref="CharRange{T}"/> class.
        /// </summary>
        public CharRange()
        {
            Low = char.MinValue;
            High = char.MaxValue;
        }

        /// <summary>
        /// Creates a new instance of the <see cref="CharRange{T}"/> class.
        /// </summary>
        /// <param name="low">The low value of the range.</param>
        /// <param name="high">The high value of the range.</param>
        public CharRange(char low, char high)
        {
            Low = low;
            High = high;
        }

        /// <summary>
        /// Determines if the range contains the element.
        /// </summary>
        /// <param name="element">The element.</param>
        /// <returns>True if the element is in the range.</returns>
        public bool Contains(char element)
        {
            return this.Low <= element && this.High >= element;
        }

        /// <summary>
        /// Get the intersection of this range and another.
        /// </summary>
        /// <param name="other">The other range.</param>
        /// <returns>A new range representing the intersection.</returns>
        public CharRange Intersect(CharRange other)
        {
            var newLo = this.Low > other.Low ? this.Low : other.Low;
            var newHi = this.High < other.High ? this.High : other.High;
            return new CharRange(newLo, newHi);
        }

        /// <summary>
        /// Get the complement ranges to this range that cover the whole space.
        /// </summary>
        /// <returns>Zero to two ranges that cover the rest of the space.</returns>
        public CharRange[] Complement()
        {
            if (this.IsFull())
            {
                return new CharRange[] { };
            }

            if (this.Low == char.MinValue)
            {
                return new CharRange[] { new CharRange((char)(this.High + 1), char.MaxValue) };
            }

            if (this.High == char.MaxValue)
            {
                return new CharRange[] { new CharRange(char.MinValue, (char)(this.Low - 1)) };
            }

            var r1 = new CharRange(char.MinValue, (char)(this.Low - 1));
            var r2 = new CharRange((char)(this.High + 1), char.MaxValue);
            return new CharRange[] { r1, r2 };
        }

        /// <summary>
        /// Checks if the range is full.
        /// </summary>
        /// <returns></returns>
        public bool IsFull()
        {
            return this.Low == char.MinValue && this.High == char.MaxValue;
        }

        /// <summary>
        /// Determines if this range is empty.
        /// </summary>
        /// <returns>True or false.</returns>
        public bool IsEmpty()
        {
            return this.High < this.Low;
        }

        /// <summary>
        /// Converts the range to a string.
        /// </summary>
        /// <returns>The range as a string.</returns>
        [ExcludeFromCodeCoverage]
        public override string ToString()
        {
            var lo = this.Low >= 32 && this.Low <= 128 ? $"char({(ushort)this.Low}, '{this.Low}')" : $"char({(ushort)this.Low})";
            var hi = this.High >= 32 && this.High <= 128 ? $"char({(ushort)this.High}, '{this.High}')" : $"char({(ushort)this.High})";
            return $"[{lo}-{hi}]";
        }

        /// <summary>
        /// Equality for ranges.
        /// </summary>
        /// <param name="obj">The other object.</param>
        /// <returns>True if equal.</returns>
        public override bool Equals(object obj)
        {
            return obj is CharRange o && Equals(o);
        }

        /// <summary>
        /// Equality for ranges.
        /// </summary>
        /// <param name="other">The other range.</param>
        /// <returns>True if equal.</returns>
        public bool Equals(CharRange other)
        {
            return other != null &&
                   this.Low.Equals(other.Low) &&
                   this.High.Equals(other.High);
        }

        /// <summary>
        /// Hashcode for ranges.
        /// </summary>
        /// <returns>A hashcode.</returns>
        public override int GetHashCode()
        {
            return this.Low.GetHashCode() + this.High.GetHashCode();
        }
    }
}
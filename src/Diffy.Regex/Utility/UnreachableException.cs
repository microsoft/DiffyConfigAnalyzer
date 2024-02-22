// <copyright file="UnreachableException.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System;
    using System.Diagnostics.CodeAnalysis;

    /// <summary>
    /// Exception for unreachable code.
    /// </summary>
    [ExcludeFromCodeCoverage]
    public class UnreachableException : Exception
    {
        /// <summary>
        /// Creates a new instance of the <see cref="ZenUnreachableException"/> class.
        /// </summary>
        public UnreachableException() : base("Unexpected unreachable code detected.")
        {
        }

        /// <summary>
        /// Creates a new instance of the <see cref="ZenException"/> class.
        /// </summary>
        /// <param name="innerException">The inner exception.</param>
        public UnreachableException(Exception innerException) : base("Unexpected unreachable code detected.", innerException)
        {
        }
    }
}
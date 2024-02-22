// <copyright file="Contract.cs" company="Microsoft">
// Copyright (c) Microsoft. All rights reserved.
// </copyright>

namespace Diffy.Regex
{
    using System;

    /// <summary>
    /// A basic contract/assertion class.
    /// </summary>
    internal static class Contract
    {
        /// <summary>
        /// Validate that an argument is true.
        /// </summary>
        /// <param name="obj">The argument.</param>
        /// <param name="msg">An optional message parameter.</param>
        public static void Assert(bool obj, string msg = "Assertion failed")
        {
            if (!obj)
            {
                throw new Exception(msg);
            }
        }

        /// <summary>
        /// Validate that an argument is not null.
        /// </summary>
        /// <param name="obj">The argument.</param>
        public static void AssertNotNull(object obj)
        {
            if (obj is null)
            {
                throw new Exception($"Invalid null argument");
            }
        }

        /// <summary>
        ///     Reports a null error in a conversion from a constant to a Zen value.
        /// </summary>
        /// <param name="obj">The object that may be null.</param>
        /// <param name="where">Description of where the error occurs.</param>
        /// <param name="type">The containing type.</param>
        public static void AssertNullConversion(object obj, string where, Type type)
        {
            if (obj is null)
            {
                throw new Exception($"Null constant in {where} of type {type} can not be converted to a Zen value.");
            }
        }
    }
}

// Aivika for .NET
// Copyright (c) 2009-2015  David Sorokin. All rights reserved.
//
// This file is a part of Aivika for .NET
//
// Commercial License Usage
// Licensees holding valid commercial Aivika licenses may use this file in
// accordance with the commercial license agreement provided with the
// Software or, alternatively, in accordance with the terms contained in
// a written agreement between you and David Sorokin, Yoshkar-Ola, Russia. 
// For the further information contact <mailto:david.sorokin@gmail.com>.
//
// GNU General Public License Usage
// Alternatively, this file may be used under the terms of the GNU
// General Public License version 3 or later as published by the Free
// Software Foundation and appearing in the file LICENSE.GPLv3 included in
// the packaging of this file.  Please review the following information to
// ensure the GNU General Public License version 3 requirements will be
// met: http://www.gnu.org/licenses/gpl-3.0.html.

namespace Simulation.Aivika

open System

/// Specifies a type of the pseudo-random number generator.
type GeneratorType =
    /// Use a simple random number generator which is fast but statistically inaccurate.
    | SimpleGenerator
    /// Use a simple random number generator with the specified seed, which allows receiving repeated numbers.
    | SimpleGeneratorWithSeed of int
    /// Use a more slow but more strong random number generator based on the cryptographic methods.
    | StrongGenerator
    /// Allows using a custom generator.
    | CustomGenerator of (unit -> Generator)

/// Represents a random number generator (not thread-safe).
and [<AbstractClass>] Generator =

    /// Initializes a new instance.
    new: unit -> Generator

    /// Creates a new generator.
    static member Create: generatorType:GeneratorType -> Generator

    /// Generates an uniform random number with minimum 0 and maximum 1.
    abstract NextUniform: unit -> float

    /// Generates an uniform random number with the specified minimum and maximum.
    abstract NextUniform: minimum:float * maximum:float -> float

    /// Generates an integer uniform random number with the specified minimum and maximum.
    abstract NextUniformInt: minimum:int * maximum:int -> int

    /// Generates a normal random number with mean 0 and deviation 1.
    abstract NextNormal: unit -> float
    
    /// Generates a normal random number with the specified mean and deviation.
    abstract NextNormal: mean:float * deviation:float -> float

    /// Generates an exponential random number with the specified mean (the reciprocal of the rate).
    abstract NextExponential: mean:float -> float

    /// Generates a random number with the Erlang distribution with the specified scale (the reciprocal of the rate) and integer shape.
    abstract NextErlang: beta:float * m:int -> float
    
    /// Generates a Poisson random number with the specified mean.
    abstract NextPoisson: mean:float -> int
    
    /// Generates a binomial random number with the specified probability and number of trials.
    abstract NextBinomial: prob:float * trials:int -> int  

/// Represents a basic random number generator created by the specified uniform generator.
and BasicGenerator =

    inherit Generator

    /// Creates a random number generator by the specified uniform generator.
    new: uniformGenerator:(unit -> float) -> BasicGenerator

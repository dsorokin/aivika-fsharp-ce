
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

/// Represents a computation that depends on the simulation parameters.
[<Sealed; NoEquality; NoComparison>]
type Parameter<'a> =

    /// Creates a new computation.
    internal new: f:(Run -> 'a) -> Parameter<'a>

    /// Returns the underlying function.
    member internal Fun: (Run -> 'a)

    /// Lifts a computation.
    static member From: comp:Parameter<'a> -> Parameter<'a>

/// Contains a function that allows invoking the Parameter computation.
[<AutoOpen>]
module internal ParameterInvoke =

    /// Invokes the computation with the specified run.
    val inline invokeParameter: r:Run -> comp:Parameter<'a> -> 'a

[<Sealed>] 
type Parameter<'a> with

    /// Overloaded prefix-plus operator.
    static member (~+): comp:Parameter<float> -> Parameter<float>

    /// Overloaded unary negation.
    static member (~-): comp:Parameter<float> -> Parameter<float>

    /// Overloaded addition operator.
    static member (+): comp1:Parameter<float> * comp2:Parameter<float> -> Parameter<float>
    
    /// Overloaded addition operator.
    static member (+): comp1:Parameter<float> * num2:float -> Parameter<float>
    
    /// Overloaded addition operator.
    static member (+): num1:float * comp2:Parameter<float> -> Parameter<float>

    /// Overloaded subtraction operator.
    static member (-): comp1:Parameter<float> * comp2:Parameter<float> -> Parameter<float>
    
    /// Overloaded subtraction operator.
    static member (-): comp1:Parameter<float> * num2:float -> Parameter<float>
    
    /// Overloaded subtraction operator.
    static member (-): num1:float * comp2:Parameter<float> -> Parameter<float>

    /// Overloaded multiplication operator.
    static member (*): comp1:Parameter<float> * comp2:Parameter<float> -> Parameter<float>
    
    /// Overloaded multiplication operator.
    static member (*): comp1:Parameter<float> * num2:float -> Parameter<float>
    
    /// Overloaded multiplication operator.
    static member (*): num1:float * comp2:Parameter<float> -> Parameter<float>

    /// Overloaded division operator.
    static member (/): comp1:Parameter<float> * comp2:Parameter<float> -> Parameter<float>
    
    /// Overloaded division operator.
    static member (/): comp1:Parameter<float> * num2:float -> Parameter<float>
    
    /// Overloaded division operator.
    static member (/): num1:float * comp2:Parameter<float> -> Parameter<float>

    /// Computation of the absolute value.
    static member Abs: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the inverse cosine.
    static member Acos: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the inverse sine.
    static member Asin: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the inverse tangent.
    static member Atan: comp:Parameter<float> -> Parameter<float>
    
    /// Inverse tangent of x/y where x and y are specified separately.
    static member Atan2: y:Parameter<float> * x:Parameter<float> -> Parameter<float>
    
    /// Computation of the ceiling.
    static member Ceiling: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the exponential.
    static member Exp: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the floor.
    static member Floor: comp:Parameter<float> -> Parameter<float>
    
    /// Overloaded truncate operator.
    static member Truncate: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the round.
    static member Round: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the natural logarithm.
    static member Log: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the logarithm to base 10.
    static member Log10: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the square root.
    static member Sqrt: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the cosine.
    static member Cos: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the hyperbolic cosine.
    static member Cosh: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the sine.
    static member Sin: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the hyperbolic sine.
    static member Sinh: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the tangent.
    static member Tan: comp:Parameter<float> -> Parameter<float>
    
    /// Computation of the hyperbolic tangent.
    static member Tanh: comp:Parameter<float> -> Parameter<float>
    
    /// Overloaded power operator.
    static member Pow: comp1:Parameter<float> * comp2:Parameter<float> -> Parameter<float>
    
    /// Overloaded power operator.
    static member Pow: comp1:Parameter<float> * num2:float -> Parameter<float>

/// Represents a builder for constructing Parameter computations.
[<Sealed>]
type ParameterBuilder =

    /// Creates a computation that returns the specified value.
    member Return: value:'a -> Parameter<'a>
    
    /// Delegates to the input computation.
    member ReturnFrom: comp:Parameter<'a> -> Parameter<'a>
    
    /// Creates a computation that binds the specified computation with its continuation.
    member Bind: comp:Parameter<'a> * cont:('a -> Parameter<'b>) -> Parameter<'b>
    
    /// Creates a computation that runs the specified one.
    member Delay: comp:(unit -> Parameter<'b>) -> Parameter<'b>
    
    /// Creates a computation that just returns ().
    member Zero: unit -> Parameter<unit>
    
    /// Creates a computation that runs the first computation and then runs the second one, returning the result of the latter.
    member Combine: comp1:Parameter<unit> * comp2:Parameter<'a> -> Parameter<'a>
    
    /// Creates a computation that enumerates the sequence on demand and runs body for each element.
    member For: comp:seq<'a> * body:('a -> Parameter<unit>) -> Parameter<unit>
    
    /// Creates a computation that runs the specified computation repeatedly until the provided guard becomes false.
    member While: pred:(unit -> bool) * body:Parameter<unit> -> Parameter<unit>
    
    /// Creates a computation that allows using the disposable resource.
    member Using: resource:'a * body:('a -> Parameter<'b>) -> Parameter<'b> when 'a :> IDisposable
    
    /// Creates a computation with the finalization part.
    member TryFinally: comp:Parameter<'a> * finalizer:(unit -> unit) -> Parameter<'a>
    
    /// Creates a computation with the exception handling.
    member TryWith: comp:Parameter<'a> * handler:(exn -> Parameter<'a>) -> Parameter<'a>

/// Contains a builder of the Parameter computation.
[<AutoOpen>]
module ParameterWorkflow =     

    /// The builder of the Parameter computation.
    val parameter: ParameterBuilder

/// The module contains useful functions for working with the Parameter computations.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Parameter =

    /// Runs a computation by the specified specs.
    [<CompiledName ("Run")>]
    val run: specs:Specs -> comp:Parameter<'a> -> 'a
    
    /// Runs a series of computations by the specified specs and number of runs.
    [<CompiledName ("RunSeries")>]
    val runSeries: n:int -> specs:Specs -> comp:Parameter<'a> -> seq<Async<'a>>

    /// Lifts a computation.
    [<CompiledName ("Lift")>]
    val inline lift: comp:Parameter<'a> -> ^m when ^m: (static member From: Parameter<'a> -> ^m)
    
    /// Flattens the nested computation.
    [<CompiledName ("Concat")>]
    val concat: comp:Parameter<Parameter<'a>> -> Parameter<'a>
    
    /// Maps a function over a computation.
    [<CompiledName ("Map")>]
    val map: f:('a -> 'b) -> comp:Parameter<'a> -> Parameter<'b>
    
    /// Transforms a computation.
    [<CompiledName ("Ap")>]
    val ap: f:Parameter<'a -> 'b> -> comp:Parameter<'a> -> Parameter<'b>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift2")>]
    val lift2: f:('a -> 'b -> 'c) -> comp1:Parameter<'a> -> comp2:Parameter<'b> -> Parameter<'c>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift3")>]
    val lift3: f:('a -> 'b -> 'c -> 'd) -> comp1:Parameter<'a> -> comp2:Parameter<'b> -> comp3:Parameter<'c> -> Parameter<'d>

    /// Promotes a function to computations.
    [<CompiledName ("Lift4")>]
    val lift4: f:('a -> 'b -> 'c -> 'd -> 'e) -> comp1:Parameter<'a> -> comp2:Parameter<'b> -> comp3:Parameter<'c> -> comp4:Parameter<'d> -> Parameter<'e>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift5")>]
    val lift5: f:('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> comp1:Parameter<'a> -> comp2:Parameter<'b> -> comp3:Parameter<'c> -> comp4:Parameter<'d> -> comp5:Parameter<'e> -> Parameter<'f>

    /// Takes two computations and creates a new computation of the corresponding pairs.   
    [<CompiledName ("Zip")>]
    val zip: comp1:Parameter<'a> -> comp2:Parameter<'b> -> Parameter<'a * 'b>
    
    /// Takes three computations and creates a new computation of the corresponding triples.
    [<CompiledName ("Zip3")>]
    val zip3: comp1:Parameter<'a> -> comp2:Parameter<'b> -> comp3:Parameter<'c> -> Parameter<'a * 'b * 'c>
    
    /// Takes a list of computations and creates a new computation of the corresponding lists.
    [<CompiledName ("OfList")>]
    val ofList: comps:Parameter<'b> list -> Parameter<'b list>
    
    /// Takes an array of computations and creates a new computation of the corresponding arrays.
    [<CompiledName ("OfArray")>]
    val ofArray: comps:Parameter<'b> [] -> Parameter<'b []>
        
    /// Takes a sequence of computations and creates a new computation of the corresponding sequences.
    [<CompiledName ("OfEnumerable")>]
    val ofSeq: comps:#seq<Parameter<'b>> -> Parameter<seq<'b>>
    
    /// Memoizes the specified computation so that a new computation would always return the same result within the simulation run.
    [<CompiledName ("Memo")>]
    val memo: comp:Parameter<'a> -> Parameter<'a>

    /// Computation that returns the simulation specs.
    [<CompiledName ("Specs")>]
    val specs: Parameter<Specs>

    /// Computation that returns the start simulation time.
    [<CompiledName ("StartTime")>]
    val starttime: Parameter<Time>

    /// Computation that returns the stop simulation time.
    [<CompiledName ("StopTime")>]
    val stoptime: Parameter<Time>

    /// Computation that returns the integration time step.
    [<CompiledName ("DT")>]
    val dt: Parameter<Time>

    /// Computation that returns the current run index.
    [<CompiledName ("RunIndex")>]
    val runIndex: Parameter<int>

    /// Computation that returns the total run count in the series.
    [<CompiledName ("RunCount")>]
    val runCount: Parameter<int>

    /// Returns a computation which value is taken from array based on the run index (Design of Experiments).
    [<CompiledName ("FromArray")>]
    val fromArray: values:'a [] -> Parameter<'a>

    /// Computation that returns the random number generator for this run.
    [<CompiledName ("Generator")>]
    val generator: Parameter<Generator>
    
    /// Computation that generates a new random number distributed uniformly with the specified minimum and maximum.
    [<CompiledName ("RandomUniform")>]
    val randomUniform: minimum:float -> maximum:float -> Parameter<float>
    
    /// Computation that generates a new integer random number distributed uniformly with the specified minimum and maximum.
    [<CompiledName ("RandomUniformInt")>]
    val randomUniformInt: minimum:int -> maximum:int -> Parameter<int>

    /// Computation that generates a new triangular random number with the specified minimum, median and maximum.
    [<CompiledName ("RandomTriangular")>]
    val randomTriangular: minimum:float -> median:float -> maximum:float -> Parameter<float>
    
    /// Computation that generates a new normal random number with the specified mean and deviation.
    [<CompiledName ("RandomNormal")>]
    val randomNormal: mean:float -> deviation:float -> Parameter<float>    
    
    /// Computation that generates a new random number distributed exponentially with the specified mean (the reciprocal of the rate).
    [<CompiledName ("RandomExponential")>]
    val randomExponential: mean:float -> Parameter<float>
    
    /// Computation that generates a new random number having the Erlang distribution with the specified scale (the reciprocal of the rate) and integer shape.
    [<CompiledName ("RandomErlang")>]
    val randomErlang: beta:float -> m:int -> Parameter<float>
    
    /// Computation that generates a new Poisson random number with the specified mean.
    [<CompiledName ("RandomPoisson")>]
    val randomPoisson: mean:float -> Parameter<int>
    
    /// Computation that generates a new random number distributed binomially with the specified probability and number of trials.
    [<CompiledName ("RandomBinomial")>]
    val randomBinomial: prob:float -> trials:int -> Parameter<int>

    /// Computation that returns true with the specified probability.
    [<CompiledName ("RandomTrue")>]
    val randomTrue: prob:float -> Parameter<bool>

    /// Computation that returns false with the specified probability.
    [<CompiledName ("RandomFalse")>]
    val randomFalse: prob:float -> Parameter<bool>

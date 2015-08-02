
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

/// Represents a computation in time points, which is suitable for approximating integrals, for example.
[<Sealed; NoEquality; NoComparison>]
type Dynamics<'a> =

    /// Creates a new computation.
    internal new: f:(Point -> 'a) -> Dynamics<'a>

    /// Returns the underlying function.
    member internal Fun: (Point -> 'a)

    /// Lifts a computation.
    static member From: comp:Parameter<'a> -> Dynamics<'a>

    /// Lifts a computation.
    static member From: comp:Simulation<'a> -> Dynamics<'a>

    /// Lifts a computation.
    static member From: comp:Dynamics<'a> -> Dynamics<'a>

/// Contains a function that allows invoking the Dynamics computation.
[<AutoOpen>]
module internal DynamicsInvoke =

    /// Invokes the computation with the specified time point.
    val inline invokeDynamics: p:Point -> comp:Dynamics<'a> -> 'a

[<Sealed>] 
type Dynamics<'a> with

    /// Overloaded prefix-plus operator.
    static member (~+): comp:Dynamics<float> -> Dynamics<float>

    /// Overloaded unary negation.
    static member (~-): comp:Dynamics<float> -> Dynamics<float>

    /// Overloaded addition operator.
    static member (+): comp1:Dynamics<float> * comp2:Dynamics<float> -> Dynamics<float>

    /// Overloaded addition operator.
    static member (+): comp1:Dynamics<float> * comp2:Parameter<float> -> Dynamics<float>
    
    /// Overloaded addition operator.
    static member (+): comp1:Dynamics<float> * num2:float -> Dynamics<float>
    
    /// Overloaded addition operator.
    static member (+): comp1:Parameter<float> * comp2:Dynamics<float> -> Dynamics<float>
    
    /// Overloaded addition operator.
    static member (+): num1:float * comp2:Dynamics<float> -> Dynamics<float>
    
    /// Overloaded subtraction operator.
    static member (-): comp1:Dynamics<float> * comp2:Dynamics<float> -> Dynamics<float>
    
    /// Overloaded subtraction operator.
    static member (-): comp1:Dynamics<float> * comp2:Parameter<float> -> Dynamics<float>
    
    /// Overloaded subtraction operator.
    static member (-): comp1:Dynamics<float> * num2:float -> Dynamics<float>
    
    /// Overloaded subtraction operator.
    static member (-): comp1:Parameter<float> * comp2:Dynamics<float> -> Dynamics<float>
    
    /// Overloaded subtraction operator.
    static member (-): num1:float * comp2:Dynamics<float> -> Dynamics<float>

    /// Overloaded multiplication operator.
    static member (*): comp1:Dynamics<float> * comp2:Dynamics<float> -> Dynamics<float>
    
    /// Overloaded multiplication operator.
    static member (*): comp1:Dynamics<float> * comp2:Parameter<float> -> Dynamics<float>
    
    /// Overloaded multiplication operator.
    static member (*): comp1:Dynamics<float> * num2:float -> Dynamics<float>
    
    /// Overloaded multiplication operator.
    static member (*): comp1:Parameter<float> * comp2:Dynamics<float> -> Dynamics<float>

    /// Overloaded multiplication operator.
    static member (*): num1:float * comp2:Dynamics<float> -> Dynamics<float>

    /// Overloaded division operator.
    static member (/): comp1:Dynamics<float> * comp2:Dynamics<float> -> Dynamics<float>
    
    /// Overloaded division operator.
    static member (/): comp1:Dynamics<float> * comp2:Parameter<float> -> Dynamics<float>
    
    /// Overloaded division operator.
    static member (/): comp1:Dynamics<float> * num2:float -> Dynamics<float>

    /// Overloaded division operator.
    static member (/): comp1:Parameter<float> * comp2:Dynamics<float> -> Dynamics<float>
    
    /// Overloaded division operator.
    static member (/): num1:float * comp2:Dynamics<float> -> Dynamics<float>

    /// Computation of the absolute value.
    static member Abs: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the inverse cosine.
    static member Acos: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the inverse sine.
    static member Asin: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the inverse tangent.
    static member Atan: comp:Dynamics<float> -> Dynamics<float>
    
    /// Inverse tangent of x/y where x and y are specified separately.
    static member Atan2: y:Dynamics<float> * x:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the ceiling.
    static member Ceiling: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the exponential.
    static member Exp: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the floor.
    static member Floor: comp:Dynamics<float> -> Dynamics<float>
    
    /// Overloaded truncate operator.
    static member Truncate: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the round.
    static member Round: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the natural logarithm.
    static member Log: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the logarithm to base 10.
    static member Log10: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the square root.
    static member Sqrt: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the cosine.
    static member Cos: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the hyperbolic cosine.
    static member Cosh: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the sine.
    static member Sin: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the hyperbolic sine.
    static member Sinh: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the tangent.
    static member Tan: comp:Dynamics<float> -> Dynamics<float>
    
    /// Computation of the hyperbolic tangent.
    static member Tanh: comp:Dynamics<float> -> Dynamics<float>
    
    /// Overloaded power operator.
    static member Pow: comp1:Dynamics<float> * comp2:Dynamics<float> -> Dynamics<float>
    
    /// Overloaded power operator.
    static member Pow: comp1:Dynamics<float> * num2:float -> Dynamics<float>

/// Represents a builder for constructing Dynamics computations.
[<Sealed>]
type DynamicsBuilder =

    /// Creates a computation that returns the specified value.
    member Return: value:'a -> Dynamics<'a>
    
    /// Delegates to the input computation.
    member ReturnFrom: comp:Dynamics<'a> -> Dynamics<'a>
    
    /// Creates a computation that binds the specified computation with its continuation.
    member Bind: comp:Dynamics<'a> * cont:('a -> Dynamics<'b>) -> Dynamics<'b>
    
    /// Creates a computation that runs the specified one.
    member Delay: comp:(unit -> Dynamics<'b>) -> Dynamics<'b>
    
    /// Creates a computation that just returns ().
    member Zero: unit -> Dynamics<unit>
    
    /// Creates a computation that runs the first computation and then runs the second one, returning the result of the latter.
    member Combine: comp1:Dynamics<unit> * comp2:Dynamics<'a> -> Dynamics<'a>
    
    /// Creates a computation that enumerates the sequence on demand and runs body for each element.
    member For: comp:seq<'a> * body:('a -> Dynamics<unit>) -> Dynamics<unit>
    
    /// Creates a computation that runs the specified computation repeatedly until the provided guard becomes false.
    member While: pred:(unit -> bool) * body:Dynamics<unit> -> Dynamics<unit>
    
    /// Creates a computation that allows using the disposable resource.
    member Using: resource:'a * body:('a -> Dynamics<'b>) -> Dynamics<'b> when 'a :> IDisposable
    
    /// Creates a computation with the finalization part.
    member TryFinally: comp:Dynamics<'a> * finalizer:(unit -> unit) -> Dynamics<'a>
    
    /// Creates a computation with the exception handling.
    member TryWith: comp:Dynamics<'a> * handler:(exn -> Dynamics<'a>) -> Dynamics<'a>

/// Contains a builder of the Dynamics computation.
[<AutoOpen>]
module DynamicsWorkflow =     

    /// The builder of the Dynamics computation.
    val dynamics: DynamicsBuilder

/// The module contains useful functions for working with the Dynamics computations.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Dynamics =

    /// Runs a computation in the start time point.
    [<CompiledName ("RunInStartTime")>]
    val runInStartTime: comp:Dynamics<'a> -> Simulation<'a>
    
    /// Runs a computation in the final time point.
    [<CompiledName ("RunInStopTime")>]
    val runInStopTime: comp:Dynamics<'a> -> Simulation<'a>
    
    /// Runs a computation in the integration time points.
    [<CompiledName ("RunInIntegTimes")>]
    val runInIntegTimes: comp:Dynamics<'a> -> Simulation<seq<'a>>
    
    /// Runs a computation in the specified time values.
    [<CompiledName ("RunInTimes")>]
    val runInTimes: ts:seq<Time> -> comp:Dynamics<'a> -> Simulation<seq<'a>>

    /// Lifts a computation.
    [<CompiledName ("Lift")>]
    val inline lift: comp:Dynamics<'a> -> ^m when ^m: (static member From: Dynamics<'a> -> ^m)
    
    /// Flattens the nested computation.
    [<CompiledName ("Concat")>]
    val concat: comp:Dynamics<Dynamics<'a>> -> Dynamics<'a>
    
    /// Maps a function over a computation.
    [<CompiledName ("Map")>]
    val map: f:('a -> 'b) -> comp:Dynamics<'a> -> Dynamics<'b>
    
    /// Transforms a computation.
    [<CompiledName ("Ap")>]
    val ap: f:Dynamics<'a -> 'b> -> comp:Dynamics<'a> -> Dynamics<'b>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift2")>]
    val lift2: f:('a -> 'b -> 'c) -> comp1:Dynamics<'a> -> comp2:Dynamics<'b> -> Dynamics<'c>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift3")>]
    val lift3: f:('a -> 'b -> 'c -> 'd) -> comp1:Dynamics<'a> -> comp2:Dynamics<'b> -> comp3:Dynamics<'c> -> Dynamics<'d>

    /// Promotes a function to computations.
    [<CompiledName ("Lift4")>]
    val lift4: f:('a -> 'b -> 'c -> 'd -> 'e) -> comp1:Dynamics<'a> -> comp2:Dynamics<'b> -> comp3:Dynamics<'c> -> comp4:Dynamics<'d> -> Dynamics<'e>
    
    /// Promotes a function to computations.
    [<CompiledName ("Lift5")>]
    val lift5: f:('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> comp1:Dynamics<'a> -> comp2:Dynamics<'b> -> comp3:Dynamics<'c> -> comp4:Dynamics<'d> -> comp5:Dynamics<'e> -> Dynamics<'f>

    /// Takes two computations and creates a new computation of the corresponding pairs. 
    [<CompiledName ("Zip")>]
    val zip: comp1:Dynamics<'a> -> comp2:Dynamics<'b> -> Dynamics<'a * 'b>
    
    /// Takes three computations and creates a new computation of the corresponding triples.
    [<CompiledName ("Zip3")>]
    val zip3: comp1:Dynamics<'a> -> comp2:Dynamics<'b> -> comp3:Dynamics<'c> -> Dynamics<'a * 'b * 'c>
    
    /// Takes a list of computations and creates a new computation of the corresponding lists.
    [<CompiledName ("OfList")>]
    val ofList: comps:Dynamics<'b> list -> Dynamics<'b list>
    
    /// Takes an array of computations and creates a new computation of the corresponding arrays.
    [<CompiledName ("OfArray")>]
    val ofArray: comps:Dynamics<'b> [] -> Dynamics<'b []>
        
    /// Takes a sequence of computations and creates a new computation of the corresponding sequences.
    [<CompiledName ("OfEnumerable")>]
    val ofSeq: comps:#seq<Dynamics<'b>> -> Dynamics<seq<'b>>

    /// Discretizes the input computation in the sense that the resulting computation changes only in the integration time points.
    [<CompiledName ("Discrete")>]
    val discrete: comp:Dynamics<'a> -> Dynamics<'a>

    /// Interpolates the input computation in the sense that the resulting computation changes only in the integration time points and intermediate points used by the integration method.
    [<CompiledName ("Interpolate")>]
    val interpolate: comp:Dynamics<'a> -> Dynamics<'a>

    /// Memoizes the computation so that a new computation would always return the same values in the integration time points and would be interpolated in other points.
    [<CompiledName ("Memo")>]
    val memo: comp:Dynamics<'a> -> Dynamics<'a>

    /// Memoizes the computation so that a new computation would always return the same values in the integration time points and would be discretized in other points.
    [<CompiledName ("Memo0")>]
    val memo0: comp:Dynamics<'a> -> Dynamics<'a>
    
    /// Memoized computation that generates random numbers distributed uniformly with the specified minimum and maximum.
    [<CompiledName ("MemoRandomUniform")>]
    val memoRandomUniform: minimum:Dynamics<float> -> maximum:Dynamics<float> -> Dynamics<float>
    
    /// Memoized computation that generates integer random numbers distributed uniformly with the specified minimum and maximum.
    [<CompiledName ("MemoRandomUniformInt")>]
    val memoRandomUniformInt: minimum:Dynamics<int> -> maximum:Dynamics<int> -> Dynamics<int>

    /// Memoized computation that generates triangular random numbers with the specified minimum, median and maximum.
    [<CompiledName ("MemoRandomTriangular")>]
    val memoRandomTriangular: minimum:Dynamics<float> -> median:Dynamics<float> -> maximum:Dynamics<float> -> Dynamics<float>
    
    /// Memoized computation that generates normal random numbers with the specified mean and deviation.
    [<CompiledName ("MemoRandomNormal")>]
    val memoRandomNormal: mean:Dynamics<float> -> deviation:Dynamics<float> -> Dynamics<float>    
    
    /// Memoized computation that generates random numbers distributed exponentially with the specified mean (the reciprocal of the rate).
    [<CompiledName ("MemoRandomExponential")>]
    val memoRandomExponential: mean:Dynamics<float> -> Dynamics<float>
    
    /// Memoized computation that generates random numbers having the Erlang distribution with the specified scale (the reciprocal of the rate) and integer shape.
    [<CompiledName ("MemoRandomErlang")>]
    val memoRandomErlang: beta:Dynamics<float> -> m:Dynamics<int> -> Dynamics<float>
    
    /// Memoized computation that generates the Poisson random numbers with the specified mean.
    [<CompiledName ("MemoRandomPoisson")>]
    val memoRandomPoisson: mean:Dynamics<float> -> Dynamics<int>
    
    /// Memoized computation that generates random numbers distributed binomially with the specified probability and number of trials.
    [<CompiledName ("MemoRandomBinomial")>]
    val memoRandomBinomial: prob:Dynamics<float> -> trials:Dynamics<int> -> Dynamics<int>

    /// Computation that returns the current simulation time.
    [<CompiledName ("Time")>]
    val time: Dynamics<Time>

    /// Computation that returns the current iteration number or its closest value for discrete event simulation.
    [<CompiledName ("IntegIteration")>]
    val integIteration: Dynamics<Iteration>

    /// Computation that returns the current integration phase or -1 for discrete event simulation.
    [<CompiledName ("IntegPhase")>]
    val integPhase: Dynamics<Phase>

    /// Computation that returns the maximum value.  
    [<CompiledName ("Max")>]
    val max: comp1:Dynamics<'a> -> comp2:Dynamics<'a> -> Dynamics<'a> when 'a: comparison

    /// Computation that returns the minimum value.  
    [<CompiledName ("Min")>]
    val min: comp1:Dynamics<'a> -> comp2:Dynamics<'a> -> Dynamics<'a> when 'a: comparison
    
    /// Implements the if-then-else operator.
    [<CompiledName ("IfThenElse")>]
    val ifThenElse: cond:Dynamics<bool> -> thenPart:Dynamics<'a> -> elsePart:Dynamics<'a> -> Dynamics<'a>
    
    /// Shows the debug message with the current simulation time.
    [<CompiledName ("Trace")>]
    val trace: message:string -> comp:Dynamics<'a> -> Dynamics<'a>

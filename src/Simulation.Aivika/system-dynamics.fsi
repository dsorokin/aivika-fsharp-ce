
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

/// Contains functions of System Dynamics (SD).
module SD =     
    
    /// Creates a computation of the specified argument.
    [<CompiledName ("Num")>]
    val inline num: value:'a -> Dynamics<'a>
    
    /// Compares for equality.
    val inline (.=.): comp1:Dynamics<'a> -> comp2:Dynamics<'a> -> Dynamics<bool> when 'a: equality
    
    /// Compares for inequality.
    val inline (.<>.): comp1:Dynamics<'a> -> comp2:Dynamics<'a> -> Dynamics<bool> when 'a: equality
    
    /// Compares for ordering.
    val inline (.<.): comp1:Dynamics<'a> -> comp2:Dynamics<'a> -> Dynamics<bool> when 'a: comparison
    
    /// Compares for ordering.
    val inline (.>=.): comp1:Dynamics<'a> -> comp2:Dynamics<'a> -> Dynamics<bool> when 'a: comparison
    
    /// Compares for ordering.
    val inline (.>.): comp1:Dynamics<'a> -> comp2:Dynamics<'a> -> Dynamics<bool> when 'a: comparison
    
    /// Compares for ordering.
    val inline (.<=.): comp1:Dynamics<'a> -> comp2:Dynamics<'a> -> Dynamics<bool> when 'a: comparison
    
    /// Implements the if-then-else operator.
    [<CompiledName ("IfThenElse")>]
    val ifThenElse: cond:Dynamics<bool> -> thenPart:Dynamics<'a> -> elsePart:Dynamics<'a> -> Dynamics<'a>
  
    /// Returns an integral by the specified derivative and initial value. 
    [<CompiledName ("Integ")>]
    val integ: derivative:Lazy<Dynamics<float>> -> init:Dynamics<float> -> Dynamics<float>

    /// Either sets a new integral value or integrates by the specified derivative and initial value. 
    [<CompiledName ("IntegChoice")>]
    val integChoice: integOrDerivative:Lazy<Dynamics<Choice<float, float>>> -> init:Dynamics<float> -> Dynamics<float>
    
    /// Returns the first order exponential smooth.
    [<CompiledName ("Smooth")>]
    val smooth: x:Dynamics<float> -> time:Lazy<Dynamics<float>> -> Dynamics<float>
    
    /// Returns the first order exponential smooth with the specified initial value.
    [<CompiledName ("SmoothI")>]
    val smoothI: x:Lazy<Dynamics<float>> -> time:Lazy<Dynamics<float>> -> init:Dynamics<float> -> Dynamics<float>
    
    /// Returns the third order exponential smooth.
    [<CompiledName ("Smooth3")>]
    val smooth3: x:Dynamics<float> -> time:Lazy<Dynamics<float>> -> Dynamics<float>
    
    /// Returns the third order exponential smooth with the specified initial value.
    [<CompiledName ("Smooth3I")>]
    val smooth3I: x:Lazy<Dynamics<float>> -> time:Lazy<Dynamics<float>> -> init:Dynamics<float> -> Dynamics<float>
    
    /// Returns the n'th order exponential smooth.
    [<CompiledName ("SmoothN")>]
    val smoothN: x:Dynamics<float> -> time:Lazy<Dynamics<float>> -> n:int -> Dynamics<float>
    
    /// Returns the n'th order exponential smooth with the specified initial value.
    [<CompiledName ("SmoothNI")>]
    val smoothNI: x:Lazy<Dynamics<float>> -> time:Lazy<Dynamics<float>> -> n:int -> init:Dynamics<float> -> Dynamics<float>
    
    /// Returns the first order exponential delay.
    [<CompiledName ("Delay1")>]
    val delay1: x:Dynamics<float> -> time:Dynamics<float> -> Dynamics<float>
    
    /// Returns the first order exponential delay with the specified initial value.
    [<CompiledName ("Delay1I")>]
    val delay1I: x:Lazy<Dynamics<float>> -> time:Dynamics<float> -> init:Dynamics<float> -> Dynamics<float>
    
    /// Returns the third order exponential delay.
    [<CompiledName ("Delay3")>]
    val delay3: x:Dynamics<float> -> time:Dynamics<float> -> Dynamics<float>
    
    /// Returns the third order exponential delay with the specified initial value.
    [<CompiledName ("Delay3I")>]
    val delay3I: x:Lazy<Dynamics<float>> -> time:Dynamics<float> -> init:Dynamics<float> -> Dynamics<float>
    
    /// Returns the n'th order exponential delay.
    [<CompiledName ("DelayN")>]
    val delayN: x:Dynamics<float> -> time:Dynamics<float> -> n:int -> Dynamics<float>
    
    /// Returns the n'th order exponential delay with the specified initial value.
    [<CompiledName ("DelayNI")>]
    val delayNI: x:Lazy<Dynamics<float>> -> time:Dynamics<float> -> n:int -> init:Dynamics<float> -> Dynamics<float>
    
    /// Returns a forecast.
    [<CompiledName ("Forecast")>]
    val forecast: x:Dynamics<float> -> averageTime:Dynamics<float> -> timeHorizon:Dynamics<float> -> Dynamics<float>
    
    /// Returns a trend as the fractional change rate.
    [<CompiledName ("Trend")>]
    val trend: x:Dynamics<float> -> averageTime:Dynamics<float> -> init:Dynamics<float> -> Dynamics<float>

    /// Returns a sum for the difference equation.
    [<CompiledName ("DiffSum")>]
    val diffsum: difference:Lazy<Dynamics<float>> -> init:Dynamics<float> -> Dynamics<float>

    /// Either sets a new sum value or returns a sum by the specified difference and initial value.
    [<CompiledName ("DiffSumChoice")>]
    val diffsumChoice: sumOrDifference:Lazy<Dynamics<Choice<float, float>>> -> init:Dynamics<float> -> Dynamics<float>

    /// Looks up x in a table of pairs (x, y) using linear interpolation.
    [<CompiledName ("Lookup")>]
    val lookup: x:Dynamics<float> -> tbl:Table -> Dynamics<float>
    
    /// Looks up x in a table of pairs (x, y) using stepwise function.
    [<CompiledName ("LookupStepwise")>]
    val lookupStepwise: x:Dynamics<float> -> tbl:Table -> Dynamics<float>
    
    /// Returns the delayed value using the specified lag time.
    [<CompiledName ("Delay")>]
    val delay: x:Dynamics<'a> -> lagTime:Dynamics<float> -> Dynamics<'a>
    
    /// Returns the delayed value using the specified lag time and initial value.
    [<CompiledName ("DelayI")>]
    val delayI: x:Lazy<Dynamics<'a>> -> lagTime:Dynamics<float> -> init:Dynamics<'a> -> Dynamics<'a>
    
    /// Returns 0 until the step time and then returns the specified height within the computation.
    [<CompiledName ("Step")>]
    val step: height:Dynamics<float> -> stepTime:Dynamics<float> -> Dynamics<float>
    
    /// Computation that returns 1.0, starting at the time start, and lasting for the interval width; 0.0 is returned at all other times.
    [<CompiledName ("Pulse")>]
    val pulse: timeStart:Dynamics<float> -> intervalWidth:Dynamics<float> -> Dynamics<float>
    
    /// Computation that returns 1.0, starting at the time start, and lasting for the interval width and then repeats this pattern with the specified period; 0.0 is returned at all other times.
    [<CompiledName ("PulseP")>]
    val pulseP: timeStart:Dynamics<float> -> intervalWidth:Dynamics<float> -> period:Dynamics<float> -> Dynamics<float>
    
    /// Computation that returns 0 until the specified time start and then slopes upward until the end time and then holds constant.
    [<CompiledName ("Ramp")>]
    val ramp: slope:Dynamics<float> -> timeStart:Dynamics<float> -> endTime:Dynamics<float> -> Dynamics<float>
    
    /// Returns the Net Present Value (NPV) of the stream computed using the specified discount rate, the initial value and some factor (usually 1).
    [<CompiledName ("NPV")>]
    val npv: stream:Dynamics<float> 
                -> rate:Dynamics<float> 
                -> init:Dynamics<float>
                -> factor:Dynamics<float>
                -> Dynamics<float>
    
    /// Return the Net Present Value End of period (NPVE) of the stream computed using the specified discount rate, the initial value and some factor.
    [<CompiledName ("NPVE")>]
    val npve: stream:Dynamics<float> 
                -> rate:Dynamics<float> 
                -> init:Dynamics<float>
                -> factor:Dynamics<float>
                -> Dynamics<float>

/// Represents an integral.
type Integ =

    /// Initializes a new instance.
    new: init:Dynamics<float> -> Integ

    /// Returns the initial value of the integral.
    member InitValue: Dynamics<float>

    /// Returns the value of the integral.
    member Value: Dynamics<float>

    /// Gets the derivative of the integral.
    member Diff: Dynamics<float>

    /// Sets the derivative of the integral. 
    member Diff: Dynamics<float> with set

/// Represents a summator.
type DiffSum =

    /// Initializes a new instance.
    new: init:Dynamics<float> -> DiffSum

    /// Returns the initial value of the summator.
    member InitValue: Dynamics<float>

    /// Returns the sum of values.
    member Value: Dynamics<float>

    /// Gets the difference value.
    member Diff: Dynamics<float>

    /// Sets the difference value. 
    member Diff: Dynamics<float> with set


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

namespace Simulation.Aivika.Experiments
        
open System        

/// The strategy applied to calculate the histogram bins.
type BinningStrategy = array<float> -> int

/// Represents a histogram.   
type Histogram = array<float * array<int>>
    
/// The module contains useful functions for working with the binning strategies.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module BinningStrategy =

    /// Returns the kurtosis.
    [<CompiledName ("Kurtosis")>]
    val kurtosis: xs:array<float> -> float 

    /// Sturges' binning strategy is the least computational work, but recommended for only normal data.
    [<CompiledName ("Sturges")>]
    val sturges: BinningStrategy

    /// Doane's binning strategy extends Sturges' for non-normal data. It takes a little more time because it must calculate the kurtosis (peakkiness) of the distribution.
    [<CompiledName ("Doane")>]
    val doane: BinningStrategy

    /// Using the sqrt of the number of samples is not supported by any theory, but is commonly used by Excel and other histogram making software.
    [<CompiledName ("Sqrt")>]
    val sqrt: BinningStrategy

    /// Scott's rule is the optimal solution for normal data, but requires more computation than Sturges'.
    [<CompiledName ("Scott")>]
    val scott: BinningStrategy

/// The module contains useful functions for working with the histograms.
[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Histogram =

    /// Creates a histogram by the exact bin size.
    [<CompiledName ("CreateByBinSize")>]
    val createByBinSize: size:float -> xs:array<array<float>> -> Histogram

    /// Creates a histogram by the approximated number of bins.
    [<CompiledName ("CreateByBinCount")>]
    val createByBinCount: n:int -> xs:array<array<float>> -> Histogram

    /// Creates a histogram by the specified binning strategy and array of series.
    [<CompiledName ("Create")>]
    val create: strat:BinningStrategy -> xs:array<array<float>> -> Histogram

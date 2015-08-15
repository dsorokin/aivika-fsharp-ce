
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

[<Sealed>]
type Table (ps: (float * float) []) =

    let last = (Array.length ps) - 1

    let rec find left right x =

        if left > right then failwithf "Invalid index."
        else
        
            let index = (left + 1 + right) / 2
            let x1 = fst ps.[index]
            
            if x1 <= x then 
            
                if index < right then find index right x
                elif right = last then snd ps.[right]
                else
                
                    let x2 = fst ps.[index + 1]
                    let y1 = snd ps.[index]
                    let y2 = snd ps.[index + 1]
                    in y1 + (y2 - y1) * (x - x1) / (x2 - x1)
                
            else
            
                if left < index then find left (index - 1) x
                elif left = 0 then snd ps.[left]
                else failwithf "Invalid index."
                
    let rec findStepwise left right x =

        if left > right then failwithf "Invalid index."
        else
        
            let index = (left + 1 + right) / 2
            let x1 = fst ps.[index]
            
            if x1 <= x then
            
                if index < right then findStepwise index right x
                elif right = last then snd ps.[right]
                else  snd ps.[index]
                
            else
            
                if left < index then findStepwise left (index - 1) x
                elif left = 0 then snd ps.[left]
                else failwithf "Invalid index."

    new (xs: float[], ys: float[]) = Table (Array.zip xs ys)

    member tbl.Lookup (x: float) = find 0 last x
    member tbl.LookupStepwise (x: float) = findStepwise 0 last x

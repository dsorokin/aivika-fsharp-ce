
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

namespace Simulation.Aivika.Collections

open System

type PriorityQueue<'a> () = 

    let mutable ks: float [] = Array.zeroCreate 11
    let mutable vs: 'a [] = Array.zeroCreate 11
    
    let kref = ref ks
    let vref = ref vs
    
    let mutable size = 0
    
    let increase capacity =
    
        let len = ks.Length
        
        assert (capacity >= 0)
        let capacity' = if len < 64 then (len + 1) <<< 1 else (len >>> 1) * 3
        assert (capacity' >= 0)
        let capacity' = if capacity' < capacity then capacity else capacity'
        
        Array.Resize (kref, capacity')
        Array.Resize (vref, capacity')
        
        ks <- !kref
        vs <- !vref
    
    let rec siftUp i k v = 
        if i = 0 then
            ks.[i] <- k
            vs.[i] <- v
        else
            let n = (i - 1) >>> 1
            if k >= ks.[n] then
                ks.[i] <- k
                vs.[i] <- v
            else
                ks.[i] <- ks.[n]
                vs.[i] <- vs.[n]
                siftUp n k v    // tail call
        
    let rec siftDown i k v =
        if i >= (size >>> 1) then
            ks.[i] <- k
            vs.[i] <- v
        else
            let n  = (i <<< 1) + 1
            let n' = n + 1
            let n  = if n' < size && ks.[n] > ks.[n'] then n' else n
            if k <= ks.[n] then
                ks.[i] <- k
                vs.[i] <- v
            else
                ks.[i] <- ks.[n]
                vs.[i] <- vs.[n]
                siftDown n k v  // tail call

    let rec indexBy p i =
        if i >= size then -1
        else if p vs.[i] then i
        else indexBy p (1 + i) 

    member x.IsEmpty = (size = 0)

    member x.Count = size

    member x.Enqueue (k, v) =
    
        let i = size
        if i >= ks.Length then 
            increase (i + 1)
        size <- i + 1
        
        siftUp i k v

    member x.Dequeue () =
    
        assert (size > 0)
        size <- size - 1
        
        let k = ks.[size]
        let v = vs.[size]
        
        ks.[size] <- Unchecked.defaultof<_>
        vs.[size] <- Unchecked.defaultof<_>
        
        if size > 0 then
            siftDown 0 k v

    member x.Front =
        assert (size > 0)
        (ks.[0], vs.[0])
        
    member x.FrontKey =
        assert (size > 0)
        ks.[0]
        
    member x.FrontValue =
        assert (size > 0)
        vs.[0]

    member internal x.RemoveBy (p) =
         
        let i = indexBy p 0

        if i < 0 then 
            None
        else

            let v0 = vs.[i]

            assert (size > 0)
            size <- size - 1

            let k = ks.[size]
            let v = vs.[size]

            ks.[size] <- Unchecked.defaultof<_>
            vs.[size] <- Unchecked.defaultof<_>
            
            if size > 0 then
                siftDown i k v

            Some v0

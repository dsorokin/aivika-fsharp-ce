
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

/// Represents the priority queue.
type PriorityQueue<'a> = 

    /// Creates a new priority queue.
    new: unit -> PriorityQueue<'a>
    
    /// Tests whether the priotity queue is empty.
    member IsEmpty: bool
    
    /// Returns the size of the queue.
    member Count: int

    /// Enqueues a new element with the specified priority.
    member Enqueue: priority:float * item:'a -> unit
    
    /// Dequeues the element with the minimal priority.
    member Dequeue: unit -> unit
    
    /// Returns the minimal priority available.
    member FrontKey: float
    
    /// Returns the element with the minimal priority.
    member FrontValue: 'a
    
    /// Returns the minimal priority and the corresponded element.
    member Front: float * 'a

    /// Tries to remove an element satisfying the specified predicate.
    member internal RemoveBy: pred:('a -> bool) -> 'a option
**Aivika for .NET** 

This is an F# port of the Haskell simulation [library] (http://hackage.haskell.org/package/aivika)
of the same name by the same author, David Sorokin, who developed
the original library. It supports discrete event simulation, system
dynamics and partially agent-based modeling.

### Idea

The main idea is that many simulation activities can actually be 
represented as abstract computations. Functional programming has 
a developed apparatus for creating such computations, combining them, 
reasoning about them and so on. This is monads, streams, arrows etc. 
It is important that F# and Haskell provide an easy-to-use syntactic 
sugar for working with such computations, which is a clue to practical 
usability of the method.

### Features

Aivika for .NET has the following features:

* allows defining recursive stochastic differential equations of 
  System Dynamics (unordered as in maths);

* supports the event-driven paradigm of DES as a basic core
  for implementing other paradigms;

* supports extensively the process-oriented paradigm of DES
  with an ability to resume, suspend and cancel 
  the discontinuous processes;

* allows working with the resources based on specified queue strategies 
  (FCFS/FIFO, LCFS/LIFO, SIRO, static priorities and so on);

* allows customizing the infinite and finite queues based on strategies too;

* supports the resource preemption;

* allows defining a queue network based on infinite streams of data
  and their processors, where we can define a complex enough
  behaviour just in a few lines of code;

* supports the activity-oriented paradigm of DES;

* supports the basic constructs for the agent-based modeling;

* allows creating combined discrete-continuous models as all parts
  of the library are well integrated and this is reflected directly in
  the type system;

* the arrays of simulation variables are inherently supported;

* supports the Monte-Carlo simulation;

* the simulation model can depend on external parameters;

* uses extensively signals for notification;

* allows gathering statistics in time points;

* hides technical details in high-level simulation computations
  (with the corresponded support on level of the syntax).

Moreover, there are additional modules that allow:

* automating simulation experiments;

* saving the results in CSV files;

* plotting the deviation chart by rule 3-sigma, histogram, 
  time series, XY chart;

* collecting the summary of statistical data;

* parallel execution of the Monte-Carlo simulation.

The library is tested on Windows, OS X and Linux.

### License

Aivika for .NET is available under a dual-license model: GNU GPL
License v3 (GPLv3) and Commercial.

A Commercial Aivika license keeps your code proprietary where only you can
control and monetize on your end product’s development, user experience 
and distribution.

Aivika for .NET is also licensed under GPLv3. If you use Aivika under 
open-source license, you need to make sure that you comply with all 
the licenses of the components you use.

Please contact the author of this library for detail of licensing:
David Sorokin <mailto:david.sorokin@gmail.com>, Yoshkar-Ola, Russia.

### Installation

The library can be automatically installed with help of NuGet after 
accepting the license.

On Windows the library consists of the following two packages:

`PM> Install-Package Simulation.Aivika`
`PM> Install-Package Simulation.Aivika.Charting`

On Linux and OS X the charting component uses Gtk# already:

`PM> Install-Package Simulation.Aivika`
`PM> Install-Package Simulation.Aivika.Charting.Gtk`

Also the library can be manually installed. For that, please download 
the sources and compile them. There are two solutions. The first one 
is destined for Windows, while another solution with word `Gtk` in 
the name is destined for OS X and Linux.

### Documentation

The PDF documentation is available by the following link:

[Aivika 3 User Guide: Version for .NET Framework and Mono] (https://github.com/dsorokin/aivika/wiki/pdf/aivika-user-guide.pdf)

This document describes the simulation API and contains examples 
with graphical illustrations.

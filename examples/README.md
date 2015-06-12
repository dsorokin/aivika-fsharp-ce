
**Examples**

Here are simulation models created with help of Aivika for .NET.

There are three kinds of Visual Studio solutions inside each sub-directory
corresponding to some simulation model:

* solution `ExperimentRun.sln` launches a single simulation run and prints the 
  results in the terminal window;

* solution `Experiment.sln` launches on Windows a Monte-Carlo simulation,
  plots the charts, saves the results and generates an `HTML` file
  in a separate directory. Then this file can be opened in your favorite
  Internet browser;

* solution `ExperimentGtk.sln` does the same that the solution above does,
  but only the former is destined for OS X and Linux.
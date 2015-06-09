
***Examples***

Here are simulation models created with help of Aivika for .NET.

They are prepared as scripts to be launched from the terminal
or inside IDE. 

Please compile the Aivika sources and put them in the `bin` directory.
The scripts expect to find the Aivika assemblies in that directory.

The usual rule is as follows:

* script `Run.fsx` launches a single simulation run and prints the 
  results in the terminal window;

* script `RunExperiment.fsx` launches on Windows a Monte-Carlo simulation,
  plots the charts, saves the results and generates an `HTML` file
  in a separate directory. Then this file can be opened in your favorite
  Internet browser;

* script `RunExperimentGtk.fsx` does the same that the script above does,
  but only the former is destined for OS X and Linux.
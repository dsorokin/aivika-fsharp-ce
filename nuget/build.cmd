
mkdir Simulation.Aivika\lib
mkdir Simulation.Aivika\lib\net40

mkdir Simulation.Aivika.Charting\lib
mkdir Simulation.Aivika.Charting\lib\net40

mkdir Simulation.Aivika.Charting.Gtk\lib
mkdir Simulation.Aivika.Charting.Gtk\lib\net40

copy ..\LICENSE Simulation.Aivika
copy ..\LICENSE.GPLv3 Simulation.Aivika

copy ..\LICENSE Simulation.Aivika.Charting
copy ..\LICENSE.GPLv3 Simulation.Aivika.Charting

copy ..\LICENSE Simulation.Aivika.Charting.Gtk
copy ..\LICENSE.GPLv3 Simulation.Aivika.Charting.Gtk

copy ..\bin\Simulation.Aivika.dll Simulation.Aivika\lib\net40
copy ..\bin\Simulation.Aivika.xml Simulation.Aivika\lib\net40

copy ..\bin\Simulation.Aivika.Results.dll Simulation.Aivika\lib\net40
copy ..\bin\Simulation.Aivika.Results.xml Simulation.Aivika\lib\net40

copy ..\bin\Simulation.Aivika.Experiments.dll Simulation.Aivika\lib\net40
copy ..\bin\Simulation.Aivika.Experiments.xml Simulation.Aivika\lib\net40

copy ..\bin\Simulation.Aivika.Charting.dll Simulation.Aivika.Charting\lib\net40
copy ..\bin\Simulation.Aivika.Charting.xml Simulation.Aivika.Charting\lib\net40

copy ..\bin\Simulation.Aivika.Charting.Gtk.dll Simulation.Aivika.Charting.Gtk\lib\net40
copy ..\bin\Simulation.Aivika.Charting.Gtk.xml Simulation.Aivika.Charting.Gtk\lib\net40

nuget pack Simulation.Aivika\Simulation.Aivika.nuspec
nuget pack Simulation.Aivika.Charting\Simulation.Aivika.Charting.nuspec
nuget pack Simulation.Aivika.Charting.Gtk\Simulation.Aivika.Charting.Gtk.nuspec

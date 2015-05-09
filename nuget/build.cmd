
mkdir Simulation.Aivika\lib
mkdir Simulation.Aivika\lib\net40

copy ..\bin\Simulation.Aivika.dll Simulation.Aivika\lib\net40
copy ..\bin\Simulation.Aivika.XML Simulation.Aivika\lib\net40

nuget pack Simulation.Aivika\Simulation.Aivika.nuspec

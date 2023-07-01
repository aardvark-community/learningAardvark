I started this as a toy projekt to learn [Aardvark](https://github.com/aardvark-platform).

01.07.2023: After Updateing to Aarvark.rendering 5.3 and switching to Vulcan (Thanks to [aszabo](https://github.com/aszabo314)) I had to remove displacemnet mapping. It never worked to my satifaction and with Vulcan it broke completly. Maybe I will implement something better sometimes. 
On the other hand, transparency with refraction looks better now and I fixed abient cocclusion.

To realy "get" a platform of this scope and conceptual deep, I feel I need to implement a sufficient complex app. So I did what I wanted to do for a long time: A Rendering Demo exploring some modern shading ideas.

Most shading algorithms I have implementet at to moment are ports of the great tutuorials at [Lern OpenGL](https://learnopengl.com/).

At the moment the Demo features 
* deferred rendering
* phisical based rendering with image based abient diffuse and specular lightning
* varialble numbers of directional and point lights
* simple shadow maps
* normal mapping
* screen space ambient occlusion
* cloth shading (sheen)
* screen space subsurface scattering
* transluceny
* transparency with partial coverage and colored transmission
* refraction

The app uses [aardvark.media](https://github.com/aardvark-platform/aardvark.media) and I explored the adaptive elm-style UI concept a bit.

Saving and loading of scenes is also implemented, but the file handing is a bit primitve at then moment. Creating a new file is not implemented jet. Just create an empty file outside the app and select this in the save dialog.

Working with Aardvark is great fun and the best way to do 3d stuff with F# I know of. 

To build the app from the command line (e.g. in Visual Studio Code) run "../build.cmd" from the  SRC directory.  This is nessesary initially and after any changes to the domain types in the model. This will generate the addaptiv model in in Model.g.fs.

For all other changes you can just compile und run the app with "dotnet run -p AardvarkRenderDemo".

To make the compilation from the command line work, the following entry in fsproject file is nessesary:
```
  <PropertyGroup>
    <RunWorkingDirectory>$(OutputPath)\netcoreapp2.0</RunWorkingDirectory>
  </PropertyGroup>
```
Be carefull if you use Ionide in Visual Studio Code to edit the projet setup, e.g. th order of files: Ionide (resprectivly Forge used by Ionide) will not recognize this entry  and remove it when saving the project file.

A note on importing objects: I have  only tested that on some of my one models in .obj format. It may or may not work for other formats.

The project was started based on https://github.com/aardvark-platform/template

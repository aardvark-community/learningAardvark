I started this as a toy projekt to learn [Aardavark] (https://github.com/aardvark-platform).

To realy "get" a platform of this scope and conceptual deep, I feel I need to implement a sufficient complex app. So I did what I wanted to do for a long time: A Rendering Demo exploringg some modern shading ideas.

Most shading algorithems I have implementet at to moment are ports of the great tutuorials at [Lern OpenGL] (https://learnopengl.com/).

At the moment the Demeo features 
* deferred rednering
* phisical based rendering with image based abient diffuse and specular lightning
* varialbel numbers of directional and point lights
* simple shadow maps
* normal mapping
* displacement mapping
* screen space ambient occlusion

The app uses [aardvark.media] (https://github.com/aardvark-platform/aardvark.media) and I explored the adaptive elm-style UI concept a bit.

Saving aod loading of scenes is also implemented, but the file handing is a bit primitve at then moment. Creating a new file is not implemented jet. Just create an empty file outside eht app and select this in the save dialog.

Working with Aardvark is great fun and the best way to do 3d stuff with F# I know of. 

To build the app from the command line (e.g. in Visual Studio Code) run "../build.cmd" from the  SRC directory.  This is nessesary initially and after any changes to the domain types in the model. it will generate the addaptiv model in in Model.g.fs.

For all other changes you can just compile und run the app with "dotnet run -p aardvark_test".

To make the compilation from the command line work, the following entry in fsproject file is nessesary:
```
  <PropertyGroup>
    <RunWorkingDirectory>$(OutputPath)\netcoreapp2.0</RunWorkingDirectory>
  </PropertyGroup>
```
Be carefull if you use Ionide in Visual Studio Code to edit the projet setup, e.g. th order of files: Ionide (resprectivly Forge usede  my Ionid) will not recocnize this entry  and remove it.

The project was started based on https://github.com/aardvark-platform/template

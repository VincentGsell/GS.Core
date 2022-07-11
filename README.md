[![](https://tokei.rs/b1/github/VincentGsell/GS.Core?category=code)](https://github.com//VincentGsell/GS.Core)
[![](https://tokei.rs/b1/github/VincentGsell/GS.Core?category=files)](https://github.com//VincentGsell/GS.Core)
[![](https://tokei.rs/b1/github/VincentGsell/GS.Core?category=lines)](https://github.com//VincentGsell/GS.Core)
[![](https://tokei.rs/b1/github/VincentGsell/GS.Core?category=blanks)](https://github.com//VincentGsell/GS.Core)
[![](https://tokei.rs/b1/github/VincentGsell/GS.Core?category=comments)](https://github.com//VincentGsell/GS.Core)

# GS.Core
  Core functions shared by projects, advanced soft graphics capabilities, parser stuffs, threads gems and so on

# history

- 20220711
	- updates for fixes 
	- introduce gs.Languages, which is mainly R&D on parser and compilation. 
	- GS.Geomtry, GS.Pixel(32), GS.System and GS.Common domain have been massively update fellowing client need.
	
- 20200626 
	- Introduce GS.Soft3d.Pipeline : More advanced 3d raster, try to have clean architecture, fellowing pipeline one. 3dRasterDemo included.
	- Several rename, add GS.System.* demos, 
	- GS.Common.* enhancement, Twee langage parser (very early)
- 20200511 
	- 3DRasterStandAloneDemos - 3d Standalone : For learning purpose : All you need to 3d raster. The Raster part is powered by Pixel32
- 20200425 
	- Introducing 3D capabilities throught GS.Soft3D.Pipeline domain : Work in progress.
- 20200401 
	- Introduce Language domain.
- 20200301 
	- Introduce Pixels domain, with Pixel32 as implemenation (32Bits raster pixel libs).

- GS.Pixel
  Raster graphics system. Design to be simple, and build upon simple "software shader" technics.
  This lib's aim is to try to reach a good level of performance, but it first target efficiency and simplicity to achieve common advanced graphics tasks.

- GS.Language
  Common compiler technique - Work In progress.
  
- GS.JSON
  Another JSON lib. this one permit JsonToObject and ObjectToJson
  Based upon [this work](https://github.com/rilyu/json4delphi) 
  Enhanced for full unicode (/uxxx) support
  Simple "periodic element" added.
  
- GS.Reference
  It implement a persistance layer middleware for Key<>Value database.
  you will find *an exemple* of persister to, to build a fine  fast key<>value database : Just very usefull for setting, parameter, and even huge static data (or not)
  It can be used too to build message system, and so on : Please see GS.Bus Repo for a full Key Value working exemple implementation.
  
- GS.Thread.Pool
  Pur Thread pool with 2 final classes 
  --> Static Thread pool, which have *no warm* time at all. Work fairly fast as PPL dephi librady (iTask), but more versatil and adaptable, IMHO.
  --> Dynamic thread pool. Match well for server tools for exeample. Used in GS.GRID.
  This library is done for "micro service,Stateless fire-and-forget task - 
  Idling thread are freed, and when needed, new thread are summon, in a given quantity range.

- GS.FileOp
  A work in progress attempt to work on file with advanced tools. See Glacify demo, for exemple.	 
  Glacify is an full R&D project, witch aim to reach the fellowing features : 
  - in one file archiving (=a "glacier"), versioning capability, archiving by provide cyphering and compression, get clear file version   on demand, capabity to scan directory and only process changed files.

# Dependancy

- graphics (Pixel's sub domain) (directly used) : 
  - the *awesome* Clipper lib (by Angus Johnson - http://www.angusj.com/delphi/clipper.php)
  - Bero's magic with its polygon's triangulation stuffs build upon Clipper (https://github.com/BeRo1985)
  - Pixels unit : shine by its simplicity (https://github.com/sysrpl/ImageShop)
  - Delaunay unit, an old one, initialy by Paul Bourke (pbourke@swin.edu.au)
  - FastGeo, by Arash Partow (http://fastgeo.partow.net)
- other domain : 
  - [cool Json lib](https://github.com/rilyu/json4delphi)     
  - [the nice BasNCondingPascal](https://github.com/Xor-el/BaseNcodingPascal)

For your conveniance and for easy integration, those libs are availables on this repo as ThirdPart directory.

# Demo

  GS.Pixel derivated![Alt text](/../master/Ressources/Pixel32tease.png?raw=true "Pixel32 Demos")
  
  - 3D Raster demo, all in one unit, for teaching purpose
  https://github.com/VincentGsell/GS.Demos.3DRasterStandAlone
  

  GS.Json : 
  
  Periodic element read and write demo.
  -> you can too compare native JSON for delphi (witch is good) and GS.JSON system (which is cool to ;)
  
  FMX small demo App for Periodic element. 
  ![Alt text](/../master/Ressources/fmxjsonperiodicdemo.png?raw=true "FMX JSON Demo")
 
  GS.FileUtils  :   A GUI app to see how glacify process could work.
  ![Alt text](/../master/Ressources/glacify.png?raw=true "Glacify RnD project")
  
  GS.ReferenceDB : A little console app to experiment Reference capability.
  

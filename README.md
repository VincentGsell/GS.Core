# GS.Core
  Core functions shared by several projects 
  
- GS.JSON
  Another JSON lib. this one permit JsonToObject and ObjectToJson
  Based upon [this work](https://github.com/rilyu/json4delphi) 
  Enhanced for full unicode (/uxxx) support
  Simple "periodic element" added.
  
- GS.Reference
  It implement a persistance layer muddleware for Key<>Value database.
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

As this Core fundation class is dedicated to Bus and GRID app, dependancy with a lot of features coverage is needed : 

- [cool Json lib](https://github.com/rilyu/json4delphi)     
- [the nice BasNCondingPascal](https://github.com/Xor-el/BaseNcodingPascal)

For your conveniance and for easy integration, those libs are availables on this repo as ThirdPart directory.
  
# Demo

  GS.Json : 
  
  Periodic element read and write demo.
  -> you can too compare native JSON for delphi (witch is good) and GS.JSON system (which is cool to ;)
  
  FMX small demo App for Periodic element. 
  ![Alt text](/../master/Ressources/fmxjsonperiodicdemo.png?raw=true "FMX JSON Demo")
 
  GS.FileUtils  :   A GUI app to see how glacify process could work.
  ![Alt text](/../master/Ressources/glacify.png?raw=true "Glacify RnD project")
  
  GS.ReferenceDB : A little console app to experiment Reference capability.
  
  
  
  
  
  
  

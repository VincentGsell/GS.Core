# GS.Core
  Core functions shared by several projects 

- GS.Bus : 
  Implements threaded based minibus : Able to make inter thread communication easely. (See demo)
  Could be used for MVVM based architecture. GS.Bus is used in Grid System internal Grid Server.

- GS.LocalMemCached : 
  Mimic in an app MemCached server (in a way...)

- GS.Reference : 
  Base chunck for Key<>Value pair file storage system. Used in GS.LocalMemCached.
  
- GS.JSON
  Another JSON lib. this one permit JsonToObject and ObjectToJson
  Based upon [this work](https://github.com/rilyu/json4delphi) 
  Enhanced for full unicode (/uxxx) support
  Simple "periodic element" added.
  
- GS.Reference
  That is cool : It is a good exemple of an heritated object from GS.Bus : It implement an in memory Key<>Value database.
  you will find persister to, to build a fine very fast key<>value database : Just very usefull for psetting, parameter, and even huge static data (or not)
  It can be used too to build message system, and so on.
  
- GS.Thread 
  All thread stuff of the lib. Used in Task, Bus and reference.

# Threading stuff : 
  I do a lot of research in pascal threading, it is near to be more complicated than cpp:thread ;)
  As a result of this research, you have here 3 libraries, for 3 different usage : 
  This 3 librarie share the same paradigme : High inter thread communication capabilities.
  
  
- GS.Thread.Pool
  Pur Thread pool with 2 object
  --> Static Thread pool, which have *no warm* time at all. Work faily fast as PPL dephi librady (iTask), but much more versatil and adaptable.
  --> Dynamic thread pool. Match well for server tools for exeample. 
  This library is done for "micro service,Stateless fire-and-forget task - 
  Idling thread are freed, and when needed, new thread are summon, in a given quandtity range.
  
- GS.Bus.Service
  Service architecture : It is a pool of thread, which can communicate via bus, and build to be resident.
  Same thing than task, but better integration and faster warmup. Better ending methoid too.

- GS.Task
  Task management with communication system between task.
  Excelent to teach system and for heavy task. But slow to warm, and a bit deprecated since GS.Bus.Service. Use this last preferably.
  
  

# Demo

  Demo for GS.Bus and GS.Task are currently provided for Delphi 10.2 VCL
  
  GS.Bus : 
  
  ![Alt text](/../master/Ressources/GSBusBenchVisual.png?raw=true "GS.Bus Bench demo")
  
  GS.Task : 
  
  ![Alt text](/../master/Ressources/GTaskBenchVisu.png?raw=true "GS.Bus Bench demo")

  GS.JSON : 
  
  Periodic element read and write demo.
  -> you can too compare native JSON for delphi (witch is good) and GS.JSON system (which is cool to ;)
  

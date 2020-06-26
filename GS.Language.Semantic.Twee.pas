unit GS.Language.Semantic.Twee;

interface

uses classes,
     SysUtils,
     GS.Language.Compiler,
     Generics.Collections;

Type
  TTweeBook = class;                              //Project owner.
  TTweeStructure = class;
    TTweePassage = class;                         //Passage Data.
    TTweeMemory = class;                          //Momory bank, variable, object, array and so on. Shared.
  TTweeCode = class;                              //code related.
    TTweeStep = class;                            //Step, code chunck in a passage.
      TTweeStepDisplayText = class;                   //Pure Display Code instruction : To be render later.
      TTweeStepVarAssign = class;                 //Variable update/assign (set:...)
      TTweeStepFlow = class;                      //code flow directive.
        TTweeStepFlowGoto = class;         //if...
//        TTweeStepFlowConditional = class;         //if...
//        TTweeStepFlowDisplay = class;             //(display: ...) (got and run a passage, and display it in current passage.
//        TTweeStepFlowClickToPassage = class;      //Solid link to a passage, user must click. (i.e. [click!->[clickMe]]



  TTweeStructure = class
  public
    constructor Create; virtual;
    destructor destroy; override;
  end;

  TTweeStructureList = class(TObjectList<TTweeStructure>) //Memory keeper. All other list are pointer only.
  end;

  TTweeCode = Class
  End;

  TTweeStep = class(TTweeCode)
  end;

  TTweeMemory = class(TTweeStructure)
  private
    fName: String;
  public
    constructor Create(memName : string); reintroduce;
    property Name : String read fName;
  end;

  TTweeStepList = class(TObjectList<TTweeStep>)
  end;

  TTweeMemoryList = class(TObjectDictionary<string,TTweeMemory>)
  end;

  TTweePassageList = class(TObjectDictionary<string,TTweePassage>)
  end;

  TTweePassage = Class(TTweeStructure)
  protected
    fbook : TTweeBook;
    fPassageName : String;
    fCodes : TTweeStepList; //ordered code description.
  public
    LocalMemory : TTweeMemoryList;

    constructor Create(_book : TTweeBook; _passageName : String); reintroduce;
    destructor destroy; override;

    function addCode(_code : TTweeStep) : TTweeStep;

    property PassageName : String read fPassageName;
    property Book : TTweeBook read fBook;
    property Code : TTweeStepList read fCodes;
  End;

  TTweeStepDisplayText = class(TTweeStep)
  private
    fFormula: string;
  public
    Constructor Create(_formula : string); reintroduce;
    property formula : string read fFormula;
  end;

  TTweeStepVarAssign = class(TTweeStep)
  private
    fFormula: string;
    fVarname: string;
  public
    Constructor Create(_varName : string; _formula : string); reintroduce;
    property varname : string read fVarname;
    property formula : string read fFormula;
  end;

  TTweeStepFlow = class(TTweeStep)
  private
    ftarget: string;
  public
    Constructor Create(_target : string); reintroduce;
    property target : string read ftarget write ftarget;
  end;

  TTweeStepFlowGoto = class(TTweeStepFlow)
  end;

  TTweeStepFlowDisplay = class(TTweeStepFlow)
  end;





  TTweeBook = class
  private
    fPassages: TTweePassageList;
    fStoryTitle: String;
    //fStoryData : TTweeStoryMetaData (struct); //Todo.
    fStructure: TTweeStructureList;
    fGlobalMemory : TTweeMemoryList; //global var $MyVar and other global structure.
  public
    Constructor Create; virtual;
    Destructor Destroy; override;

    function addObject(struct : TTweeStructure) : TTweeStructure;

    property StoryTitle : String read fStoryTitle;

    property passages : TTweePassageList read fPassages;
    property Memory : TTweeMemoryList read fGlobalMemory;
    property AllObjects : TTweeStructureList read fStructure;
  end;



  TTweeSemanticProcessor = Class(TSemanticProcessor)
  private
    fBook: TTweeBook;
  public
    constructor Create(_root : TGSCRoot); virtual;
    destructor Destroy; override;

    property Book : TTweeBook read fBook;
  End;


implementation

{ TTweeBook }

function TTweeBook.addObject(struct: TTweeStructure): TTweeStructure;
begin
  result := struct;
  fStructure.Add(struct);
  if struct is TTweePassage then
  begin
    if Not fPassages.ContainsKey(TTweePassage(struct).PassageName) then
    begin
      fPassages.Add(TTweePassage(struct).PassageName,TTweePassage(struct))
    end
    else
      raise Exception.Create('TTweeBook.addObject : Error : try to add already existing Passage. ("'+TTweePassage(struct).PassageName+'")');
  end
  else
  if struct is TTweeMemory then
    fGlobalMemory.Add(TTweeMemory(struct).Name, TTweeMemory(struct));
end;

constructor TTweeBook.Create;
begin
  fStructure := TTweeStructureList.Create(True);
  fPassages := TTweePassageList.Create([]);
  fGlobalMemory := TTweeMemoryList.Create([]);
end;

destructor TTweeBook.Destroy;
begin
  FreeAndNil(fGlobalMemory);
  FreeAndNil(fPassages);
  FreeAndNil(fStructure);
  inherited;
end;

{ TTweeStructure }

constructor TTweeStructure.Create;
begin
  inherited; //here for syntax (general override for inherited stuffs)
end;

destructor TTweeStructure.destroy;
begin
  inherited; //here for syntax (general override for inherited stuffs)
end;

{ TTweePassage }

function TTweePassage.addCode(_code: TTweeStep): TTweeStep;
begin
  assert(assigned(_code));
  result := _code;
  fCodes.Add(_code);
end;

constructor TTweePassage.Create(_book : TTweeBook; _passageName : String);
var pass : string;
begin
  assert(assigned(_book));
  fbook := _book;
  pass := (trim(_passageName));
  assert(pass<>'');
  assert(Not Book.passages.ContainsKey(pass),'Passage "'+pass+'" already exists');
  inherited Create;

  fPassageName := pass;
  fCodes := TTweeStepList.Create(true); //ordered code description.
  LocalMemory := TTweeMemoryList.Create([doOwnsValues]); //var _myVar.
end;

destructor TTweePassage.destroy;
begin
  FreeAndNil(fcodes);
  FreeAndNil(LocalMemory);
  inherited;
end;

{ TTweeMemory }

constructor TTweeMemory.Create(memName: string);
begin
  inherited create;
  fName := memName;
end;

{ TTweeSemanticProcessor }

constructor TTweeSemanticProcessor.Create(_root: TGSCRoot);
begin
  inherited Create(_root);
  fBook := TTweeBook.Create;
end;

destructor TTweeSemanticProcessor.Destroy;
begin
  FreeAndNil(fBook);
  inherited;
end;

{ TTweeStepVarAssign }

constructor TTweeStepVarAssign.Create(_varName, _formula: string);
begin
  inherited Create;
  assert(length(_varName)>0);
  assert(length(_formula)>0);
  fVarname := _varName;
  fFormula := _formula;
end;

{ TTweeStepFlow }

constructor TTweeStepFlow.Create(_target: string);
begin
  inherited create;
  assert(length(_target)>0);
  ftarget := _target;
end;

{ TTweeStepDisplayText }

constructor TTweeStepDisplayText.Create(_formula: string);
begin
  inherited Create;
  assert(length(_formula)>0);
  fFormula := _formula;
end;

end.

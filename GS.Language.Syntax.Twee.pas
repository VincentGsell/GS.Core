unit GS.Language.Syntax.Twee;

interface

uses classes,
     SysUtils,
     GS.Language.WordTokenizer,
     GS.Language.Tokenizer.Twee,
     GS.Language.Compiler;


Type
  ///  SYNTAX ANALYZE :

  //  TWEE PROJECT HEADER

  TGSNode_Twee_Custom = class(TGSCWorkNode)
  public
  end;

  TGSCNode_Twee_ProjectHeader = class(TGSNode_Twee_Custom)
  private
    FStoryName: String;
  protected
    procedure InternalProcess; override;
  public
    procedure init; override;
    Property StoryName : String read FStoryName;
  end;

  TGSCNode_Twee_StoryData = class(TGSNode_Twee_Custom)
  private
    fIfid, fFormat, fFormatVersion, fStartPassageName: String;
    fide_Zoom : single;
    fPlainData: string;
  protected
    procedure InternalProcess; override;
  public
    procedure init; override;
    property plainData : string read fPlainData;
    Property Ifif : String read fIfid;
  end;

  TGSCNode_Twee_GenericPassage = class(TGSNode_Twee_Custom)
  private
    fPassageName: string;
    FMetaData: string;
    fPassageText: TStringList;
  protected
    procedure InternalProcess; override;
  public
    Constructor Create(_root : TGSCRoot; const _NodeName : String = ''; const comeFrom : TGSCNode = Nil); override;
    Destructor Destroy;

    procedure init; override;
    property PassageName : string read fPassageName;
    property MetaData : string read FMetaData;
    property PassageText : TStringList read fPassageText;
  end;

  TGSCNode_Twee_GenericPassage_CodeAnalytics = class(TGSNode_Twee_Custom)
  private
    fPassageObject: TObject;
  protected
    procedure InternalProcess; override;
  public
    Constructor Create(_root : TGSCRoot; _passage : TObject; const _NodeName : String = ''; const comeFrom : TGSCNode = Nil); reintroduce;
  end;

  //CA = code analytics.
  TGSCNode_Twee_GenericPassage_CA_VarAssignation = class(TGSCNode_Twee_GenericPassage_CodeAnalytics)
  private
  protected
    procedure InternalProcess; override;
  end;


  TTweeSyntaxCheck = Class(TSyntaxCheck)
  public
    constructor Create(_root : TGSCRoot; _tokenizer : TWordTokenizer); override;
    destructor Destroy; override;


  End;


implementation

Uses GS.Language.Parser.Twee,
     GS.Language.Semantic.Twee;

{ TGSCNode_Twee_ProjectHeader }

procedure TGSCNode_Twee_ProjectHeader.init;
begin
  inherited;
  FStoryName := 'Unknown';
end;

procedure TGSCNode_Twee_ProjectHeader.InternalProcess;
var lr : TGSCRootTwee;
    l : TTweeTokenItem;
begin
  lr := TGSCRootTwee(froot);
  l := lr.Tokenizer.TT.Next;

  Next := TGSCNode_Twee_StoryData.Create(froot);

  if lr.Tokenizer.TT.IsNextTokenSequenceConformTo([tttsymbol,tttsymbol,tttword]) then
  begin
    if lr.Tokenizer.TT.IsNextItemSequenceConformTo([':',':','StoryTitle']) then
    begin
      lr.Tokenizer.TT.Next;
      lr.Tokenizer.TT.Next;
      l := lr.Tokenizer.TT.Next; //The third is the real name.

      FStoryName := '';
      //Here we have to get the whole line
      FStoryName := lr.Tokenizer.code[l.TokenSource.line]; //Alakazam.
      //TODO : StoryName, tester si pas keyword !!

      //Jumpline.
      while not(lr.Tokenizer.TT.IsLast) and (lr.Tokenizer.TT.Current.TokenSource.line=l.TokenSource.line) do
        lr.Tokenizer.TT.Next;

    end
    else
    begin
      raise Exception.Create('Project header not found.');
    end;
  end
  else
  begin
    raise Exception.Create('Project header not correct : must be "word" indentifier only.');
  end;
end;



{ TGSCNode_Twee_StoryData }

procedure TGSCNode_Twee_StoryData.init;
begin
  inherited;
  fPlainData := '';
  fIfid := '';
  fFormat := '';
  fFormatVersion := '';
  fStartPassageName := '';
  fide_Zoom := 1.0;
end;

procedure TGSCNode_Twee_StoryData.InternalProcess;
var lr : TGSCRootTwee;
    l : TTweeTokenItem;
    finish : boolean;
begin
  lr := TGSCRootTwee(froot);
  l := lr.Tokenizer.TT.Current;

  Next := TGSCNode_Twee_GenericPassage.Create(froot,'',self);

  if lr.Tokenizer.TT.IsNextTokenSequenceConformTo([tttsymbol,tttsymbol,tttword, tttsymbol]) then
  begin
    if lr.Tokenizer.TT.IsNextItemSequenceConformTo([':',':','StoryData','{']) then
    begin
      lr.Tokenizer.TT.Next;
      lr.Tokenizer.TT.Next;
      lr.Tokenizer.TT.Next; //"{"
      fplainData := lr.Tokenizer.TT.getDataBetween('{','}');
    end
    else
    begin
      raise Exception.Create('Project header not found.');
    end;
  end
  else
  begin
    raise Exception.Create('Project header not correct : must be "word" indentifier only.');
  end;

end;

{ TTweeSyntaxCheck }

constructor TTweeSyntaxCheck.Create(_root: TGSCRoot;
  _tokenizer: TWordTokenizer);
begin
  inherited Create(_root, TWordTokenizer(_tokenizer));
  Start.First := TGSCNode_Twee_ProjectHeader.Create(_root,'Twee ProjectHeaader');
end;

destructor TTweeSyntaxCheck.Destroy;
begin

  inherited;
end;

{ TGSCNode_Twee_GenericPassage }

constructor TGSCNode_Twee_GenericPassage.Create(_root: TGSCRoot;
  const _NodeName: String; const comeFrom: TGSCNode);
begin
  inherited;
  fPassageText:= TStringList.create;
end;

destructor TGSCNode_Twee_GenericPassage.Destroy;
begin
  FreeAndNil(fPassageText);
  inherited;
end;

procedure TGSCNode_Twee_GenericPassage.init;
begin
  inherited;
  fPassageName := '';
  FMetaData := '';
end;

procedure TGSCNode_Twee_GenericPassage.InternalProcess;
var lr : TGSCRootTwee;
    l : TTweeTokenItem;
    i, lineBegin, lineEnd : Uint32;
    newPassage : TTweePassage;
begin
  lr := TGSCRootTwee(froot);
  if lr.Tokenizer.TT.IsLast then
    Exit;
  l := lr.Tokenizer.TT.Next;
  Next := nil;

  if lr.Tokenizer.TT.IsNextTokenSequenceConformTo([tttsymbol,tttsymbol,tttword]) then
  begin
    if lr.Tokenizer.TT.IsNextItemSequenceConformTo([':',':']) then
    begin
      lr.Tokenizer.TT.Next;
//      l := lr.Tokenizer.TT.Next;
      fPassageName := lr.Tokenizer.TT.getDataBetween('','{');
//      l := lr.Tokenizer.TT.Next;
      if lr.Tokenizer.TT.IsNextItemSequenceConformTo(['{']) then
        FMetaData := lr.Tokenizer.TT.getDataBetween('{','}');
      l := lr.Tokenizer.TT.Next;
      lineBegin := l.TokenSource.line; //Begin of content.

      lr.Tokenizer.TT.Push;

      while not(lr.Tokenizer.TT.IsLast) and Not(lr.Tokenizer.TT.IsNextItemSequenceConformTo([':',':'])) do
        lr.Tokenizer.TT.Next;

      l := lr.Tokenizer.TT.Previous;
      lineEnd := l.TokenSource.line;
      for i:= lineBegin to lineEnd do
        fPassageText.Add(lr.Tokenizer.code[i]);

      l := lr.Tokenizer.TT.Pop;
      newPassage := TTweePassage(lr.SemanticProcessor.Book.addObject(TTweePassage.Create(lr.SemanticProcessor.Book, fPassageName)));
      //--> analyse, and semantic injection.
      Next := TGSCNode_Twee_GenericPassage_CodeAnalytics.Create(froot,newPassage,'',Self);
    end
    else
    begin
      raise Exception.Create('Error Message');
    end;
  end;

end;

{ TGSCNode_Twee_GenericPassage_CodeAnalytics }

constructor TGSCNode_Twee_GenericPassage_CodeAnalytics.Create(_root: TGSCRoot;
  _passage: TObject; const _NodeName: String; const comeFrom: TGSCNode);
begin
  inherited Create(_root,_NodeName,comeFrom);
  assert(assigned(_passage));
  assert(_passage is TTweePassage);
  fPassageObject := _passage;
end;

procedure TGSCNode_Twee_GenericPassage_CodeAnalytics.InternalProcess;
var passage : TTweePassage;
var lr : TGSCRootTwee;
    l : TTweeTokenItem;
    varname : String;
    operand : String;
    formula : string;
    passagename :string;
    word : string;
    line : integer;
begin
  passage := TTweePassage(fPassageObject);
  lr := TGSCRootTwee(froot);
  if lr.Tokenizer.TT.IsLast then
    Exit;
  next := nil;
  l := lr.Tokenizer.TT.Current;

  if lr.Tokenizer.TT.IsNextTokenSequenceConformTo([tttsymbol,tttword, tttSymbol]) then
  begin
    if lr.Tokenizer.TT.IsNextItemSequenceConformTo(['(','set',':']) then
    begin //we are on "("
      lr.Tokenizer.TT.Next; //"Set"
      lr.Tokenizer.TT.Next; //":"

      next := TGSCNode_Twee_GenericPassage_CA_VarAssignation.Create(froot,passage,'',Self);
    end
    else
    if lr.Tokenizer.TT.IsNextItemSequenceConformTo(['(','go','-']) then
    begin //we are on "("
      lr.Tokenizer.TT.Next; //"go"
      lr.Tokenizer.TT.Next; //"-"
      lr.Tokenizer.TT.Next; //"to"
      lr.Tokenizer.TT.Next; //":"
      l := lr.Tokenizer.TT.Next;
      if l.ItemStringValue <> '"' then
        raise Exception.Create('Error Message');
      passageName := lr.Tokenizer.TT.getDataBetween('"','"');
      passage.addCode(TTweeStepFlowGoto.Create(passagename));
      l := lr.Tokenizer.TT.Next; //")"
      if l.ItemStringValue<>')' then
        raise Exception.Create('Error Message');

      l := lr.Tokenizer.TT.Next;
      next := Self;
    end
    else
    if lr.Tokenizer.TT.IsNextItemSequenceConformTo(['(','display',':']) then
    begin //we are on "("
      lr.Tokenizer.TT.Next; //"display"
      lr.Tokenizer.TT.Next; //":"
      l := lr.Tokenizer.TT.Next;
      if l.ItemStringValue <> '"' then
        raise Exception.Create('Error Message');
      passageName := lr.Tokenizer.TT.getDataBetween('"','"');
      passage.addCode(TTweeStepFlowDisplay.Create(passagename));
      l := lr.Tokenizer.TT.Next; //")"
      if l.ItemStringValue<>')' then
        raise Exception.Create('Error Message');

      l := lr.Tokenizer.TT.Next;
      next := Self;
    end
    else
    begin
      lr.log('[TGSCNode_Twee_GenericPassage_CodeAnalytics] WARNING : Not managed : '+lr.Tokenizer.TT.NextSequenceToString(3)+'...');

      while not(lr.Tokenizer.TT.IsLast) and not(lr.Tokenizer.TT.IsNextItemSequenceConformTo([':',':'])) do
        l := lr.Tokenizer.TT.Next;
      if not(lr.Tokenizer.TT.IsLast) then
      begin
        lr.Tokenizer.TT.Previous;
        Next := fcomeFrom;
      end;
    end;
  end
  else
  begin
    if not lr.Tokenizer.TT.IsNextItemSequenceConformTo([':',':']) then
    begin
      word := lr.Tokenizer.code[lr.Tokenizer.TT.Current.TokenSource.line];
      if word<>'' then
      begin
        line := lr.Tokenizer.TT.Current.TokenSource.col - length(lr.Tokenizer.TT.Current.ItemStringValue);
        //word := Copy(word,lr.Tokenizer.TT.Current.TokenSource.col,length(word)-lr.Tokenizer.TT.Current.TokenSource.col);
        word := Copy(word,line,length(word)-line);

        if pos('(display:',word)>0 then
        begin
          word := copy(word,1,pos('(display:',word)-1);
          while not(lr.Tokenizer.TT.IsLast) and not(lr.Tokenizer.TT.IsNextItemSequenceConformTo(['(','display',':'])) do
            l := lr.Tokenizer.TT.Next;
        end
        else
        begin
          //next line.
          line := lr.Tokenizer.TT.Current.TokenSource.line;
          while not(lr.Tokenizer.TT.IsLast) and not(lr.Tokenizer.TT.Current.TokenSource.line<>line) do
            l := lr.Tokenizer.TT.Next;
        end;
      end;
      passage.addCode(TTweeStepDisplayText.Create(word));
      Next := self;
    end;
  end;


  if not(lr.Tokenizer.TT.IsLast) and (next=nil) then
  begin
    l := lr.Tokenizer.TT.Previous;
    Next := fcomeFrom;
  end;
end;

{ TGSCNode_Twee_GenericPassage_CA_VarAssignation }

procedure TGSCNode_Twee_GenericPassage_CA_VarAssignation.InternalProcess;
var passage : TTweePassage;
var lr : TGSCRootTwee;
    l : TTweeTokenItem;
    varname : String;
    operand : String;
    formula : string;
begin
  passage := TTweePassage(fPassageObject);
  lr := TGSCRootTwee(froot);
  if lr.Tokenizer.TT.IsLast then
    Exit;

  l := lr.Tokenizer.TT.Current;

  l := lr.Tokenizer.TT.Next;  //variable name.
  varname := l.ItemStringValue;
  l := lr.Tokenizer.TT.Next; //opertator.
  operand := l.ItemStringValue;

  formula := '';
  l := lr.Tokenizer.TT.Next; //formula beginning.
  while not(lr.Tokenizer.TT.IsLast) and ((l.ItemStringValue <> ',') and (l.ItemStringValue <> ')')) do
  begin
    formula := formula + l.ItemStringValue;
    l := lr.Tokenizer.TT.Next;
  end;

  varname := (trim(varname));
  operand := lowercase(trim(operand));
  formula := (trim(formula));

  if (operand = '=') or (operand = 'to') then
  begin
    passage.addCode(TTweeStepVarAssign.Create(varname,formula));
  end
  else
  begin
    lr.log('[TGSCNode_Twee_GenericPassage_CodeAnalytics] WARNING : Operand Not managed : '+operand);
  end;

  if l.ItemStringValue = ',' then
    Next := self // :-/
  else
  begin
    l := lr.Tokenizer.TT.Next;
    Next := fcomeFrom;
  end;

end;

end.

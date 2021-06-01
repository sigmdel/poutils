unit mpofiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

(*

PO file entry as defined in chapter 3 The Format of PO Files
(https://www.gnu.org/software/gettext/manual/html_node/PO-Files.html#PO-Files)
of GNU gettext utilities

   white-space
   #  translator-comments
   #. extracted-comments
   #: reference…
   #, flag…
   #| msgctxt previous-context
   #| msgid previous-untranslated-string
   msgctxt context
   msgid untranslated-string
   msgstr translated-string

The format for the #, flag line is:

   #, fuzzy, format-string [, format-string]

To my knowledge, Lazarus never generates
     translator-comments
     extracted-commants
     previous-context
fields in PO files and fuzzy is the only flag used  }

*)

type

  { TPoEntry }

  TPoEntry = class
  private
    FTranslatorComments: TStrings; // #<space>
    FExtractedComments: TStrings;  // #.
    FReference: string;            // #:
    FFlag: string;                 // #,
    FPrevmsgctxt: string;          // #| msgctxt
    FPrevmsgid: TStrings;          // #| msgid
    FMsgctxt: string;              // msgctxt
    FMsgid: TStrings;              // msgid
    FMsgstr: TStrings;             // msgstr

    FIsFuzzy: boolean;
  protected
    procedure SetFuzzy(value: boolean);
  public
      { True if another entry has the same reference. This field is
        for use by the TPoFile.UpdateCount method only. }
    HasDuplicateReference: boolean;

      { True if another entry has the same msgid. This field is
        for use by the TPoFile.UpdateCount method only. }
    HasDuplicateMsgid: boolean;

      { True if another entry has the same msgstr. This field is
        for use by the TPoFile.UpdateCount method only. }
    HasDuplicateMsgstr: boolean;

    constructor Create;
    destructor Destroy; override;

      { Properties HasDuplicateReference, HasDuplicateMsgid,  HasDuplicateMsgstr,
        and IsAmbiguous are set to false, and the other fields are copied
        from source. }
    procedure Assign(source: TPoEntry);

       { Override of parent Equals to handle objects that are of type
         TPoEntry }
    function Equals(Obj: TObject): Boolean; override; overload;

      { Returns true if the entry and the source entry have the
        same reference, msgid and msgstr}
    function Equals(source: TPoEntry): boolean; overload;

      { Returns true if msgid in not empty }
    function HasMsgid: boolean;

      { Returns true if msgstr is not empty }
    function HasMsgstr: boolean;

      { Returns true if prevmsgid is not empty }
    function HasPrevmsgid: boolean;

      { PO entry translator-comments field }
    property TranslatorComments: TStrings read FTranslatorComments;

      { PO entry extracted-comments field }
    property ExtractedComments: TStrings read FExtractedComments;

      { PO entry reference field.
        In the Free Pascal / Lazarus implemenation this is a fully
        qualified name to a resource string (something like TForm1.Label3.caption)
        guaranteed to be unique if the application compiles.}
    property Reference: string read FReference;

      { PO entry flag field.
        Only the fuzzy flag is used here, but format flags will not be
        changed. This is a read only property, use the IsFuzzy property to
        add or remove "fuzzy" from the Flag property.}
    property Flag: string read FFlag;

      { PO entry previous-context field. }
    property Prevmsgctxt: string read FPrevmsgctxt;

      { PO entry previious-unstranslated-string field}
    property Prevmsgid: TStrings read FPrevmsgid;

      { PO entry context field.
        If present in a Free Pascal / Lazarus generated file, it will
        be the qualitifed name in Reference. The context is in quotes
        in the PO file, but they are stripped here}
    property Msgctxt: string read FMsgctxt;

      { PO entry untranslated-string field.
        These strings are quoted in the PO file but not in here}
    property Msgid: TStrings read FMsgId;

      { PO entry translated-string field.
        These strings are quoted in the PO file but not in here}
    property Msgstr: TStrings read FMsgstr;

    property IsFuzzy: boolean read FisFuzzy write SetFuzzy;
  end;

  { TPoFile }

  TPoFile = class
  private
    FList: TList;
    FFilename: string;
    FPrevmsgidCount: integer;
    FDuplicateReferenceCount: integer;
    FDuplicateMsgidCount: integer;
    FDuplicateMsgstrCount: integer;
    FMissingReferenceCount: integer;
    FErrorCount: integer;
    FFuzzyCount: integer;
    FEmptyMsgidCount: integer;
    FEmptyMsgstrCount: integer;
    FVerbose: boolean;
  protected
    function Compare(i,j: integer): integer;
    procedure Exchange(i, j: integer);
    function FoundMsgid(const value: TStrings; lastIndex: integer): integer;
    function FoundMsgstr(const value: TStrings; lastIndex: integer): integer;
    function FoundReference(const value: string; lastIndex: integer): integer;
    function GetCount: integer;
    function GetEntry(index: integer): TPoEntry;
    function LastEntry: TPoEntry;
    procedure LoadFile;
    procedure QuickSort(L, R: Integer);
    procedure SetFilename(const aFilename: string);
    procedure UpdateCounts;
  public
      { Creates an empty PO file object. By default Verbose is true.
        Set the filename property to load a PO file.) }
    constructor Create;

      { Creates a PO file object, sets the Verbose flag to false and then
        quietly loads aFilename}
    constructor Create(const aFilename: string);

    destructor Destroy; override;

      { Removes all PO Entries }
    procedure Clear;

      { Deletes the PO Entry at the specified Index}
    procedure Delete(Index: integer);

      { Searches all Entries from 0 to Index-1 and returns the index of
        the first of the searched entries that is equal to Entries[Index].
        If none of the previous entries is equal then returns -1.
        Entries are duplicates if they have the same reference, msgid and
        msgstr (see TPoEntry.Equals). It should be safe to eliminate such
        duplicates.}
    function FindDuplicateEntry(Index: integer): integer;

      { Returns true if Entries[0] is a header entry without reference,
        without msgid but with a msgstr. There is no check of the validity
        of the meta data in msgstr.}
    function HasHeader: boolean;

      { Searches all Entries starting at Index 0 and returns the Index
        of the first entry that has the same msgid. If none is found returns
        -1. The string comparison is case sensitive.}
    function IndexOfMsgid(const Value: TStrings): integer;

      { Searches all Entries starting at Index 0 and returns the Index
        of the first entry that has the same reference. If none is found returns
        -1. While references generated by the Lazarus IDE are always lower case,
        Pascal identifiers are case insensitive so the string comparison is case
        insensitive.}
    function IndexOfReference(const Value: string): integer;

      { Inserts a blank TPoEntry at the specified position in the Entries
        array. If Index = Count, then this effectively adds a blank TPoEntry.}
    function Insert(Index: integer): TPoEntry;

      { Save the Entries to a text file of the given name. If the trimmed
        aFilename is empty, the procedure does nothing. If the Filename
        property is blank then it is set to aFilename without calling LoadFile.}
    procedure SaveToFile(const aFilename: string);

      { Alphabetically sorts the Entries according to their reference. }
    procedure Sort;

      { Updates counts and writes summary statistics }
    procedure WriteStatistics(const filelabel: string; const aFilename: string = '');

      { Number of entries }
    property Count: integer read GetCount;

      { Number of entries with a reference already in the .po file. Empty references are ignored }
    property DuplicateReferenceCount: integer read FDuplicateReferenceCount;

      { Number of entries with a msgid already in the .po file. Empty msgid are ignored }
    property DuplicateMsgidCount: integer read FDuplicateMsgidCount;

      { Number of entries with a msgstr already in the .po file. Empty msgstr are ignored }
    property DuplicateMsgstrCount: integer read FDuplicateMsgstrCount;

      { Array of all TPoEntry Found in the file }
    property Entries[Index: integer]: TPoEntry read GetEntry; default;

      { Number of errors encountered in the LoadFile method }
    property ErrorCount: integer read FErrorCount;

      { Name of file containing the source of the object. It is set explicitely
        with the assignment Filename := aFilename implicetly with the
        constructor create(aFilename). In both cases the  or explicitely}
    property Filename: string read FFilename write SetFilename;

      { Number of Entries with the fuzzy attribute }
    property FuzzyCount: integer read FFuzzyCount;

      { Number of entries with a empty msgid }
    property EmptyMsgidCount: integer read FEmptyMsgidCount;

      { Number of entries with a empty msgstr }
    property EmptyMsgstrCount: integer read FEmptyMsgstrCount;

      { Number of entries that do not have a reference. }
    property MissingReferenceCount: integer read FMissingReferenceCount;

      { Number of entries with an alternate msgid }
    property PrevmsgidCount: integer read FPrevmsgidCount;

      { Flag that determines if LoadFile is verbose or not. See constructors. }
    property Verbose: boolean read FVerbose write FVerbose;
  end;


implementation

{ TPoEntry }


constructor TPoEntry.Create;
begin
  inherited create;
  FTranslatorComments := TStringList.create;
  FExtractedComments := TStringList.create;
  Fprevmsgid := TStringList.create;
  Fmsgid := TStringList.create;
  Fmsgstr := TStringList.create;
end;

destructor TPoEntry.Destroy;
begin
  msgstr.free;
  msgid.free;
  prevmsgid.free;
  extractedcomments.free;
  translatorcomments.free;
  inherited destroy;
end;

procedure TPoEntry.Assign(source: TPoEntry);
begin
  HasDuplicateReference := false;
  HasDuplicateMsgid := false;
  HasDuplicateMsgstr := false;
  FIsFuzzy := source.IsFuzzy;

  FTranslatorComments.assign(source.translatorcomments);
  FExtractedComments.assign(source.extractedcomments);
  FReference := source.Reference;
  FFlag := source.Flag;
  FPrevmsgctxt := source.prevmsgctxt;
  Fmsgctxt := source.msgctxt;
  Fmsgid.assign(source.msgid);
  Fmsgstr.assign(source.msgstr);
  Fprevmsgid.assign(source.prevmsgid);
end;

function TPoEntry.Equals(Obj: TObject): Boolean;
begin
  if Obj is TPoEntry then
    Result := Equals(TPoEntry(Obj))
  else
    Result := inherited Equals(Obj);
end;

function TPoEntry.Equals(source: TPoEntry): boolean;
begin
  result := (Reference = source.Reference) and msgid.Equals(source.msgid)
    and msgstr.Equals(source.msgstr)
end;

function TPoEntry.HasPrevmsgid: boolean;
begin
  result := ((prevmsgid.Count = 1) and ( prevmsgid[0] <> '')) or (prevmsgid.Count > 1);
end;

function TPoEntry.HasMsgid: boolean;
begin
  result := ((msgid.Count = 1) and ( msgid[0] <> '')) or (msgid.Count > 1);
end;

function TPoEntry.HasMsgstr: boolean;
begin
  result := ((msgstr.Count = 1) and ( msgstr[0] <> '')) or (msgstr.Count > 1);
end;

procedure TPoEntry.SetFuzzy(value: boolean);
var
  i: integer;
  sl: TStrings;
  wd: string;
begin
  i := pos('fuzzy', FFlag);
  if (i > 0) and not value then begin
    FIsFuzzy := false;
    sl := TStringList.create;
    try
      i := 1;
      repeat
        wd := trim(ExtractWord(i, FFlag, [',']));
        inc(i);
        if wd = 'fuzzy' then
          continue
        else if wd <> '' then
          sl.add(wd);
      until wd = '';
      if sl.count = 0 then
        FFlag := ''
      else begin
        FFlag := sl[0];
        for i := 1 to sl.count-1 do
          FFlag := FFlag + ', ' + sl[i];
      end;
    finally
      sl.free;
    end;
  end
  else if (i < 0) and value then begin
    if length(FFlag) > 0 then
      FFlag := 'fuzzy, ' + FFlag
    else
      FFlag := 'fuzzy';
    FIsFuzzy := true;
  end;
end;

{ TPoFile }

constructor TPoFile.Create;
begin
  inherited;
  FList := TList.Create;
  Clear;
  FVerbose := true;
end;

constructor TPoFile.Create(const aFilename: string);
begin
  create;
  FVerbose := false;
  FFilename := trim(aFilename);
  if FFilename <> '' then
    LoadFile;
end;

destructor TPoFile.Destroy;
begin
  Clear;
  FList.free;
  inherited destroy;
end;

procedure TPoFile.Clear;
var
  i: integer;
begin
  for i := FList.Count-1 downto 0 do
    TPoEntry(FList[i]).Free;
  FList.Clear;
  FPrevmsgidCount := -1;
  FDuplicateReferenceCount := -1;
  FDuplicateMsgidCount := -1;
  FDuplicateMsgstrCount := -1;
  FMissingReferenceCount := -1;
  FErrorCount := -1;
  FFuzzyCount := -1;
  FEmptyMsgidCount := -1;
  FEmptyMsgstrCount := -1;
end;

function TPoFile.Compare(i,j: integer): integer;
// compare function for sort
begin
  // Possible comparisons of two references

  //result := CompareStrt(Entries[i].Reference, Entries[j].Reference); // case sensitve, ASCII only
  //result := CompareText(Entries[i].Reference, Entries[j].Reference);  // case insensitive, ASCII only
  //result := AnsiCompareStr(Entries[i].Reference, Entries[j].Reference); // case sensitve, ignore accents
  result := AnsiCompareText(Entries[i].Reference, Entries[j].Reference); // case insensitive, ignore accents
  //result := UnicodeCpareStr(Entries[i].Reference, Entries[j].Reference); // case sensitive  needs unit cwstrings
  //result := UnicodeCompareText(Entries[i].Reference, Entries[j].Reference); // case insensitive
end;

procedure TPoFile.Delete(Index: integer);
begin
  TPoEntry(FList[Index]).free;
  Flist.delete(Index);
end;

procedure TPoFile.Exchange(i, j: integer);
var
  temp: TPoEntry;
begin
  if i = j then
    exit;
  temp := TPoEntry.create;
  temp.assign(Entries[i]);
  Entries[i].assign(Entries[j]);
  Entries[j].assign(temp);
  temp.free;
end;

function TPoFile.FindDuplicateEntry(Index: integer): integer;
var
  i: integer;
begin
  for i := 0 to Index-1 do begin
    if Entries[i].Equals(Entries[Index]) then begin
      result := i;
      exit;
    end;
  end;
  result := -1;
end;

function TPoFile.HasHeader: boolean;
begin
  result := (Count > 0) and not Entries[0].Hasmsgid;
end;

function TPoFile.FoundReference(const Value: string; lastIndex: integer): integer;
var
  i: integer;
begin
  for i := 0 to lastIndex-1 do
    if AnsiSameText(Entries[i].Reference, Value) then begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TPoFile.FoundMsgid(const value: TStrings; lastIndex: integer): integer;
var
  i: integer;
begin
  for i := 0 to lastIndex-1 do
    if Entries[i].msgid.Equals(value) then begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TPoFile.FoundMsgstr(const value: TStrings; lastIndex: integer): integer;
var
  i: integer;
begin
  for i := 0 to lastIndex-1 do
    if Entries[i].msgstr.Equals(value) then begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TPoFile.GetCount: integer;
begin
  result := FList.Count;
end;

function TPoFile.GetEntry(Index: integer): TPoEntry;
begin
  result := TPoEntry(FList[Index]);
end;

function TPoFile.IndexOfReference(const Value: string): integer;
begin
  result := FoundReference(Value, Count);
end;

function TPoFile.IndexOfMsgid(const Value: TStrings): integer;
begin
  result := FoundMsgId(value, count);
end;

function TPoFile.Insert(Index: integer): TPoEntry;
begin
  result := TPoEntry.create;
  FList.Insert(Index, result);
end;

procedure TPoFile.QuickSort(L, R: Integer);
var
  Pivot, vL, vR: Integer;
begin
  if R - L <= 1 then begin // a little bit of time saver
    if L < R then
      if Compare(L, R) > 0 then
        Exchange(L, R);
    Exit;
  end;

  vL := L;
  vR := R;

  Pivot := L + Random(R - L); // they say random is best

  while vL < vR do begin
    while (vL < Pivot) and (Compare(vL, Pivot) <= 0) do
      Inc(vL);

    while (vR > Pivot) and (Compare(vR, Pivot) > 0) do
      Dec(vR);

    Exchange(vL, vR);

    if Pivot = vL then // swap pivot if we just hit it from one side
      Pivot := vR
    else if Pivot = vR then
      Pivot := vL;
  end;

  if Pivot - 1 >= L then
    QuickSort(L, Pivot - 1);
  if Pivot + 1 <= R then
    QuickSort(Pivot + 1, R);
end;

function TPoFile.LastEntry: TPoEntry;
begin
  result := TPoEntry(FList[FList.Count-1]);
end;

type
  TReadStatus = (
    rsIdle,  // starting
    rsTranslatorComments,
    rsExtractedComments,
    rsReference,
    rsFlag,
    rsPrevMsgctxt,
    rsPrevMsgid,
    rsMsgctxt,
    rsMsgId,
    rsMsgstr,
    rsError);

procedure TPoFile.LoadFile;
var
  src: TextFile;
  currentLineNumber: integer;
  currentLine: string;  // trimmed current line from src
  status: TReadStatus;

  procedure Report(const msg: string);
  begin
    if FVerbose then begin
      writeln(FErrorCount:6, '  Error: ', msg, ' in line ', currentLineNumber)
    end;
  end;

  procedure CheckLastEntry;
  begin
    if Count < 1 then exit;
    if (Count = 1) and (Entries[0].Reference = '') then with Entries[0] do begin
      if HasMsgid then begin
        inc(FErrorCount);
        Report('Entry 0 with no reference has an msgid');
      end;
      if not HasMsgstr then begin
        inc(FErrorCount);
        Report('Entry 0 with no reference does not have a msgstr');
      end;
    end
    else with Entries[Count-1] do begin
      if Reference = '' then begin
        inc(FErrorCount);
        Report(Format('Entry %d does not have a reference', [Count-1]));
      end;
      if not HasMsgid then begin
        inc(FErrorCount);
        Report(Format('Entry %d (%s) does not have a msgid', [Count-1, Reference]));
      end;
    end;
  end;

  procedure AddEntry;
  begin
    CheckLastEntry;
    Insert(Count);
  end;

  function StartsWith(const value: string): boolean;
  begin
    result := value = copy(currentLine, 1, length(value));
  end;

  procedure ReadTranslatorComments;
  {
   # a comment
  }
  begin
    system.delete(currentline, 1, 2);
    if status in [rsIdle, rsMsgId, rsMsgstr, rsError] then
      AddEntry;
    LastEntry.TranslatorComments.add(trim(currentLine));
    status := rsTranslatorComments;
  end;

  procedure ReadExtractedComments;
  {
   #. a comment
  }
  begin
    system.delete(currentline, 1, 2);
    if status in [rsIdle, rsMsgId, rsMsgstr, rsError] then
      AddEntry;
    LastEntry.ExtractedComments.add(trim(currentLine));
    status := rsExtractedComments;
  end;

  procedure ReadFlag;
  (*
  #, flag {, flag}

  where flag = fuzzy | format-string
  *)
  begin
    system.delete(currentLine, 1, 2); // delete leading #,
    currentLine := trim(currentLine);
    if length(currentLine) < 1 then begin
      status := rsError;
      inc(FErrorCount);
      Report('missing flag (fuzzy or format-string)');
      exit;
    end;
    if status in [rsIdle, rsMsgId, rsMsgstr, rsError] then
      AddEntry;
    LastEntry.FFlag := currentLine;
    LastEntry.FIsFuzzy := pos('fuzzy', currentLine) > 0;
    status := rsFlag;
  end;

  procedure ReadReference;
  {
   #: tform1.caption
  }
  begin
    system.delete(currentLine, 1, 2); // skip #:
    if status in [rsIdle, rsMsgId, rsMsgstr, rsError] then
      AddEntry
    else if LastEntry.FReference <> '' then begin
      inc(FErrorCount);
      Report('Reference already defined');
    end;
    LastEntry.FReference := trim(currentLine); // removes any leading space
    if LastEntry.Reference = '' then begin
      inc(FErrorCount);
      Report('Reference empty');
    end;
    status := rsReference;
  end;

  procedure ReadPrevious;
  {
  #| msgctxt "oldctxt"
    or
  #| msgid "oldid"
    or
  #| msgid ""
  #| "Default names\n"
  #| "(%d is number)\n"
  }
  var
    q, n: integer;
  begin
    if status in [rsIdle, rsMsgId, rsMsgstr, rsError] then
      AddEntry;
    system.delete(currentLine, 1, 2); // eliminate #|
    currentLine := trim(currentLine);
    if startsWith('msgctxt') then begin
      status := rsPrevMsgctxt;
      if (LastEntry.Prevmsgctxt <> '') then begin
        inc(FErrorCount);
        Report('More than one #| msgctxt');
        // carry on
      end;
    end
    else if startsWith('msgid') then begin
      status := rsPrevmsgId;
      if LastEntry.HasPrevmsgid then begin
        inc(FErrorCount);
        Report('More than one #| msgid');
        // carry on
      end;
    end
    else if status <> rsPrevmsgId  then begin
      inc(FErrorCount);
      Report('Previous context should be only one line');
      status := rsError;
      exit;
    end;

    q := pos('"', currentLine);
    if q < 1 then begin
      inc(FErrorCount);
      status := rsError;
      Report('Missing leading " quote in prevmsgid');
      exit;
    end;
    system.delete(currentLine, 1, q);

    n := length(currentLine);
    if currentLine[n] = '"' then
      system.delete(currentLine, n, 1)
    else begin
      inc(FErrorCount);
      Report('Missing trailing " quote in prevmsgid');
      // carry on as if ok
    end;
    if status = rsPrevmsgId then
      LastEntry.prevmsgid.Add(currentLine)
    else if status = rsPrevmsgctxt then begin
      LastEntry.Fprevmsgctxt := currentLine;
      if currentLine = '' then begin
        inc(FErrorCount);
        Report('Missing previous msgctxt');
      end;
    end;
  end;

  {ReadExtraLines
    #: appconsts.ssavefilechanges
    msgid ""
    "Save changes to\n"
    "%currentLine\n"
    "before proceeding?\n"
    msgstr ""
  }

  procedure ReadExtraLine;
  var
    n: integer;
  begin
    system.delete(currentLine, 1, 1); // delete first "
    n := length(currentLine);
    if currentLine[n] = '"' then
      system.delete(currentLine, n, 1);
    if status = rsmsgId then
      LastEntry.msgid.Add(currentLine)
    else if status = rsMsgstr then
      LastEntry.msgstr.Add(currentLine)
    else if status = rsPrevmsgid then
      LastEntry.prevmsgid.Add(currentLine)
    else begin
      status := rsError;
      inc(FErrorCount);
      Report('Line starting with " quote while not in msgid, msgstr or altmsgid section');
    end;
  end;

  procedure ReadMsgctxt;
  var
    q, n: integer;
  begin
    if status in [rsIdle, rsMsgId, rsMsgstr, rsError] then
      AddEntry;
    q := pos('"', currentLine);
    if q < 1 then begin
      inc(FErrorCount);
      status := rsError;
      Report('Missing leading " quote in msgctx');
      exit;
    end;
    system.delete(currentLine, 1, q);
    n := length(currentLine);
    if currentLine[n] = '"' then
      system.delete(currentLine, n, 1)
    else begin
       inc(FErrorCount);
       Report('Missing trailing " quote in msgctx');
       // carry on as if ok
    end;
    LastEntry.FMsgctxt := trim(currentLine);
    status := rsMsgctxt;
  end;

  procedure ReadMsgid;
  var
    q, n: integer;
  begin
    if status in [rsIdle, rsMsgstr, rsError] then begin
      AddEntry;   // starting new entry
    end;

    // msg must start with " quote
    q := pos('"', currentLine);
    if q < 1 then begin
      inc(FErrorCount);
      status := rsError;
      Report('Missing leading " quote in msgid');
      exit;
    end;
    system.delete(currentLine, 1, q); // ignore 'msgid "'
    n := length(currentLine);
    if currentLine[n] = '"' then
      system.delete(currentLine, n, 1)
    else begin
      inc(FErrorCount);
      Report('Missing trailing " quote in msgid');
      // carry on as if ok
    end;
    LastEntry.msgid.add(currentLine);
    status := rsMsgid;
  end;

  procedure ReadMsgstr;
  var
    q, n: integer;
  begin
    if status in [rsIdle, rsError] then
      AddEntry;
    q := pos('"', currentLine);
    if q < 1 then begin
      inc(FErrorCount);
      status := rsError;
      Report('Missing leading " quote in msgstr');
      exit;
    end;
    system.delete(currentLine, 1, q);
    n := length(currentLine);
    if currentLine[n] = '"' then
      system.delete(currentLine, n, 1)
    else begin
       inc(FErrorCount);
       Report('Missing trailing " quote in msgstr');
       // carry on as if ok
     end;
    LastEntry.msgstr.add(currentLine);
    //Writeln('dbg: Added msgstr in line ', currentLineNumber);
    status := rsMsgstr;
  end;

begin
  if not fileexists(Filename) then begin
    if fileexists(Filename + '.po') then
      FFilename := FFilename + '.po'
    else
      Raise Exception.CreateFmt('"%s" does not exist', [Filename]);
  end;

  status := rsIdle;
  FErrorCount := 0;
  assign(src, Filename);
  currentLineNumber := 0;
  try
    reset(src);
    while not eof(src) do begin
      readln(src, currentLine);
      inc(currentLineNumber);
      currentLine := trim(currentLine);
      if currentLine = '' then
        continue
      else if currentLine = '#' then
        currentLine := '# ';             // allow empty translator comment
      //writeln('dbg: ', currentline);
      if currentLine[1] = '"' then
        ReadExtraLine
      else if currentLine[1] = '#' then begin
        if length(currentLine) < 2 then begin
          status := rsError;
          inc(FErrorCount);
          Report('Missing operand after #');
          continue;
        end;
        case currentLine[2] of
          ' ': ReadTranslatorComments;
          '.': ReadExtractedComments;
          ',': ReadFlag;
          ':': ReadReference;
          '|': ReadPrevious;
        else
          begin
            status := rsError;
            inc(FErrorCount);
            Report('Unknown operand afer #');
            continue;
          end;
        end;
      end
      else if StartsWith('msgctxt') then
        ReadMsgctxt
      else if StartsWith('msgid') then
        ReadMsgid
      else if StartsWith('msgstr') then
        ReadMsgstr
      else begin
        status := rsError;
        inc(FErrorCount);
        Report('Unknow msg identifier');
      end;
    end;
  finally
    CheckLastEntry;
    closefile(src);
  end;
end;

procedure TPoFile.SaveToFile(const aFilename: string);
var
  dst: Textfile;
  i,j: integer;
begin
  if trim(aFilename) = '' then exit;
  if FFilename = '' then
    FFilename := trim(aFilename);
  assign(dst, trim(aFilename));
  try
    rewrite(dst);
    for i := 0 to Count-1 do
      with Entries[i] do begin
        // dbg:: writeln(Format('Writing entry %d, reference: <%s>', [i, reference]));
        if i > 0 then
          writeln(dst);

        for j := 0 to TranslatorComments.count-1 do
          writeln(dst, '# ', TranslatorComments[j]);

        for j := 0 to ExtractedComments.count-1 do
          writeln(dst, '#. ', ExtractedComments[j]);

        if Reference <> '' then
          writeln(dst, '#: ', Reference);

        if Flag <> '' then
          writeln(dst, '#, ', Flag);

        if Prevmsgctxt <> '' then
          writeln(dst, '#| msgctxt "', Prevmsgctxt, '"');

        if (prevmsgid.Count > 0) then begin
          writeln(dst, '#| msgid "', Prevmsgid[0], '"');
          for j := 1 to prevmsgid.count-1 do
            writeln(dst, '#| "', Prevmsgid[j], '"');
        end;

        if msgctxt <> '' then
          writeln(dst, 'msgctxt "', Msgctxt, '"');

        if (msgid.count = 0) then
          writeln(dst, 'msgid ""')
        else begin
          writeln(dst, 'msgid "', msgid[0], '"');
          for j := 1 to msgid.count-1 do
            writeln(dst, '"', msgid[j], '"');
        end;

        if (msgstr.count = 0) then
          writeln(dst, 'msgstr ""')
        else begin
          writeln(dst, 'msgstr "', Msgstr[0], '"');
          for j := 1 to msgstr.count-1 do
              writeln(dst, '"', Msgstr[j], '"');
        end;
        {
         #: dmulist.sinvalidperiods
         #, fuzzy
         msgctxt "dmulist.sinvalidperiods"
         msgid "%d is an invalid number of periods"
         msgstr "Nombre de périodes, %d, incorrect"
         }
      end;
  finally
    closefile(dst);
  end;
end;

procedure TPoFile.SetFilename(const aFilename: string);
begin
  if AnsiCompareFilename(aFilename, FFilename) = 0 then exit;
  FFilename := aFilename;
  Clear;
  if FFilename <> '' then
    LoadFile
end;

procedure TPoFile.Sort;
var
  first: integer;
begin
  if Count < 2 then exit;
  if HasHeader then
    first := 1
  else
    first := 0;
  QuickSort(first, FList.Count-1);
end;

procedure TPoFile.WriteStatistics(const filelabel: string; const aFilename: string);
var
  Ecount: integer;

  procedure writeStat(const msg: string; value: integer);
  begin
    if (value > 0) then
      writeln('  ', msg, ': ', value, Format(' (%.1f%%)', [100*value/Ecount]))
    else if Verbose then
      writeln('  ', msg, ': ', value)
  end;

begin
  UpdateCounts;
  write(Filelabel, ': ');
  if aFilename <> '' then
    writeln(aFilename)
  else
    writeln(Filename);
  writeln('  Errors: ', ErrorCount);

  if hasHeader then begin
    Ecount := Count-1;
    writeln('  Entries: ', Ecount, ' plus a header');
  end
  else begin
    Ecount := Count;
    writeln('  Entries: ', Ecount, ' and no header');
  end;

  writestat('Missing references', MissingReferenceCount);
  writestat('Duplicate references', DuplicateReferenceCount);

  writestat('Empty msgids', EmptyMsgidCount);
  writestat('Duplicate msgids', DuplicateMsgidCount);
  if Verbose then
     writestat('Previous msgids', PrevMsgidCount);

  writestat('Empty msgstrs', EmptyMsgstrCount);
  writestat('Duplicate msgstrs', DuplicateMsgstrCount);

  writestat('Fuzzys', FuzzyCount);
end;

procedure TPoFile.UpdateCounts;
var
 i, first: integer;
begin
  FPrevmsgidCount := 0;
  FDuplicateReferenceCount := 0;
  FDuplicateMsgidCount := 0;
  FDuplicateMsgstrCount := 0;
  FMissingReferenceCount := 0;
  //FErrorCount := 0;
  FFuzzyCount := 0;
  FEmptyMsgidCount := 0;
  FEmptyMsgstrCount := 0;

  if HasHeader then
    first := 1
  else
    first := 0;
  for i := first to Count-1 do begin
    if (Entries[i].Reference = '') then
      inc(FMissingReferenceCount)
    else
      Entries[i].HasDuplicateReference := FoundReference(Entries[i].Reference, i) >= 0;

    if Entries[i].HasMsgid then begin
      Entries[i].HasDuplicateMsgid :=  FoundMsgId(Entries[i].msgid, i) >= 0;
    end
    else
      inc(FEmptyMsgidCount);

    if Entries[i].HasMsgstr then
      Entries[i].HasDuplicateMsgstr := FoundMsgstr(Entries[i].msgstr, i) >= 0
    else
      inc(FEmptyMsgstrCount);

    if Entries[i].HasPrevmsgid then
      inc(FPrevmsgidCount);
    if Entries[i].HasDuplicateReference then
      inc(FDuplicateReferenceCount);
    if Entries[i].HasDuplicateMsgid then
      inc(FDuplicateMsgIdCount);
    if Entries[i].HasDuplicateMsgstr then
      inc(FDuplicateMsgstrCount);
    if Entries[i].IsFuzzy then
      inc(FFuzzyCount);
  end;
end;

end.


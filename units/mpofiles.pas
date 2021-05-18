unit mpofiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPoEntry = class
  public
      { The elements of a Lazarus .po file entry.
        To my knowledge, there are no
          translator-comments // # xxxxxx
          extracted-commants  // #.
          previous-context    // #| msgctxt "xx"
        fields in Lazarus .po files.
        Furthermore, fuzzy is the only flag used  }
    Reference: string;   // #: xx
    IsFuzzy: boolean;    // #, fuzzy
    msgctxt: string;     // msgctxt "xx"
    msgid: TStrings;     // msgid "xx"
    msgstr: TStrings;    // msgstr "xx"
    prevmsgid: TStrings; // #| msgid "xx"

      { True if another entry has the same reference (*) }
    HasDuplicateReference: boolean;

      { True if another entry has the same msgid (*) }
    HasDuplicateMsgid: boolean;

      { True if another entry has the same msgstr (*) }
    HasDuplicateMsgstr: boolean;

      { True if another entry is the same msgid and msgctxt (*) }
    IsAmbiguous: boolean;

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

      { Returns true if this entry and source have the
        same msgid and the same msgctxt. These two entries
        are said to be ambigous. If the entry has an empty
        msgid, the result is false even if the source msgid and msgctxt
        are the same}
    function Same(source: TPoEntry): boolean;

      { Returns true if msgid has no lines or contains a single empty line }
    function HasMsgid: boolean;

      { Returns true if msgid has no lines or contains a single empty line }
    function HasMsgstr: boolean;

      { Returns true if prevmsgid has no lines or contains a single empty line }
    function HasPrevmsgid: boolean;
  end;

  { TPoFile }

  TPoFile = class
  private
    FList: TList;
    FFilename: string;
    FAmbiguousCount: integer;
    FPrevmsgidCount: integer;
    FDuplicateReferenceCount: integer;
    FDuplicateMsgidCount: integer;
    FDuplicateMsgstrCount: integer;
    FMissingReferenceCount: integer;
    FErrorCount: integer;
    FFuzzyCount: integer;
    FEmptyMsgidCount: integer;
    FEmptyMsgstrCount: integer;
    FReportError: boolean;
  protected
    function Compare(i,j: integer): integer;
    procedure Exchange(i, j: integer);
    function FoundMsgid(const value: TStrings; lastIndex: integer): integer;
    function FoundMsgstr(const value: TStrings; lastIndex: integer): integer;
    function FoundReference(const value: string; lastIndex: integer): integer;
    function FoundSame(index: integer): boolean;
    function GetCount: integer;
    function GetEntry(index: integer): TPoEntry;
    function LastEntry: TPoEntry;
    procedure LoadFile;
    procedure QuickSort(L, R: Integer);
    procedure SetFilename(const aFilename: string);
    procedure UpdateCounts;
  public
    constructor Create;
    constructor Create(const aFilename: string);
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(index: integer);

      { Searches all Entries and returns the index of item equal to Items[index] }
    function FindDuplicateEntry(index: integer): integer;

    function HasHeader: boolean;

    function IndexOfMsgid(const value: TStrings): integer;

      { Searches all Entries[i] the index of the first entry with an
        reference equal to value }
    function IndexOfReference(const value: string): integer;

      { Inserts a blank TPoEntry at the specified position}
    function Insert(index: integer): TPoEntry;

      { Save the Entries to a text file of the given name. If the trimmed
        aFilename is empty, the procedure does nothing. If the Filename
        property is blank then it is set to aFilename without calling LoadFile.}
    procedure SaveToFile(const aFilename: string);

      { Alphabetically sorts the Entries according to their reference }
    procedure Sort;

      { Updates counts and writes summary statistics }
    procedure WriteStatistics(const filelabel: string);


    property AmbiguousCount: integer read FAmbiguousCount;

      { Number of entries }
    property Count: integer read GetCount;

      { Number of entries with a reference already in the .po file. Empty references are ignored }
    property DuplicateReferenceCount: integer read FDuplicateReferenceCount;

      { Number of entries with a msgid already in the .po file. Empty msgid are ignored }
    property DuplicateMsgidCount: integer read FDuplicateMsgidCount;

      { Number of entries with a msgstr already in the .po file. Empty msgstr are ignored }
    property DuplicateMsgstrCount: integer read FDuplicateMsgstrCount;

      { Array of all TPoEntry Found in the file }
    property Entries[index: integer]: TPoEntry read GetEntry; default;

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

    { Flag that determines if LoadFile is verbose or not. By default this
        is false, so the constructor create(aFilename) do not report parsing
        errors to the standard output }
    property ReportError: boolean read FReportError write FReportError;
  end;


implementation

{ TPoEntry }

constructor TPoEntry.create;
begin
  inherited create;
  msgid := TStringList.create;
  msgstr := TStringList.create;
  prevmsgid := TStringList.create;
end;

destructor TPoEntry.destroy;
begin
  msgid.free;
  msgstr.free;
  prevmsgid.free;
  inherited destroy;
end;

procedure TPoEntry.Assign(source: TPoEntry);
begin
  HasDuplicateReference := false;
  HasDuplicateMsgid := false;
  HasDuplicateMsgstr := false;
  IsAmbiguous := false;
  IsFuzzy := source.IsFuzzy;

  Reference := source.Reference;
  msgctxt := source.msgctxt;
  msgid.assign(source.msgid);
  msgstr.assign(source.msgstr);
  prevmsgid.assign(source.prevmsgid);
end;

function TPoEntry.Equals(Obj: TObject): boolean;
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

{ returns true if this entry and source have the
   same msgid and the same msgctxt }
function TPoEntry.Same(source: TPoEntry): boolean;
begin
  result := HasMsgid and msgid.Equals(source.msgid) and (msgctxt = source.msgctxt);
end;

{ TPoFile }

constructor TPoFile.Create;
begin
  inherited;
  FList := TList.Create;
  Clear;
end;

constructor TPoFile.Create(const aFilename: string);
begin
  create;
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
  FAmbiguousCount := -1;
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
begin
  // Possible comparisons of two entities

  //result := CompareStrt(Entries[i].Reference, Entries[j].Reference); // case sensitve, ASCII only
  //result := CompareText(Entries[i].Reference, Entries[j].Reference);  // case insensitive, ASCII only
  //result := AnsiCompareStr(Entries[i].Reference, Entries[j].Reference); // case sensitve, ignore accents
  result := AnsiCompareText(Entries[i].Reference, Entries[j].Reference); // case insensitive, ignore accents
  //result := UnicodeCpareStr(Entries[i].Reference, Entries[j].Reference); // case sensitive  needs unit cwstrings
  //result := UnicodeCompareText(Entries[i].Reference, Entries[j].Reference); // case insensitive
end;

procedure TPoFile.Delete(index: integer);
begin
  TPoEntry(FList[index]).free;
  Flist.delete(index);
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

function TPoFile.FindDuplicateEntry(index: integer): integer;
var
  i: integer;
begin
  for i := 0 to index-1 do begin
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

function TPoFile.FoundReference(const value: string; lastIndex: integer): integer;
var
  i: integer;
begin
  for i := 0 to lastIndex-1 do
    if TPoEntry(FList[i]).Reference = value then begin
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

function TPoFile.FoundSame(index: integer): boolean;
var
  i: integer;
begin
  for i := 0 to index-1 do
    if Entries[index].Same(Entries[i]) then begin
      result := true;
      exit;
    end;
  result := false;
end;


function TPoFile.GetCount: integer;
begin
  result := FList.Count;
end;

function TPoFile.GetEntry(index: integer): TPoEntry;
begin
  result := TPoEntry(FList[index]);
end;

function TPoFile.IndexOfReference(const value: string): integer;
begin
  result := FoundReference(value, Count);
end;

function TPoFile.IndexOfMsgid(const value: TStrings): integer;
begin
  result := FoundMsgId(value, count);
end;

function TPoFile.Insert(index: integer): TPoEntry;
begin
  result := TPoEntry.create;
  FList.Insert(index, result);
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
    rsIdle,     // starting
    rsReference,
    rsFuzzy,
    rsMsgctxt,
    rsMsgId,
    rsMsgstr,
    rsAltMsgid,
    rsError);

procedure TPoFile.LoadFile;
var
  src: TextFile;
  currentLineNumber: integer;
  currentLine: string;
  status: TReadStatus;

  procedure Report(const msg: string);
  begin
    if FReportError then begin
      writeln(FErrorCount:6, '  Error: ', msg, ' in line ', currentLineNumber)
    end;
  end;

  procedure CheckEntry(n: integer);
  begin
    if (n = 0) and (Entries[0].Reference = '') then with Entries[0] do begin
      if HasMsgid then begin
        inc(FErrorCount);
        Report('Entry 0 with no reference has an msgid');
      end;
      if not HasMsgstr then begin
        inc(FErrorCount);
        Report('Entry 0 with no reference does not have a msgstr');
      end;
    end
    else with Entries[n] do begin
      if not HasMsgid then begin
        inc(FErrorCount);
        Report(Format('Entry %d (%s) does not have a msgid', [n, Reference]));
      end;
      (*
      if not HasMsgstr then begin
        inc(FErrorCount);
        Report(Format('Entry %d (%s) does not have a msgstr', [n, Reference]));
      end;
      *)
    end;
  end;

  function StartsWith(const value: string): boolean;
  begin
    result := value = copy(currentLine, 1, length(value));
  end;

  procedure ReadFuzzy;
  {
  #: dmulist.sinvalidperiods
  #, fuzzy
  msgctxt "dmulist.sinvalidperiods"
  msgid "%d is an invalid number of periods"
  msgstr "Nombre de périodes, %d, incorrect"
  }
  begin
    if pos('fuzzy', currentLine) < 1 then begin
      status := rsError;
      inc(FErrorCount);
      Report('missing "fuzzy" keyword');
      exit;
    end;
    if status in [rsIdle, rsMsgId, rsMsgstr, rsError] then
      Insert(count);
    LastEntry.IsFuzzy := true;
    status := rsFuzzy;
  end;

  procedure ReadReference;
  begin
    if count > 0 then
      CheckEntry(count-1);
    system.delete(currentLine, 1, 2);
    if status in [rsIdle, rsMsgId, rsMsgstr, rsError] then
      Insert(count);
    LastEntry.Reference := trim(currentLine);
    if LastEntry.Reference = '' then begin
      inc(FErrorCount);
      Report('Reference empty');
    end;
    status := rsReference;
  end;

  {
  #: tnewprojectform.label2.caption
  #, fuzzy
  #| msgid ""
  #| "Default names\n"
  #| "(%d is number)\n"
  msgid "Default names"
  msgstr ""
  "Noms par défaut\n"
  "(%d : index)\n"
  }
  procedure ReadAltmsgid;
  var
    q, n: integer;
  begin
    if status in [rsIdle, rsMsgId, rsMsgstr, rsError] then
      Insert(count);
    q := pos('"', currentLine);
    if q < 1 then begin
      inc(FErrorCount);
      status := rsError;
      Report('Missing leading " quote in altmsgid');
      exit;
    end;
    system.delete(currentLine, 1, q);
    n := length(currentLine);
    if currentLine[n] = '"' then
      system.delete(currentLine, n, 1)
    else begin
      inc(FErrorCount);
      Report('Missing trailing " quote in altmsgid');
      // carry on as if ok
    end;
    LastEntry.prevmsgid.Add(currentLine);
    status := rsAltmsgId;
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
    else if status = rsAltmsgid then
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
      Insert(count);
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
    LastEntry.msgctxt := trim(currentLine);
    status := rsMsgctxt;
  end;

  procedure ReadMsgid;
  var
    q, n: integer;
  begin
    if status in [rsIdle, rsMsgstr, rsError] then begin
      Insert(count);   // starting new entry
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
      Insert(count);
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
        continue;
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
          ',': ReadFuzzy;
          ':': ReadReference;
          '|': ReadAltMsgId;
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
    if count > 1 then
      CheckEntry(count-1);
  finally
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
        if i > 0 then
          writeln(dst);

        if Reference <> '' then
          writeln(dst, '#: ', Reference);

        if IsFuzzy then
          writeln(dst, '#, fuzzy');

        if (prevmsgid.Count > 0) then begin
          writeln(dst, '#| msgid "', prevmsgid[0], '"');
          for j := 1 to prevmsgid.count-1 do
            writeln(dst, '#| "', prevmsgid[j], '"');
        end;

        if msgctxt <> '' then
          writeln(dst, 'msgctxt "', msgctxt, '"');

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
          writeln(dst, 'msgstr "', msgstr[0], '"');
          for j := 1 to msgstr.count-1 do
              writeln(dst, '"', msgstr[j], '"');
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

procedure TPoFile.WriteStatistics(const filelabel: string);
var
  Ecount: integer;
begin
  UpdateCounts;
  writeln(Filelabel, ': ', Filename);
  writeln('  Errors: ', ErrorCount);

  if hasHeader then begin
    Ecount := Count-1;
    writeln('  Entries: ', Ecount, ' plus a header');
  end
  else begin
    Ecount := Count;
    writeln('  Entries: ', Ecount, ' and no header');
  end;

  writeln('  Ambiguous entries: ', AmbiguousCount);

  writeln('  Missing references: ', MissingReferenceCount);
  writeln('  Duplicate references: ', DuplicateReferenceCount);

  writeln('  Empty msgids: ', EmptyMsgidCount);
  writeln('  Duplicate msgids: ', DuplicateMsgidCount);

  writeln('  Empty msgstrs: ', EmptyMsgstrCount);
  writeln('  Duplicate msgstrs: ', DuplicateMsgstrCount);

  writeln('  Fuzzys: ', FuzzyCount);

  writeln('  prevmsgids: ', PrevMsgidCount);
end;

procedure TPoFile.UpdateCounts;
var
 i, first: integer;
begin
  FAmbiguousCount := 0;
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
      Entries[i].IsAmbiguous := FoundSame(i);
    end
    else
      inc(FEmptyMsgidCount);

    if Entries[i].HasMsgstr then
      Entries[i].HasDuplicateMsgstr := FoundMsgstr(Entries[i].msgstr, i) >= 0
    else
      inc(FEmptyMsgstrCount);

    if Entries[i].IsAmbiguous then
      inc(FAmbiguousCount);
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


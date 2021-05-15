unit mpofiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TPoEntry }

  TPoEntry = class
  public
    hasDuplicateEntity: boolean;
    hasDuplicateMsgid: boolean;
    hasDuplicateMsgstr: boolean;
    isFuzzy: boolean;    // #, fuzzy
    entity: string;      // #: entity xx
    msgctxt: string;     // msgctxt "xx"
    msgid: TStrings;     // msgid "xx"
    msgstr: TStrings;    // msgstr "xx"
    altmsgid: TStrings;  // #| msgid "xx"
    procedure Assign(source: TPoEntry);
    function isEqual(source: TPoEntry): boolean;
    constructor create;
    destructor destroy; override;
    function HasMsgid: boolean;
    function HasMsgstr: boolean;
    function HasAltmsgid: boolean;
  end;

  { TPoFile }

  TPoFile = class
  private
    FList: TList;
    FFilename: string;
    FReportError: boolean;
    FErrorCount: integer;
    FFuzzyCount: integer;
    FAltMsgidCount: integer;
    FDuplicateEntityCount: integer;
    FDuplicateMsgidCount: integer;
    FDuplicateMsgstrCount: integer;
    function foundEntity(const value: string; lastIndex: integer): integer;
    function foundMsgid(const value: TStrings; lastIndex: integer): integer;
    function foundMsgstr(const value: TStrings; lastIndex: integer): integer;
    function GetCount: integer;
    function GetEntry(index: integer): TPoEntry;
    procedure LoadFile;
    function LastEntry: TPoEntry;
    function Compare(i,j: integer): integer;
    procedure QuickSort(L, R: Integer);
    procedure SetFilename(const aFilename: string);
    procedure Exchange(i, j: integer);
    procedure UpdateCounts;
  public
    constructor create;
    constructor create(const aFilename: string);
    destructor destroy; override;
    procedure Clear;
    procedure Delete(index: integer);

    function IndexOfMsgid(const value: TStrings): integer;

      { Searches all Items[i], i=0,...,index-1 and returns the
        index of item equal to Items[index] }
    function FindDuplicateEntry(index: integer): integer;

      { Searches all Items[i] from i = 0 on returning the
        first index with an entity equal to value }
    function IndexOfEntity(const value: string): integer;

      { Inserts a blank TPoEntry at the specified position}
    function Insert(index: integer): TPoEntry;

      { Save the Entries to a text file of the given name. If the trimmed
        aFilename is empty, the procedure does nothing. If the Filename
        property is blank then it is set to aFilename without calling LoadFile.}
    procedure SaveToFile(const aFilename: string);

      { Alphabetically sorts the Entries according to their entity }
    procedure Sort;

      { Updates counts and writes summary statistics }
    procedure WriteStatistics(const filelabel: string);

      { Number of Entries }
    property count: integer read GetCount;

      { Number of errors encountered in the LoadFile method }
    property ErrorCount: integer read FErrorCount;

      { Number of Entries with the fuzzy attribute }
    property FuzzyCount: integer read FFuzzyCount;

      { Number of Entries with an alternate msgid }
    property AltMsgidCount: integer read FAltMsgidCount;

      { Number of Entries that have a duplicate entity }
    property DuplicateEntityCount: integer read FDuplicateEntityCount;

      { Number of Entries that have a msgId in an other entry. Empty msgid are ignored }
    property DuplicateMsgidCount: integer read FDuplicateMsgidCount;

      { Number of Entries that have a msgstr in an other entry. Empty msgst are ignored }
    property DuplicateMsgstrCount: integer read FDuplicateMsgstrCount;

      { Flag that determines if LoadFile is verbose or not. By default this
        is false, so the constructor create(aFilename) do not report parsing
        errors to the standard output }
    property ReportError: boolean read FReportError write FReportError;

      { Array of all TPoEntry found in the file }
    property Entries[index: integer]: TPoEntry read GetEntry; default;

      { Name of file containing the source of the object. It is set explicitely
        with the assignment Filename := aFilename implicetly with the
        constructor create(aFilename). In both cases the  or explicitely}
    property Filename: string read FFilename write SetFilename;
  end;


implementation

{ TPoEntry }

constructor TPoEntry.create;
begin
  inherited create;
  msgid := TStringList.create;
  msgstr := TStringList.create;
  altmsgid := TStringList.create;
end;

destructor TPoEntry.destroy;
begin
  msgid.free;
  msgstr.free;
  altmsgid.free;
  inherited destroy;
end;

procedure TPoEntry.Assign(source: TPoEntry);
begin
  // hasDuplicateEntity not assigned
  // hasDuplicateMsgId not assigned
  hasDuplicateEntity := false;
  hasDuplicateMsgId := false;
  hasDuplicateMsgstr := false;
  isFuzzy := source.isFuzzy;
  entity := source.entity;
  msgctxt := source.msgctxt;
  msgid.assign(source.msgid);
  msgstr.assign(source.msgstr);
  altmsgid.assign(source.altmsgid);
end;

function TPoEntry.isEqual(source: TPoEntry): boolean;
begin
  result := (entity = source.entity) and msgid.Equals(source.msgid)
    and msgstr.Equals(source.msgstr)
end;

function TPoEntry.HasAltmsgid: boolean;
begin
  result := altmsgid.Count > 0;
end;

function TPoEntry.HasMsgid: boolean;
begin
  result := ((msgid.Count = 1) and ( msgid[0] <> '')) or (msgid.Count > 1);
end;

function TPoEntry.HasMsgstr: boolean;
begin
  result := ((msgstr.Count = 1) and ( msgstr[0] <> '')) or (msgstr.Count > 1);
end;


{ TPoFile }

constructor TPoFile.create;
begin
  inherited;
  FList := TList.Create;
  Clear;
end;

constructor TPoFile.create(const aFilename: string);
begin
  create;
  FFilename := trim(aFilename);
  if FFilename <> '' then
    LoadFile;
end;

destructor TPoFile.destroy;
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
  FErrorCount := -1;
  FFuzzyCount := -1;
  FAltMsgidCount := -1;
  FDuplicateEntityCount := -1;
  FDuplicateMsgidCount := -1;
  FDuplicateMsgstrCount := -1;
end;

function TPoFile.Compare(i,j: integer): integer;
begin
  // Possible comparisons of two entities

  //result := CompareStrt(Entries[i].entity, Entries[j].entity); // case sensitve, ASCII only
  //result := CompareText(Entries[i].entity, Entries[j].entity);  // case insensitive, ASCII only
  //result := AnsiCompareStr(Entries[i].entity, Entries[j].entity); // case sensitve, ignore accents
  result := AnsiCompareText(Entries[i].entity, Entries[j].entity); // case insensitive, ignore accents
  //result := UnicodeCpareStr(Entries[i].entity, Entries[j].entity); // case sensitive  needs unit cwstrings
  //result := UnicodeCompareText(Entries[i].entity, Entries[j].entity); // case insensitive
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

function TPoFile.FindDuplicateEntry(Index: integer): integer;
var
  i: integer;
begin
  for i := 0 to index-1 do begin
    if Entries[i].isEqual(Entries[Index]) then begin
      result := i;
      exit;
    end;
  end;
  result := -1;
end;

function TPoFile.FoundEntity(const value: string; lastIndex: integer): integer;
var
  i: integer;
begin
  for i := 0 to lastIndex-1 do
    if TPoEntry(FList[i]).entity = value then begin
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

function TPoFile.GetEntry(index: integer): TPoEntry;
begin
  result := TPoEntry(FList[index]);
end;

function TPoFile.IndexOfEntity(const value: string): integer;
begin
  result := FoundEntity(value, Count);
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
    rsEntity,
    rsFuzzy,
    rsMsgctxt,
    rsMsgId,
    rsMsgstr,
    rsAltMsgid,
    rsError);
  {
  isFuzzy: boolean;    // #, fuzzy
  entity: string;      // #: entity xx
  msgctxt: string;     // msgctxt "xx"
  msgid: TStrings;       // msgid "xx"
  msgstr: TStrings;      // msgstr "xx"
  altmsgid: string;    // #| msgid "xx"
  }
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
    if (n = 0) and (Entries[0].Entity = '') then with Entries[0] do begin
      if HasMsgid then begin
        inc(FErrorCount);
        Report('Entry 0 with no entity has an msgid');
      end;
      if not HasMsgstr then begin
        inc(FErrorCount);
        Report('Entry 0 with no entity does not have a msgstr');
      end;
    end
    else with Entries[n] do begin
      if not HasMsgid then begin
        inc(FErrorCount);
        Report(Format('Entry %d (%s) does not have a msgid', [n, Entity]));
      end;
      (*
      if not HasMsgstr then begin
        inc(FErrorCount);
        Report(Format('Entry %d (%s) does not have a msgstr', [n, Entity]));
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
    LastEntry.isFuzzy := true;
    status := rsFuzzy;
  end;

  procedure ReadEntity;
  begin
    if count > 0 then
      CheckEntry(count-1);
    system.delete(currentLine, 1, 2);
    if status in [rsIdle, rsMsgId, rsMsgstr, rsError] then
      Insert(count);
    LastEntry.Entity := trim(currentLine);
    if LastEntry.Entity = '' then begin
      inc(FErrorCount);
      Report('Entity empty');
    end;
    status := rsEntity;
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
    LastEntry.AltmsgId.Add(currentLine);
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
      LastEntry.altmsgid.Add(currentLine)
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
          ':': ReadEntity;
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

        if entity <> '' then
          writeln(dst, '#: ', entity);

        if isFuzzy then
          writeln(dst, '#, fuzzy');

        if msgctxt <> '' then
          writeln(dst, 'msgctxt "', msgctxt, '"');

        if (altmsgid.Count > 0) then begin
          writeln(dst, '#| msgid "', altmsgid[0], '"');
          for j := 1 to altmsgid.count-1 do
            writeln(dst, '"', altmsgid[j], '"');
        end;

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
  if (Entries[0].entity = '') then
    first := 1
  else
    first := 0;
  QuickSort(first, FList.Count-1);
end;

procedure TPoFile.WriteStatistics(const FileLabel: string);
begin
  UpdateCounts;
  writeln(Filelabel, ': ', Filename);
  writeln('  Entries: ', Count);
  writeln('  Errors: ', ErrorCount);
  writeln('  Fuzzys: ', FuzzyCount);
  writeln('  Alternate msgids: ', AltMsgidCount);
  writeln('  Duplicate entities: ', DuplicateEntityCount);
  writeln('  Duplicate msgids: ', DuplicateMsgidCount);
  writeln('  Duplicate msgstrs: ', DuplicateMsgstrCount);
end;

procedure TPoFile.UpdateCounts;
var
  first, i: integer;

begin
  FFuzzyCount := 0;
  FDuplicateEntityCount := 0;
  FDuplicateMsgidCount := 0;
  FDuplicateMsgstrCount := 0;
  FAltMsgidCount := 0;
  if Count > 0 then begin
    if Entries[0].entity = '' then
      first := 1
    else
      first := 0;
  end;
  for i := first to Count-1 do begin
    Entries[i].hasDuplicateEntity := foundEntity(Entries[i].entity, i) >= 0;
    Entries[i].hasDuplicateMsgId := Entries[i].HasMsgid and (foundMsgId(Entries[i].msgid, i) >= 0);
    Entries[i].hasDuplicateMsgstr := Entries[i].HasMsgstr and (foundMsgstr(Entries[i].msgstr, i) >= 0);
    if Entries[i].isFuzzy then
      inc(FFuzzyCount);
    if Entries[i].hasAltMsgid then
      inc(FAltMsgidCount);
    if Entries[i].HasDuplicateEntity then
      inc(FDuplicateEntityCount);
    if Entries[i].hasDuplicateMsgId then
      inc(FDuplicateMsgIdCount);
    if Entries[i].hasDuplicateMsgstr then
      inc(FDuplicateMsgstrCount)
  end;
end;

end.


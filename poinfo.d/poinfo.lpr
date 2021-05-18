program poinfo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, mpofiles, futils;

var
  PoFile: TPoFile;
  i: integer;

begin
  if (paramcount <> 1)  then begin
    writeln('Verifies po file syntax and reports some statistics');
    writeln;
    writeln('usage:');
    writeln(Format('  %s source[.po]', [appName]));
  end
  else begin
    PoFile := TPoFile.Create;
    PoFile.ReportError := true;
    PoFile.Filename := ParamFilename(1);
    try
      if PoFile.ErrorCount > 0 then
        writeln;
      PoFile.WriteStatistics('Source');
      with PoFile do begin
        for i := 0 to Count-1 do with Entries[i] do begin
          if IsFuzzy or HasPrevmsgid or HasDuplicateReference
          or HasDuplicateMsgid or HasDuplicateMsgstr then
            writeln;
          if IsAmbiguous then
            writeln(Format('Entry %d (%s) is ambiguous', [i, Reference]));
          if HasDuplicateMsgid then
            writeln(Format('Entry %d (%s) has a duplicate msgid', [i, Reference]));
          if HasDuplicateMsgstr then
            writeln(Format('Entry %d (%s) has a duplicate msgstr', [i, Reference]));
          if IsFuzzy then
            writeln(Format('Entry %d (%s) is fuzzy', [i, Reference]));
          if HasPrevmsgid then
            writeln(Format('Entry %d (%s) has a previous msgid', [i, Reference]));
          if HasDuplicateReference then
            writeln(Format('Entry %d (%s) has a duplicate reference', [i, Reference]));
        end;
      end;

    finally
      PoFile.free;
    end;
  end;
end.


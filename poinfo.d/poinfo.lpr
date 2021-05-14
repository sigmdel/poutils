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
          if isFuzzy or hasAltmsgid or hasDuplicateEntity
          or hasDuplicateMsgId or hasDuplicateMsgstr then
            writeln;
          if isFuzzy then
            writeln(Format('Entry %d (%s) is fuzzy', [i, entity]));
          if hasAltmsgid then
            writeln(Format('Entry %d (%s) has an alternate msgid', [i, entity]));
          if hasDuplicateEntity then
            writeln(Format('Entry %d (%s) has a duplicate entity', [i, entity]));
          if hasDuplicateMsgid then
            writeln(Format('Entry %d (%s) has a duplicate msgid', [i, entity]));
          if hasDuplicateMsgstr then
            writeln(Format('Entry %d (%s) has a duplicate msgstr', [i, entity]));
        end;
      end;

    finally
      PoFile.free;
    end;
  end;
end.


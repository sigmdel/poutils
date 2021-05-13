program poinfo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, mpofiles, futils;

var
  PoFile: TPoFile;
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
      writeln('Source: ', PoFile.Filename);
      writeln('  Entries: ', PoFile.count);
      writeln('  Errors: ', PoFile.ErrorCount);
      writeln('  Fuzzys: ', PoFile.FuzzyCount);
      writeln('  Duplicate entities: ', PoFile.DuplicateEntityCount);
      writeln('  Duplicate msgid: ', PoFile.DuplicateMsgidCount);
      writeln('  Duplicate msgstr: ', PoFile.DuplicateMsgstrCount);
      if (PoFile.DuplicateEntityCount > 0)
      or (PoFile.DuplicateMsgidCount > 0)
      or (PoFile.DuplicateMsgstrCount > 0)then begin
        writeln;
        PoFile.ReportDuplicates;
      end;
    finally
      PoFile.free;
    end;
  end;
end.


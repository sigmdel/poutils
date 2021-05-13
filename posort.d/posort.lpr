program posort;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, mpofiles, futils;

var
  PoFile: TPoFile;
  fname: string;

begin
  if (paramcount < 1) or (paramcount > 2) then begin
    writeln('Sorts a po file by entity');
    writeln;
    writeln('usage:');
    writeln(Format('  %s source[.po] [output[.po]]', [appName]));
  end
  else begin
    PoFile := TPoFile.Create(ParamFilename(1));
    try
      writeln('Source: ', PoFile.Filename);
      writeln('  Entries: ', PoFile.count);
      writeln('  Errors: ', PoFile.ErrorCount);
      writeln('  Fuzzys: ', PoFile.FuzzyCount);
      writeln('  Duplicate entities: ', PoFile.DuplicateEntityCount);
      writeln('  Duplicate msgid: ', PoFile.DuplicateMsgidCount);
      writeln('  Duplicate msgstr: ', PoFile.DuplicateMsgstrCount);
      PoFile.Sort;
      if paramcount > 1 then
        fname := ParamFilename(2)
      else
        fname := PoFile.Filename;
      SaveToBackup(fname);
      PoFile.SaveToFile(fname);
      writeln('Output: ', fname);
    finally
      PoFile.free;
    end;
  end;
end.


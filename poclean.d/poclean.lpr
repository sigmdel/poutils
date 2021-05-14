program poclean;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, mpofiles, futils;

var
  PoFile: TPoFile;
  fname: string;
  i, count: integer;

begin
  if (paramcount < 1) or (paramcount > 2) then begin
    writeln('Removes duplicate entries in a po file');
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

      count := 0;
      if (poFile.DuplicateEntityCount > 0) then begin
        for i := PoFile.Count-1 downto 0 do begin
          if PoFile[i].hasDuplicateEntity and (PoFile.FindDuplicateEntry(i) >= 0) then begin
            PoFile.delete(i);
            inc(count);
          end;
        end;
      end;

      if count < 1 then
        writeln('No duplicates')
      else begin
        PoFile.Sort;
        if paramcount > 1 then
          fname := ParamFilename(2)
        else
          fname := PoFile.Filename;
        if not SaveToBackup(fname) then
          fname := RandomFilename(fname);
        PoFile.SaveToFile(fname);
        PoFile.UpdateCounts;
        writeln('Output: ', fname);
        writeln('  Entries: ', PoFile.count);
        writeln('  Errors: ', PoFile.ErrorCount);
        writeln('  Fuzzys: ', PoFile.FuzzyCount);
        writeln('  Duplicate entities: ', PoFile.DuplicateEntityCount);
        writeln('  Duplicate msgid: ', PoFile.DuplicateMsgidCount);
        writeln('  Duplicate msgstr: ', PoFile.DuplicateMsgstrCount);
      end;
    finally
      PoFile.free;
    end;
  end;
end.


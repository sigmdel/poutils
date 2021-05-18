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
    writeln('Removes entries with duplicate references, msgid and msgstr in a po file');
    writeln;
    writeln('usage:');
    writeln(Format('  %s source[.po] [output[.po]]', [appName]));
  end
  else begin
    PoFile := TPoFile.Create(ParamFilename(1));
    try
      PoFile.WriteStatistics('Source');
      count := 0;
      if (poFile.DuplicateReferenceCount > 0) then begin
        for i := PoFile.Count-1 downto 0 do begin
          if PoFile[i].HasDuplicateReference and (PoFile.FindDuplicateEntry(i) >= 0) then begin
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
          fname := UniqueFilename(fname);
        PoFile.SaveToFile(fname);
        PoFile.WriteStatistics('Output');
      end;
    finally
      PoFile.free;
    end;
  end;
end.


program poscrub;

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
    writeln('Removes duplicate entries, fuzzy flag and altmsgid in a po file');
    writeln;
    writeln('usage:');
    writeln(Format('  %s source[.po] [output[.po]]', [appName]));
  end
  else begin
    PoFile := TPoFile.Create(ParamFilename(1));
    try
      PoFile.WriteStatistics('Source');
      count := 0;
      if (poFile.DuplicateEntityCount > 0) then begin
        for i := PoFile.Count-1 downto 0 do begin
          if PoFile[i].hasDuplicateEntity and (PoFile.FindDuplicateEntry(i) >= 0) then begin
            PoFile.delete(i);
            inc(count);
          end;
        end;
      end;

      for i := 0 to PoFile.Count-1 do begin
        if PoFile[i].isFuzzy then begin
          PoFile[i].isFuzzy := false;
          inc(count);
        end;
        if (PoFile[i].altmsgid.count > 0) then begin
          PoFile[i].altmsgid.clear;
          inc(count);
        end;
      end;

      if count < 1 then
        writeln('No changes')
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

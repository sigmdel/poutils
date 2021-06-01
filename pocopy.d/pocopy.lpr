program pocopy;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, mpofiles, futils;

var
  PoFile: TPoFile;
  fname: string;
  i, first: integer;

begin
  if (paramcount < 1) or (paramcount > 2) then begin
    writeln('Copies all msgid to msgstr in .po file');
    writeln;
    writeln('usage:');
    writeln(Format('  %s source[.po] [output[.po]]', [appName]));
  end
  else begin
    PoFile := TPoFile.Create(ParamFilename(1));
    try
      PoFile.WriteStatistics('Source');
      if PoFile.HasHeader then
        first := 1
      else
        first := 0;
      for i := first to PoFile.count-1 do begin
        PoFile[i].msgstr.Assign(PoFile[i].msgid);
      end;
      if paramcount > 1 then
        fname := ParamFilename(2)
      else
        fname := PoFile.Filename;
      if not SaveToBackup(fname) then
        fname := UniqueFilename(fname);
      PoFile.SaveToFile(fname);
      PoFile.WriteStatistics('Output', fname);
    finally
      PoFile.free;
    end;
  end;
end.


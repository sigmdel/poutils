program pofill;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, mpofiles, futils;

var
  PoFile: TPoFile;
  fname: string;
  i: integer;

begin
  if (paramcount < 1) or (paramcount > 2) then begin
    writeln('Copies msgid to any empty msgstr in .po file');
    writeln;
    writeln('usage:');
    writeln(Format('  %s source[.po] [output[.po]]', [appName]));
  end
  else begin
    PoFile := TPoFile.Create(ParamFilename(1));
    try
      PoFile.WriteStatistics('Source');
      for i := 0 to PoFile.count-1 do begin
        if (i = 0) and (PoFile[i].entity = '') or PoFile[i].hasMsgstr then
          continue;
        PoFile[i].msgstr.assign(PoFile[i].msgid);
      end;
      if paramcount > 1 then
        fname := ParamFilename(2)
      else
        fname := PoFile.Filename;
      if not SaveToBackup(fname) then
        fname := UniqueFilename(fname);
      PoFile.SaveToFile(fname);
      PoFile.WriteStatistics('Output');
    finally
      PoFile.free;
    end;
  end;
end.


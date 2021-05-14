program poswap;

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
  swap: string;

begin
  if (paramcount < 1) or (paramcount > 2) then begin
    writeln('Exchanges all msgid and msgstr fields in .po file');
    writeln;
    writeln('usage:');
    writeln(Format('  %s source[.po] [output[.po]]', [appName]));
  end
  else begin
    PoFile := TPoFile.Create(ParamFilename(1));
    try
      PoFile.WriteStatistics('Source');
      for i := 0 to PoFile.count-1 do begin
        if (i = 0) and (PoFile[i].entity = '') then
          continue;
        if (Pofile[i].msgstr.count = 0) or (Pofile[i].msgstr.text = #$0A) then
          continue;
        swap := PoFile[i].msgstr.Text;
        PoFile[i].msgstr.Text := PoFile[i].msgid.Text;
        PoFile[i].msgid.Text := swap;
      end;
      if paramcount > 1 then
        fname := ParamFilename(2)
      else
        fname := PoFile.Filename;
      if not SaveToBackup(fname) then
        fname := (fname);
      PoFile.SaveToFile(fname);
      PoFile.WriteStatistics('Output');
    finally
      PoFile.free;
    end;
  end;
end.


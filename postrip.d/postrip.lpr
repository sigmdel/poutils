program postrip;

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
    writeln('Removes all translated strings in .po file');
    writeln;
    writeln('usage:');
    writeln(Format('  %s source[.po] [output[.po]]', [appName]));
  end
  else begin
    PoFile := TPoFile.Create(ParamFilename(1));
    try
      PoFile.WriteStatistics('Source');
      if PoFile.count < 1 then
        exit;
      for i := 0 to PoFile.count-1 do begin
        if (i = 0) and (PoFile[i].entity = '') then
          continue;
        PoFile[i].msgstr.Clear;
        PoFile[i].msgstr.add('');
      end;
      // add initial Content-Type translation if needed
      if PoFile[0].entity <> '' then
        PoFile.Insert(0);
      with PoFile[0] do begin
        msgid.clear;
        msgstr.clear;
        msgid.add('');
        msgstr.add('Content-Type: text/plain; charset=UTF-8');
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


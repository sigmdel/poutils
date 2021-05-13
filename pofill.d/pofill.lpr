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
      writeln('Source: ', PoFile.Filename);
      writeln('  Entries: ', PoFile.count);
      writeln('  Errors: ', PoFile.ErrorCount);
      writeln('  Fuzzys: ', PoFile.FuzzyCount);
      writeln('  Duplicate entities: ', PoFile.DuplicateEntityCount);
      writeln('  Duplicate msgid: ', PoFile.DuplicateMsgidCount);
      writeln('  Duplicate msgstr: ', PoFile.DuplicateMsgstrCount);
      for i := 0 to PoFile.count-1 do begin
        if (i = 0) and (PoFile[i].entity = '') then
          continue;
        //writeln('text <', PoFile[i].msgstr.Text, '>');
        if (Pofile[i].msgstr.Count = 0) or (PoFile[i].msgstr.Text = #$0A) then
          PoFile[i].msgstr.Text := PoFile[i].msgid.Text;
      end;
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


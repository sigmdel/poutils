program poupdate;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, mpofiles, futils;

var
  SourcePo, TransPo: TPoFile;
  fname: string;
  i, n, count: integer;

begin
  if (paramcount < 2) or (paramcount > 3) then begin
    writeln('Updates empty msgstr from translations found in trans');
    writeln;
    writeln('usage:');
    writeln(Format('  %s source[.po] trans[.po] [output[.po]]', [appName]));
  end
  else begin
    SourcePo := TPoFile.Create(ParamFilename(1));
    try
      writeln('Source 1: ', SourcePo.filename);
      writeln('  Entries: ', SourcePo.count);
      writeln('  Errors: ', SourcePo.ErrorCount);
      writeln('  Fuzzys: ', SourcePo.FuzzyCount);
      writeln('  Duplicate entities: ', SourcePo.DuplicateEntityCount);
      writeln('  Duplicate msgid: ', SourcePo.DuplicateMsgidCount);

      TransPo := TPoFile.Create(ParamFilename(2));
      try
        writeln('Source 2: ', TransPo.filename);
        writeln('  Entries: ', TransPo.count);
        writeln('  Errors: ', TransPo.ErrorCount);
        writeln('  Fuzzys: ', TransPo.FuzzyCount);
        writeln('  Duplicate entities: ', TransPo.DuplicateEntityCount);
        writeln('  Duplicate msgid: ', TransPo.DuplicateMsgidCount);
        count := 0;
        for i := 0 to SourcePo.count-1 do begin
          //writeln(Format('dbg: MorePo[%d].entity="%s"', [i, TransPo[i].entity]));
          if (i = 0) and (SourcePo[i].Entity = '') then
            continue;
          if (SourcePo[i].msgstr.text = #$0A) or (SourcePo[i].msgstr.text = '') or (SourcePo[i].msgstr.count = 0) then begin
             n := TransPo.IndexOfMsgid(SourcePo[i].msgId.Text);
             if n >= 0 then begin
               SourcePo[i].msgstr.text := TransPo[n].msgstr.text;
               inc(count);
             end;
          end;
        end;

        if count < 1 then
          writeln('No translations found')
        else begin
          if paramcount > 2 then
            fname := ParamFilename(3)
          else
            fname := SourcePo.Filename;
          if not SaveToBackup(fname) then
            fname := RandomFilename(fname);
          SaveToBackup(fname);
          SourcePo.SaveToFile(fname);
          if count = 1 then
            writeln('One translation found')
          else
            writeln(Format('%d translations found', [count]));
          SourcePo.UpdateCounts;
          writeln('Output: ', fname);
          writeln('  Entries: ', SourcePo.count);
          writeln('  Errors: ', SourcePo.ErrorCount);
          writeln('  Fuzzys: ', SourcePo.FuzzyCount);
          writeln('  Duplicate entities: ', SourcePo.DuplicateEntityCount);
          writeln('  Duplicate msgid: ', SourcePo.DuplicateMsgidCount);
        end;
      finally
        TransPo.free;
      end;
    finally
      SourcePo.free;
    end;
  end;
end.


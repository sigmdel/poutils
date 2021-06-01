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
  i, first, n, count: integer;

begin
  if (paramcount < 2) or (paramcount > 3) then begin
    writeln('Updates empty msgstr from translations found in trans');
    writeln;
    writeln('usage:');
    writeln(Format('  %s source[.po] translations[.po] [output[.po]]', [appName]));
  end
  else begin
    SourcePo := TPoFile.Create(ParamFilename(1));
    try
      SourcePo.WriteStatistics('Source');
      TransPo := TPoFile.Create(ParamFilename(2));
      try
        TransPo.WriteStatistics('Translations');
        count := 0;
        if SourcePo.HasHeader then
          first := 1
        else
          first := 0;
        for i := first to SourcePo.count-1 do begin
          if SourcePo[i].hasMsgstr then
            continue;
          if not SourcePo[i].hasMsgid then
            continue;
          n := TransPo.IndexOfMsgid(SourcePo[i].msgid);
          if n >= 0 then begin
            SourcePo[i].msgstr.assign(TransPo[n].msgstr);
            inc(count);
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
            fname := UniqueFilename(fname);
          SourcePo.SaveToFile(fname);
          if count = 1 then
            writeln('One translation found')
          else
            writeln(Format('%d translations found', [count]));
          SourcePo.WriteStatistics('Output', fname);
        end;
      finally
        TransPo.free;
      end;
    finally
      SourcePo.free;
    end;
  end;
end.


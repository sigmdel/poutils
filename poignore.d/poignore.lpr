program poignore;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, mpofiles, futils;

var
  SourcePo, RemovePo, OutPo: TPoFile;
  fname: string;
  i: integer;

begin
  writeln;
  if (paramcount < 2) or (paramcount > 3) then begin
    writeln('Removes all entries found in a remove .po file from a source .po file');
    writeln;
    writeln('usage:');
    writeln(Format('  %s source[.po] remove[.po] [output[.po]]', [appName]));
  end
  else begin
    OutPo := nil;
    RemovePo := nil;
    SourcePo := TPoFile.Create(ParamFilename(1));
    RemovePo := TPoFile.Create(ParamFilename(2));
    OutPo := TPoFile.Create('');
    try
      writeln('Source: ', SourcePo.filename);
      writeln('  Entries: ', SourcePo.count);
      writeln('  Errors: ', SourcePo.ErrorCount);
      writeln('  Fuzzys: ', SourcePo.FuzzyCount);
      writeln('  Duplicate entities: ', SourcePo.DuplicateEntityCount);
      writeln('  Duplicate msgid: ', SourcePo.DuplicateMsgidCount);
      writeln('  Duplicate msgstr: ', SourcePo.DuplicateMsgstrCount);

      writeln('Remove: ', RemovePo.filename);
      writeln('  Entries: ', RemovePo.count);
      writeln('  Errors: ', RemovePo.ErrorCount);
      writeln('  Fuzzys: ', RemovePo.FuzzyCount);
      writeln('  Duplicate entities: ', RemovePo.DuplicateEntityCount);
      writeln('  Duplicate msgid: ', RemovePo.DuplicateMsgidCount);
      writeln('  Duplicate msgstr: ', RemovePo.DuplicateMsgstrCount);

      for i := 0 to SourcePo.count-1 do begin
        if ((i = 0) and (SourcePo[i].entity = '')) or (RemovePo.IndexOfEntity(SourcePo[i].Entity) < 0) then
          OutPo.Insert(OutPo.count).Assign(SourcePo[i]);
      end;

      if paramcount > 2 then
        fname := ParamFilename(3)
      else
        fname := SourcePo.filename;

      SaveToBackup(fname);
      OutPo.SaveToFile(fname);
      OutPo.UpdateCounts;
      writeln('Output: ', fname);
      writeln('  Entries: ', OutPo.count);
      writeln('  Errors: ', OutPo.ErrorCount);
      writeln('  Fuzzys: ', OutPo.FuzzyCount);
      writeln('  Duplicate entities: ', OutPo.DuplicateEntityCount);
      writeln('  Duplicate msgid: ', OutPo.DuplicateMsgidCount);
      writeln('  Duplicate msgstr: ', OutPo.DuplicateMsgstrCount);
    finally
      SourcePo.free;
      RemovePo.free;
      OutPo.free;
    end;
  end;
end.


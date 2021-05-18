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
  i, first: integer;

begin
  writeln;
  if (paramcount < 2) or (paramcount > 3) then begin
    writeln('Removes from a source .po file all entries with a reference found in a remove .po file');
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
      SourcePo.WriteStatistics('Source');
      RemovePo.WriteStatistics('Remove');
      if SourcePo.HasHeader then begin
        OutPo.Insert(OutPo.count).Assign(SourcePo[0]);
        first := 1
      end
      else
        first := 0;
      for i := first to SourcePo.count-1 do begin
        if (RemovePo.IndexOfReference(SourcePo[i].Reference) < 0) then
          OutPo.Insert(OutPo.count).Assign(SourcePo[i]);
      end;

      if paramcount > 2 then
        fname := ParamFilename(3)
      else
        fname := SourcePo.filename;

      if not SaveToBackup(fname) then
        fname := UniqueFilename(fname);
      OutPo.SaveToFile(fname);
      OutPo.WriteStatistics('Output');
    finally
      SourcePo.free;
      RemovePo.free;
      OutPo.free;
    end;
  end;
end.


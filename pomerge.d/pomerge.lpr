program pomerge;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, mpofiles, futils;

var
  SourcePo, MorePo: TPoFile;
  fname: string;
  i, n: integer;

begin
  if (paramcount < 2) or (paramcount > 3) then begin
    writeln('Merges two .po files');
    writeln;
    writeln('usage:');
    writeln(Format('  %s source[.po] more[.po] [output[.po]]', [appName]));
  end
  else begin
    SourcePo := TPoFile.Create(ParamFilename(1));
    try
      SourcePo.WriteStatistics('Source');
      MorePo := TPoFile.Create(ParamFilename(2));
      try
        MorePo.WriteStatistics('More');

        for i := MorePo.count-1 downto 0 do begin
          //writeln(Format('dbg: MorePo[%d].entity="%s"', [i, MorePo[i].entity]));
          if (i = 0) and (MorePo[i].Entity = '') then
            continue;
          n := SourcePo.indexofEntity(MorePo[i].Entity);
          if n >= 0 then begin
            //writeln(Format('dbg: found SourcePo[%d].entity="%s"', [n, SourcePo[n].entity]));
            if SourcePo[n].Equals(MorePo[i]) then begin
               MorePo.delete(i);
               //writeln(Format('dbg: entities equal, MorePo[%d] deleted', [i]));
            end
            else begin
              //MorePo[i] is different remains in MorePo
              //writeln(Format('dbg MorePo[%d] conflicts with SourcePo[%d] so it remains in MorePo', [i, n]));
            end;
          end
          else begin
            SourcePo.Insert(SourcePo.Count).Assign(MorePo[i]);
            MorePo.delete(i);
            //writeln(Format('dbg: MorePo[%d] copied to SourcePo[%d] and deleted from MorePo', [i, SourcePo.Count-1]));
          end;
        end;

        // Now SourcePo has all its original entries plus all different MorePo
        // entries, while MorePo contains all of its original entries less
        // all those copied into SourcePo. In other words, MorePo contains
        // the duplicate entities that have either a different msgid or
        // different msgstr

        if paramcount > 2 then
          fname := ParamFilename(3)
        else
          fname := SourcePo.Filename;
        SaveToBackup(fname);
        SourcePo.SaveToFile(fname);
        SourcePo.WriteStatistics('Output');

        If (MorePo.Count = 0) or ((MorePo.Count = 1) and (MorePo[0].entity = '') ) then
          writeln('No conflicting entries')
        else begin
          fname := MorePo.Filename + '.conflicts';
          if not SaveToBackup(fname) then
            fname := UniqueFilename(fname);
          MorePo.SaveToFile(fname);
          MorePo.WriteStatistics('Conflicts');
        end;
      finally
        MorePo.free;
      end;
    finally
      SourcePo.free;
    end;
  end;
end.


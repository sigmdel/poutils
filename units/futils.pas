unit futils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


  { Returns application name }
function AppName: string;

  { Returns Paramstr(index) as a input po filename. If
    a file with a name equal to paramstr(index) does not exist
    the a .po extension is added and that is returned}
function ParamFilename(index: integer): string;

  { If adds .bak, .bak1, .bak2 etc extention to filename
    until a non existing file is found. Then filename is
    renamed to the given name.}
function SaveToBackup(const filename: string): string;

implementation


function AppName: string;
begin
  result := changefileext(extractfilename(paramstr(0)), '');
end;

function ParamFilename(index: integer): string;
begin
  result := paramstr(index);
  if (result <> '') and not fileexists(result) then
    result := result + '.po';
end;

function SaveToBackup(const filename: string): string;
var
  i: integer;
  fname: string;
begin
  result := filename + '.bak';
  if fileexists(result) then begin
    fname := result;
    i := 0;
    repeat
      inc(i);
      result := fname + inttostr(i);
    until not fileexists(result);
  end;
  renamefile(filename, result);
end;

end.


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
    renamed to that new name. Returns false if the file
    could not be renamed, true otherwise}
function SaveToBackup(const filename: string): boolean;

function RandomFilename(const filename: string): string;

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

function SaveToBackup(const filename: string): boolean;
var
  i: integer;
  fname: string;
  stub: string;
begin
  fname := filename + '.bak';
  if fileexists(fname) then begin
    stub := fname;
    i := 0;
    repeat
      inc(i);
      fname := stub + inttostr(i);
    until not fileexists(fname);
  end;
  result := renamefile(filename, fname);
end;

function RandomFilename(const filename: string): string;
var
  ext, stub: string;
begin
  ext := extractfileext(filename);
  stub := changefileext(filename, '');
  repeat
    result := Format('%s-%d%s', [stub, random(maxint), ext]);
  until not fileexists(result);
end;


initialization
  randomize;
end.


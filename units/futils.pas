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

  { If filename exists, then adds .bak, .bak1, .bak2, etc extention
    to the filename until a non existing file is found. Then filename is
    renamed to that new name. Returns false should renaming the file not
    work or true otherwise (including when filename does not exist)}
function SaveToBackup(const filename: string): boolean;

  { Returns filename if it does not exist, otherwise returns filenameXXXX
    where XXXX are random digits chosen so that this new name does not
    exits.}
function UniqueFilename(const filename: string): string;

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
  result := true;
  if fileexists(filename) then begin
    fname := filename + '.bak';
    stub := fname;
    i := 0;
    while fileexists(fname) do begin
      inc(i);
      fname := stub + inttostr(i);
    end;
    result := renamefile(filename, fname);
  end;
end;

function UniqueFilename(const filename: string): string;
var
  ext, stub: string;
  i: integer;
begin
  ext := extractfileext(filename);
  stub := changefileext(filename, '');
  i := 0;
  result := filename;
  while fileexists(result) do begin
    inc(i);
    result := Format('%s-%d%s', [stub, i, ext]);
  end;
end;


initialization
  randomize;
end.


unit pofilestest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,  mpofiles;

type
  TPoFileProt = class(TPoFile);  // get access to protected methods

  { TTestPofiles }

  TTestPofiles= class(TTestCase)
  private
    PoFile: TPoFileProt;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConstructor;
    procedure TestUpdateCountOnEmptyFile;
    procedure TestLoadOkSimpleFile;
    procedure TestLoadOkComplexFile;
    procedure TestSaveOkSimpleFile;
    procedure TestSaveOkComplexFile;
  end;

implementation


procedure TTestPofiles.SetUp;
begin
  PoFile := TPoFileProt.Create;
end;

procedure TTestPofiles.TearDown;
begin
  freeandnil(PoFile);
end;

procedure TTestPofiles.TestConstructor;
begin
  AssertEquals('Count:', 0, PoFile.Count);
  AssertEquals('ErrorCount', -1, PoFile.ErrorCount);
  AssertEquals('FuzzyCount', -1, PoFile.FuzzyCount);
  AssertEquals('AltMsgidCount', -1, PoFile.AltMsgidCount);
  AssertEquals('DuplicateEntityCount', -1, PoFile.DuplicateEntityCount);
  AssertEquals('DuplicateMsgidCount', -1, PoFile.DuplicateMsgidCount);
  AssertEquals('DuplicateMsgstrCount', -1, PoFile.DuplicateMsgstrCount);
end;

procedure TTestPofiles.TestUpdateCountOnEmptyFile;
begin
  PoFile.UpdateCounts;
  AssertEquals('Count:', 0, PoFile.Count);
  AssertEquals('ErrorCount', -1, PoFile.ErrorCount);
  AssertEquals('FuzzyCount', 0, PoFile.FuzzyCount);
  AssertEquals('AltMsgidCount', 0, PoFile.AltMsgidCount);
  AssertEquals('DuplicateEntityCount', 0, PoFile.DuplicateEntityCount);
  AssertEquals('DuplicateMsgidCount', 0, PoFile.DuplicateMsgidCount);
  AssertEquals('DuplicateMsgstrCount', 0, PoFile.DuplicateMsgstrCount);
end;

procedure TTestPofiles.TestLoadOkSimpleFile;
begin
  AssertEquals('Count:', 0, PoFile.Count);
  AssertEquals('ErrorCount', -1, PoFile.ErrorCount);
  PoFile.Filename := 'ok_simple.po.test';
  AssertEquals('Count:', 7, PoFile.Count);
  AssertEquals('ErrorCount', 0, PoFile.ErrorCount);
  PoFile.UpdateCounts;
  AssertEquals('FuzzyCount', 0, PoFile.FuzzyCount);
  AssertEquals('AltMsgidCount', 0, PoFile.AltMsgidCount);
  AssertEquals('DuplicateEntityCount', 0, PoFile.DuplicateEntityCount);
  AssertEquals('DuplicateMsgidCount', 0, PoFile.DuplicateMsgidCount);
  AssertEquals('DuplicateMsgstrCount', 0, PoFile.DuplicateMsgstrCount);
end;

procedure TTestPofiles.TestLoadOkComplexFile;
var
  sl: TStrings;
begin
  AssertEquals('Count:', 0, PoFile.Count);
  AssertEquals('ErrorCount', -1, PoFile.ErrorCount);
  PoFile.Filename := 'ok_complex.po.test';
  AssertEquals('Count:', 6, PoFile.Count);
  AssertEquals('ErrorCount', 0, PoFile.ErrorCount);
  PoFile.UpdateCounts;
  AssertEquals('FuzzyCount', 3, PoFile.FuzzyCount);
  AssertEquals('AltMsgidCount', 2, PoFile.AltMsgidCount);
  AssertEquals('DuplicateEntityCount', 0, PoFile.DuplicateEntityCount);
  AssertEquals('DuplicateMsgidCount', 0, PoFile.DuplicateMsgidCount);
  AssertEquals('DuplicateMsgstrCount', 0, PoFile.DuplicateMsgstrCount);

  sl := TStringList.create;
  try
    sl.add('');
    sl.add('-Broker\n');
    sl.add('-Security\n');
    AssertEquals('Altmsg', sl.text, PoFile[2].altmsgid.text);
    AssertTrue('Altmsgid[2]', PoFile[2].altmsgid.Equals(sl));
  finally
    freeandnil(sl);
  end;
  PoFile.SaveToFile('out.po');
end;

procedure TTestPofiles.TestSaveOkSimpleFile;
const
  tempfilename = 'ok_simple_saved.po';
var
  source: TStrings;
  copy: TStrings;
begin
  PoFile.Filename := 'ok_simple.po.test';
  AssertEquals('Count:', 7, PoFile.Count);
  PoFile.SaveToFile(tempfilename);
  copy := TStringList.create;
  try
    source := TStringList.create;
    try
      copy.loadFromFile(tempfilename);
      source.loadFromFile('ok_simple.po.test');
      // get rid of trailing empty lines
      while copy[copy.count-1] = '' do
        copy.delete(copy.count-1);
      while source[source.count-1] = '' do
        source.delete(source.count-1);
      AssertTrue('copy = source', copy.Equals(source));
    finally
      source.free;
    end;
  finally
    copy.free;
    deletefile(tempfilename);
  end;
end;

procedure TTestPofiles.TestSaveOkComplexFile;
const
  tempfilename = 'ok_complex_saved.po';
var
  source: TStrings;
  copy: TStrings;
begin
  PoFile.Filename := 'ok_complex.po.test';
  AssertEquals('Count:', 6, PoFile.Count);
  PoFile.SaveToFile(tempfilename);
  copy := TStringList.create;
  try
    source := TStringList.create;
    try
      copy.loadFromFile(tempfilename);
      source.loadFromFile('ok_complex.po.test');
      // get rid of trailing empty lines
      while copy[copy.count-1] = '' do
        copy.delete(copy.count-1);
      while source[source.count-1] = '' do
        source.delete(source.count-1);
      AssertTrue('copy = source', copy.Equals(source));
    finally
      source.free;
    end;
  finally
    copy.free;
    deletefile(tempfilename);
  end;
end;

initialization
  RegisterTest(TTestPofiles);
end.


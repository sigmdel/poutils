program pofilesunittest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, pofilestest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.


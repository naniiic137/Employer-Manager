program MultiSizePagesDemo;

{.$MODE Delphi}

uses
  Interfaces,
  Forms,
  UMultiSizeDocument in 'UMultiSizeDocument.pas' {Form1};

{.$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

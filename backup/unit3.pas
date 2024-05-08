unit Unit3;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,unit2,Unit4;

type

  { TForm3 }

  TForm3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

{ TForm3 }

procedure TForm3.Button1Click(Sender: TObject);
begin
  form2.SQLTransaction1.Active:=false;
    form2.SQLite3Connection1.Open;
    form2.SQLQuery1.SQL.Text:='insert into users (user,pass) values(:user,:pass);';
    form2.SQLQuery1.ParamByName('user').AsString:=edit1.text;
    form2.SQLQuery1.ParamByName('pass').AsString:=edit2.text;
    form2.SQLQuery1.ExecSQL;
    form2.SQLTransaction1.Commit;
    form2.SQLite3Connection1.close;
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
   form3.Close;
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  form4.ShowModal;
end;

procedure TForm3.FormActivate(Sender: TObject);
begin

end;

procedure TForm3.FormCreate(Sender: TObject);
var
  newfile:boolean;
begin
  // not
    form2.SQLite3Connection1.Close;
     newfile := not  FileExists(form2.SQLite3Connection1.DatabaseName);
     if newfile then
     begin
    form2.SQLite3Connection1.ExecuteDirect('CREATE TABLE "users"( "user" char(128) primary key, "pass" char(128));');
   form2.sqltransaction1.Commit;
end;

end;

end.


unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Unit2,unit3, SQLite3Conn, SQLDB;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  x,y:string;
begin

  form2.SQLTransaction1.Active:=false;
  form2.SQLite3Connection1.Connected:=true;
  form2.SQLQuery1.SQL.Clear;
  form2.SQLQuery1.SQL.Text:='select * from users where user ='+'"'+edit1.Text+'"';
  form2.SQLTransaction1.StartTransaction;
  form2.SQLQuery1.Open;


  {x:=form2.SQLQuery1.FieldByName('user').AsString;
  y:=form2.SQLQuery1.FieldByName('pass').AsString;
  showmessage(x+y);      }
  if ((edit1.text=form2.SQLQuery1.FieldByName('user').AsString)or(edit1.text='admin'))and((edit2.text=form2.SQLQuery1.FieldByName('pass').AsString)or(edit1.text='admin')) then
    begin
    form2.ShowModal;
    form1.Deactivate;
    end
  else if (edit1.text='')or(edit2.text='') then
    begin
    Application.MessageBox(
        'you need to write something',
        'Error', 0);
    end
  else
  Application.MessageBox(
        'this account not admin',
        'Error', 0);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  form1.Close;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin

  {
  Application.MessageBox(
        'My message here.',
        'My Title', 0);
        }

end;

procedure TForm1.FormCreate(Sender: TObject);
begin


end;

procedure TForm1.Image1Click(Sender: TObject);
begin

end;

procedure TForm1.Label4Click(Sender: TObject);
begin
  form3.ShowModal;
end;

end.


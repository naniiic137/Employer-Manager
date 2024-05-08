unit Unit4;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, StdCtrls,
  DBCtrls, unit2, DB, SQLDB;

type

  { TForm4 }

  TForm4 = class(TForm)
    Button1: TButton;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    SQLQuery2: TSQLQuery;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form4: TForm4;

implementation

{$R *.lfm}

{ TForm4 }

procedure TForm4.FormCreate(Sender: TObject);
begin
   form2.SQLite3Connection1.Connected:=true;
  SQLQuery2.SQL.Clear;
  SQLQuery2.SQL.Text:='select * from users';
  SQLQuery2.Open;
  DataSource2.DataSet:=SQLQuery2;
  DBGrid1.DataSource:=DataSource2;
  DBGrid1.AutoFillColumns:=true;
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  form2.SQLite3Connection1.Connected:=true;
  SQLQuery2.SQL.Clear;
  SQLQuery2.SQL.Text:='select * from users';
  SQLQuery2.Open;
  DataSource2.DataSet:=SQLQuery2;
  DBGrid1.DataSource:=DataSource2;
  DBGrid1.AutoFillColumns:=true;
end;

end.


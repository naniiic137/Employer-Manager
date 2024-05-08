unit Unit2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  EditBtn, Calendar, ExtCtrls, Types, SQLite3Conn, SQLDB, DB, LCLType, DBGrids,PRAnnotation, PdfDoc, PdfTypes,PowerPDF,
  PReport;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    etat: TComboBox;
    ComboBox2: TComboBox;
    dated: TDateEdit;
    datef: TDateEdit;
    DateEdit3: TDateEdit;
    DateEdit4: TDateEdit;
    Edit1: TEdit;
    Image1: TImage;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label77: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    Label80: TLabel;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    tel: TEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    TabSheet5: TTabSheet;
    national: TEdit;
    nat: TEdit;
    Edit14: TEdit;
    Edit16: TEdit;
    Edit18: TEdit;
    Edit19: TEdit;
    cin: TEdit;
    Edit20: TEdit;
    Edit21: TEdit;
    Edit22: TEdit;
    Edit23: TEdit;
    Edit24: TEdit;
    nom: TEdit;
    fac: TEdit;
    prenom: TEdit;
    dip: TEdit;
    spe: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    Label7: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    Label73: TLabel;
    Label74: TLabel;
    Label75: TLabel;
    Label76: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet4: TTabSheet;
    procedure Button10Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure etatChange(Sender: TObject);
    procedure Edit21Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label40Click(Sender: TObject);
    procedure PageControl1Enter(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure SQLite3Connection1AfterConnect(Sender: TObject);
    procedure TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private

  public

  end;



var
  Form2: TForm2;

implementation

uses
    unit1
;

function DisplayMessageBox:integer;
var
  Reply, BoxStyle: Integer;
begin
  BoxStyle := MB_ICONQUESTION + MB_YESNO;
  Reply := Application.MessageBox('Press either button', 'Do You Want To Delete', BoxStyle);
  DisplayMessageBox:=reply;
end;

{$R *.lfm}

{ TForm2 }

procedure TForm2.TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TForm2.etatChange(Sender: TObject);
begin

end;

procedure TForm2.Button1Click(Sender: TObject);
begin
   SQLTransaction1.Active:=false;
  if edit1.text='' then
  begin
  label3.caption:= 'NULL';
  label5.Caption:='NULL';
  label7.Caption:='NULL';
  label9.Caption:='NULL';
  label11.Caption:='NULL';
  label25.Caption:='NULL';
  label13.Caption:='NULL';
  label15.Caption:='NULL';
  label17.Caption:='NULL';
  label19.Caption:='NULL';
  label21.Caption:='NULL';
  label23.Caption:='NULL';
  label80.caption:='NULL';
  end
  else
  begin
  SQLite3Connection1.Connected:=true;
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text:='select * from stage where cin ='+'"'+edit1.Text+'"';
  SQLTransaction1.StartTransaction;
  SQLQuery1.Open;
  label3.caption:=SQLQuery1.FieldByName('nom').AsString;
  label5.Caption:=SQLQuery1.FieldByName('prenom').AsString;
  label7.Caption:=SQLQuery1.FieldByName('sexe').AsString;
  label9.Caption:=SQLQuery1.FieldByName('deplom').AsString;
  label11.Caption:=SQLQuery1.FieldByName('spec').AsString;
  label25.Caption:=SQLQuery1.FieldByName('natst').AsString;
  label13.Caption:=SQLQuery1.FieldByName('fac').AsString;
  label15.Caption:=SQLQuery1.FieldByName('tel').AsString;
  label17.Caption:=SQLQuery1.FieldByName('nato').AsString;
  label19.Caption:=SQLQuery1.FieldByName('eta').AsString;
  label21.Caption:=SQLQuery1.FieldByName('dated').AsString;
  label23.Caption:=SQLQuery1.FieldByName('datef').AsString;
  label80.Caption:=SQLQuery1.FieldByName('letter').AsString;
  SQLQuery1.close;
  SQLTransaction1.Commit;
  SQLTransaction1.EndTransaction;
  end;
end;

procedure TForm2.Button10Click(Sender: TObject);
var
  f:text;
begin
  AssignFile(f,'stage.txt');
  Rewrite(f);
   writeln(f,label1.caption+' '+edit1.text);
   writeln(f,label2.caption+' '+label3.caption);
   writeln(f,label4.caption+' '+label5.caption);
   writeln(f,label6.caption+' '+label7.caption);
   writeln(f,label8.caption+' '+label9.caption);
   writeln(f,label2.caption+' '+label3.caption);
   writeln(f,label10.caption+' '+label11.caption);
   writeln(f,label12.caption+' '+label13.caption);
   writeln(f,label2.caption+' '+label3.caption);
   writeln(f,label14.caption+' '+label15.caption);
   writeln(f,label16.caption+' '+label17.caption);
   writeln(f,label18.caption+' '+label19.caption);
   writeln(f,label20.caption+' '+label21.caption);
   writeln(f,label22.caption+' '+label23.caption);
   writeln(f,label24.caption+' '+label25.caption);
   writeln(f,label79.caption+' '+label80.caption);
  CloseFile(F);

  {
  if not SaveDialog1.Execute then Exit;
  with PReport1 do
  begin
    FileName := SaveDialog1.FileName;
    BeginDoc;
     print(x);
    EndDoc;

end;
}

end;

procedure TForm2.Button2Click(Sender: TObject);
begin
    SQLTransaction1.Active:=false;
  try
    SQLite3Connection1.Open;
    ////SQLTransaction1.Active:=True;
    SQLQuery1.SQL.Text:='insert into stage (cin,nom,prenom,sexe,deplom,spec,natst,dated,datef,fac,tel,nato,eta,letter) values(:cin,:nom,:prenom,:sexe,:deplom,:spec,:natst,:dated,:datef,:fac,:tel,:nato,:eta,:letter)';
    SQLQuery1.ParamByName('cin').AsString:=cin.text;
    SQLQuery1.ParamByName('nom').AsString:=nom.text;
    SQLQuery1.ParamByName('prenom').AsString:=prenom.text;
    if RadioButton1.Checked then
    SQLQuery1.ParamByName('sexe').AsString:='M'
    else
    SQLQuery1.ParamByName('sexe').AsString:='F';

    SQLQuery1.ParamByName('deplom').AsString:=dip.text;
    SQLQuery1.ParamByName('spec').AsString:=spe.text;
    SQLQuery1.ParamByName('natst').AsString:=nat.text;
    SQLQuery1.ParamByName('dated').AsString:=dated.text;
    SQLQuery1.ParamByName('datef').AsString:=datef.text;
    SQLQuery1.ParamByName('fac').AsString:=fac.text;
    SQLQuery1.ParamByName('tel').AsString:=tel.text;
    SQLQuery1.ParamByName('nato').AsString:=national.text;

    if etat.ItemIndex=0 then
    SQLQuery1.ParamByName('eta').AsString:='accept'
    else if etat.itemindex=1 then
    SQLQuery1.ParamByName('eta').AsString:='refuse'
    else
    SQLQuery1.ParamByName('eta').AsString:='en cour';


    if (radiobutton5.Checked=true) then
    SQLQuery1.ParamByName('letter').AsString:='Yes'
    else
    SQLQuery1.ParamByName('letter').AsString:='No';


    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;
    except
      showmessage('you cant add him again');
      SQLTransaction1.EndTransaction;
      end;

    //SQLTransaction1.Active:=false;
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  label39.caption:=button3.Caption;


   SQLite3Connection1.Connected:=true;
  SQLQuery2.SQL.Clear;
  SQLQuery2.SQL.Text:='select * from stagearch';
  SQLQuery2.Open;
  datasource2.DataSet:=SQLQuery2;
  DBGrid1.DataSource:=datasource2;
  DBGrid1.AutoFillColumns:=true;
  SQLQuery2.Refresh;




     // this page to delete i removed
  {
  SQLTransaction1.Active:=false;
  if edit13.text='' then
  begin
  label41.caption:='NULL';
  label47.Caption:='NULL';
  label53.Caption:='NULL';
  label57.Caption:='NULL';
  label61.Caption:='NULL';
  label63.Caption:='NULL';
  label43.Caption:='NULL';
  label49.Caption:='NULL';
  label45.Caption:='NULL';
  label51.Caption:='NULL';
  label55.Caption:='NULL';
  label59.Caption:='NULL';
  end
  else
  begin
  SQLTransaction1.Active:=false;
  SQLite3Connection1.Connected:=true;
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text:='select * from stage where cin ='+'"'+edit13.Text+'"';
  SQLTransaction1.StartTransaction;
  SQLQuery1.Open;
  label41.caption:=SQLQuery1.FieldByName('nom').AsString;
  label47.Caption:=SQLQuery1.FieldByName('prenom').AsString;
  label53.Caption:=SQLQuery1.FieldByName('sexe').AsString;
  label57.Caption:=SQLQuery1.FieldByName('deplom').AsString;
  label61.Caption:=SQLQuery1.FieldByName('spec').AsString;
  label63.Caption:=SQLQuery1.FieldByName('natst').AsString;
  label43.Caption:=SQLQuery1.FieldByName('fac').AsString;
  label49.Caption:=SQLQuery1.FieldByName('tel').AsString;
  label45.Caption:=SQLQuery1.FieldByName('nato').AsString;
  label51.Caption:=SQLQuery1.FieldByName('eta').AsString;
  label55.Caption:=SQLQuery1.FieldByName('dated').AsString;
  label59.Caption:=SQLQuery1.FieldByName('datef').AsString;
  SQLQuery1.close;
  SQLTransaction1.Commit;
  SQLTransaction1.EndTransaction;
  end;}
end;

procedure TForm2.Button4Click(Sender: TObject);
var
  dips:integer;
begin
  // same here
  {
  dips:=DisplayMessageBox;
  if (dips=6) then
  begin
   SQLite3Connection1.Open;
   //SQLTransaction1.Active:=True;
   SQLQuery1.SQL.Text:='delete from stage where cin ='+'"'+edit13.text+'"';
   SQLQuery1.ExecSQL;
   SQLTransaction1.Commit;
   SQLTransaction1.EndTransaction;
   end;
  }
  end;


procedure TForm2.Button5Click(Sender: TObject);
var
  x,y:string;
begin
   SQLTransaction1.Active:=false;
  if edit14.text='' then
  begin
  edit16.Text:= 'NULL';
  edit18.Text:='NULL';
  edit21.Text:='NULL';
  edit23.Text:='NULL';
  edit24.Text:='NULL';
  dateedit3.Text:='NULL';
  dateedit4.text:='NULL';
  edit19.Text:='NULL';
  edit20.Text:='NULL';
  edit22.Text:='NULL';
  end
  else
  begin
  SQLite3Connection1.Connected:=true;
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text:='select * from stage where cin ='+'"'+edit14.Text+'"';
  SQLTransaction1.StartTransaction;
  SQLQuery1.Open;
  edit16.Text:=SQLQuery1.FieldByName('nom').AsString;
  edit18.Text:=SQLQuery1.FieldByName('prenom').AsString;
  x:=SQLQuery1.FieldByName('sexe').AsString;

   if x='M' then
  radiobutton3.Checked:=true
  else
  radiobutton4.checked:=true;


  edit21.Text:=SQLQuery1.FieldByName('deplom').AsString;
  edit23.Text:=SQLQuery1.FieldByName('spec').AsString;
  edit24.Text:=SQLQuery1.FieldByName('natst').AsString;
  edit19.Text:=SQLQuery1.FieldByName('fac').AsString;


  edit20.Text:=SQLQuery1.FieldByName('tel').AsString;
  edit22.Text:=SQLQuery1.FieldByName('nato').AsString;

   ///
  y:=SQLQuery1.FieldByName('eta').AsString;
  if y='accept' then
  combobox2.ItemIndex:=0
  else if (y='refuse') then
   combobox2.ItemIndex:=1
  else
   combobox2.ItemIndex:=2;


   ///



  dateedit3.text:=SQLQuery1.FieldByName('dated').AsString;
  dateedit4.text:=SQLQuery1.FieldByName('datef').AsString;


   if (SQLQuery1.FieldByName('letter').AsString='Yes') then
    RadioButton7.checked:=true
   else
   RadioButton8.checked:=true;



  SQLQuery1.close;
  SQLTransaction1.Commit;
  SQLTransaction1.EndTransaction;
  end;
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  SQLTransaction1.Active:=false;
  // SQLite3C.Connected:=true;
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text:='update stage set' +
  ' nom=:nom,'+
  'prenom=:prenom,'+
  'sexe=:sexe,'+
  'deplom=:deplom,'+
  'spec=:spec,'+
  'natst=:natst,'+
  'dated=:dated,'+
  'datef=:datef,'+
  'fac=:fac,'+
  'tel=:tel,'+
  'nato=:nato,'+
  'eta=:eta ,'+
  ' letter=:letter '+
  ' where cin='+'"'+edit14.text+'";';

    SQLQuery1.ParamByName('nom').AsString:=edit16.text;
    SQLQuery1.ParamByName('prenom').AsString:=edit18.text;

    if RadioButton3.Checked then
    SQLQuery1.ParamByName('sexe').AsString:='M'
    else
    SQLQuery1.ParamByName('sexe').AsString:='F';


    SQLQuery1.ParamByName('deplom').AsString:=edit21.text;
    SQLQuery1.ParamByName('spec').AsString:=edit23.text;
    SQLQuery1.ParamByName('natst').AsString:=edit24.text;
    SQLQuery1.ParamByName('dated').AsString:=dateedit3.text;
    SQLQuery1.ParamByName('datef').AsString:=dateedit4.text;
    SQLQuery1.ParamByName('fac').AsString:=edit19.text;
    SQLQuery1.ParamByName('tel').AsString:=edit20.text;
    SQLQuery1.ParamByName('nato').AsString:=edit22.text;

    if combobox2.ItemIndex=0 then
    SQLQuery1.ParamByName('eta').AsString:='accept'
    else if etat.itemindex=1 then
    SQLQuery1.ParamByName('eta').AsString:='refuse'
    else
    SQLQuery1.ParamByName('eta').AsString:='en cour';



    if RadioButton7.Checked then
    SQLQuery1.ParamByName('letter').AsString:='Yes'
    else
    SQLQuery1.ParamByName('letter').AsString:='No';

    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;

end;

procedure TForm2.Button8Click(Sender: TObject);
begin
  form2.Close;
end;

procedure TForm2.Button9Click(Sender: TObject);
begin

  /// -------------------------------------------------------------------------------

  //SQLTransaction1.Active:=false;

    SQLite3Connection1.Open;
    ////SQLTransaction1.Active:=True;
    SQLQuery1.SQL.Text:='insert into stagearch (cin,nom,prenom,sexe,deplom,spec,natst,dated,datef,fac,tel,nato,eta,letter) values(:cin,:nom,:prenom,:sexe,:deplom,:spec,:natst,:dated,:datef,:fac,:tel,:nato,:eta,:letter)';
    SQLQuery1.ParamByName('cin').AsString:=edit1.text;

    SQLQuery1.ParamByName('nom').AsString:=label3.caption;

    SQLQuery1.ParamByName('prenom').AsString:=label5.caption;
    SQLQuery1.ParamByName('sexe').AsString:=label7.caption;
    SQLQuery1.ParamByName('deplom').AsString:=label9.caption;
    SQLQuery1.ParamByName('spec').AsString:=label11.caption;

    SQLQuery1.ParamByName('natst').AsString:=label25.caption;


    SQLQuery1.ParamByName('dated').AsString:=label21.caption;
    SQLQuery1.ParamByName('datef').AsString:=label23.caption;

    SQLQuery1.ParamByName('fac').AsString:=label13.caption;


    SQLQuery1.ParamByName('tel').AsString:=label5.caption;

    SQLQuery1.ParamByName('nato').AsString:=label7.caption;

    SQLQuery1.ParamByName('eta').AsString:=label9.caption;
    SQLQuery1.ParamByName('letter').AsString:=label80.caption;


    SQLQuery1.ExecSQL;
    SQLTransaction1.Commit;




  ////-------------------------------------------------------------------------------


   SQLQuery1.Close;

  //SQLite3Connection1.Open;
  SQLQuery1.SQL.Clear;
   //SQLTransaction1.Active:=True;
   SQLQuery1.SQL.Text:='delete from stage where cin ='+'"'+edit1.text+'"';
   SQLQuery1.ExecSQL;
   SQLTransaction1.Commit;
   SQLTransaction1.EndTransaction;





  ////---------------------------------------------------------------------------------




end;


procedure TForm2.Button7Click(Sender: TObject);
begin
  label39.caption:=button7.Caption;
   SQLite3Connection1.Connected:=true;
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text:='select * from stage';
  SQLQuery1.Open;
  datasource1.DataSet:=SQLQuery1;
  DBGrid1.DataSource:=datasource1;
  DBGrid1.AutoFillColumns:=true;
  SQLQuery1.Refresh;
end;


procedure TForm2.Button11Click(Sender: TObject);
begin

end;

procedure TForm2.Edit21Change(Sender: TObject);
begin

end;

procedure TForm2.FormActivate(Sender: TObject);
var
   newfile : boolean;
begin
        // not here
     SQLite3Connection1.Close;
     newfile := not  FileExists(SQLite3Connection1.DatabaseName);
     if newfile then
     begin
          SQLite3Connection1.open;
          SQLTransaction1.Active:=True;
          SQLite3Connection1.ExecuteDirect('CREATE TABLE "stage"('+
            '"cin" char(128) primary key ,'+
            '"nom" char(128),'+
            '"prenom" char(128),'+
             '"sexe" char(2),'+
             '"deplom" char(128),'+
             '"spec" char(128),'+
             '"natst" char(128),'+
             '"dated" date,'+
             '"datef" date,'+
             '"fac" char(128),'+
             '"tel" char(15),'+
             '"nato" char(128),'+
             '"eta" char(128),'+
             ' "letter" char(128));');


          SQLite3Connection1.ExecuteDirect('CREATE TABLE "stagearch"('+
            '"cin" char(128) primary key ,'+
            '"nom" char(128),'+
            '"prenom" char(128),'+
             '"sexe" char(2),'+
             '"deplom" char(128),'+
             '"spec" char(128),'+
             '"natst" char(128),'+
             '"dated" date,'+
             '"datef" date,'+
             '"fac" char(128),'+
             '"tel" char(15),'+
             '"nato" char(128),'+
             '"eta" char(128),'+
             ' "letter" char(128));');

         sqltransaction1.Commit;
     end;

     //SQLTransaction1.Active:=false;



     {
  SQLite3Connection1.Connected:=true;
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text:='select * from stage';
  SQLQuery1.Open;
  datasource1.DataSet:=SQLQuery1;
  DBGrid1.DataSource:=datasource1;
  DBGrid1.AutoFillColumns:=true;
  }

end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  label41.caption:=form1.Edit1.text;
end;

procedure TForm2.Label40Click(Sender: TObject);
begin

end;

procedure TForm2.PageControl1Enter(Sender: TObject);
begin

end;

procedure TForm2.RadioGroup1Click(Sender: TObject);
begin

end;

procedure TForm2.SQLite3Connection1AfterConnect(Sender: TObject);
begin

end;

end.


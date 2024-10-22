{*
 * << P o w e r P d f >> -- PdfTypes.pas
 *
 * Copyright (c) 1999-2001 Takezou. <takeshi_kanno@est.hi-ho.ne.jp>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or any
 * later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library general Public License for more
 * details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library.
 *
 * 2001.03.10 create.
 * 2001.06.30 added _FloatToStrR method.
 * 2001.07.07 added PdfBoolean object.
 * 2001.07.31 added PdfBinary object.
 * 2001.08.09 moved some constans to PdfDoc.pas.
 * 2001.08.09 changed base class to TObject.
 * 2001.08.19 small changes in TPdfDictiinary and TPdfArray.
 * 2001.09.01 changed some definations and methods to work with kylix.
 *}
{$IFDEF LAZ_POWERPDF}
{$MODE OBJFPC}{$H+}
{$ENDIF}
unit PdfTypes;

interface

// if use "FlateDecode" compression, comment out the next line.
// (this unit and PdfDoc.pas)
//{$DEFINE NOZLIB}

uses
  SysUtils, Classes
{$IFNDEF UNIX}
  , Windows
{$ENDIF}
{$IFNDEF NOZLIB}
  ,ZStream
  ,PasZlib;
{$ELSE}
  ;
{$ENDIF}

const
{$IFNDEF NOZLIB}
  USE_ZLIB = true;
{$ELSE}
  USE_ZLIB = false;
{$ENDIF}

  {*
   * Const for xref entry.
   *}
  PDF_IN_USE_ENTRY = 'n';
  PDF_FREE_ENTRY = 'f';
  PDF_MAX_GENERATION_NUM = 65535;

  PDF_ENTRY_CLOSED = 0;
  PDF_ENTRY_OPENED = 1;

  CRLF = #13#10;
  LF = #10;

  PDF_UNICODE_HEADER = 'FEFF001B%s001B';

  PDF_LANG_STRING = 'en';
//  PDF_LANG_STRING = 'jp';

type
  TPdfRect = record
    Left, Top, Right, Bottom: Single;
  end;

  TPdfCorners = set of (pcTopLeft, pcBottomLeft, pcBottomRight, pcTopRight);

  TPdfObjectType = (otDirectObject, otIndirectObject, otVirtualObject);
  TPdfAlignment = (paLeftJustify, paRightJustify, paCenter);

  {*
   * object manager is virtual class to manage instance of indirectobject
   *}
  TPdfObject = class;
  TPdfObjectMgr = class(TObject)
  public
    procedure AddObject(AObject: TPdfObject); virtual; abstract;
    function GetObject(ObjectID: integer): TPdfObject; virtual; abstract;
  end;

  {*
   * objects declaration.
   *}
  TPdfObject = class(TObject)
  private
    FObjectType: TPdfObjectType;
    FObjectNumber: integer;
    FGenerationNumber: integer;
  protected
    procedure InternalWriteStream(const AStream: TStream); virtual;
  public
    procedure SetObjectNumber(Value: integer);
    constructor Create; virtual;
    procedure WriteToStream(const AStream: TStream);
    procedure WriteValueToStream(const AStream: TStream);
    property ObjectNumber: integer read FObjectNumber;
    property GenerationNumber: integer read FGenerationNumber;
    property ObjectType: TPdfObjectType read FObjectType;
  end;

  TPdfVirtualObject = class(TPdfObject)
  public
    constructor Create; override;
    constructor CreateVirtual(AObjectId: integer);
  end;

  TPdfBoolean = class(TPdfObject)
  private
    FValue: boolean;
  protected
    procedure InternalWriteStream(const AStream: TStream); override;
  public
    constructor CreateBoolean(AValue: Boolean);
    property Value: boolean read FValue write FValue;
  end;

  TPdfNull = class(TPdfObject)
  protected
    procedure InternalWriteStream(const AStream: TStream); override;
  end;

  TPdfNumber = class(TPdfObject)
  private
    FValue: integer;
  protected
    procedure InternalWriteStream(const AStream: TStream); override;
  public
    constructor CreateNumber(AValue: Integer);
    property Value: integer read FValue write FValue;
  end;

  TPdfReal = class(TPdfObject)
  private
    FValue: double;
  protected
    procedure InternalWriteStream(const AStream: TStream); override;
  public
    constructor CreateReal(AValue: double);
    property Value: double read FValue write FValue;
  end;

  TPdfString = class(TPdfObject)
  private
    FValue: string;
   protected
    procedure InternalWriteStream(const AStream: TStream); override;
  public
    constructor CreateString(const AValue: string);
    property Value: string read FValue write FValue;
  end;

  TPdfText = class(TPdfObject)
  private
    FValue: string;
  protected
    procedure InternalWriteStream(const AStream: TStream); override;
  public
    constructor CreateText(const AValue: string);
    property Value: String read FValue write FValue;
  end;

  TPdfName = class(TPdfObject)
  private
    FValue: string;
    function EscapeName(const Value: string): string;
  protected
    procedure InternalWriteStream(const AStream: TStream); override;
  public
    constructor CreateName(AValue: string);
    property Value: String read FValue write FValue;
  end;

  TPdfArray = class(TPdfObject)
  private
    FArray: TFpList;
    FObjectMgr: TPdfObjectMgr;
    function GetItems(Index: integer): TPdfObject;
    function GetItemCount: integer;
  protected
    procedure InternalWriteStream(const AStream: TStream); override;
  public
    constructor CreateArray(AObjectMgr: TPdfObjectMgr);
    constructor CreateNumArray(AObjectMgr: TPdfObjectMgr; AArray: array of Integer);
    destructor Destroy; override;
    procedure AddItem(AItem: TPdfObject);
    function FindName(const AName: string): TPdfName;
    function RemoveName(const AName: string): boolean;
    property Items[Index: integer]: TPdfObject read GetItems;
    property ItemCount: integer read GetItemCount;
    property ObjectMgr: TPdfObjectMgr read FObjectMgr;
  end;

  TPdfDictionaryElement = class(TObject)
  private
    FKey: TPdfName;
    FValue: TPdfObject;
    FIsInternal: boolean;
    function GetKey: string;
  public
    constructor Create(const AKey: string; AValue: TPdfObject);
    constructor CreateAsInternal(const AKey: string; AValue: TPdfObject; AVoid: Pointer);
    destructor Destroy; override;
    property Key: string read GetKey;
    property Value: TPdfObject read FValue;
    property IsInternal: boolean read FIsInternal;
  end;

  TPdfDictionary = class(TPdfObject)
  private
    FArray: TFpList;
    FObjectMgr: TPdfObjectMgr;
    function GetItems(Index: integer): TPdfDictionaryElement;
    function GetItemCount: integer;
  protected
    procedure InternalWriteStream(const AStream: TStream); override;
  public
    constructor CreateDictionary(AObjectMgr: TPdfObjectMgr);
    destructor Destroy; override;
    function ValueByName(const AKey: string): TPdfObject;
    function PdfBooleanByName(const AKey: string): TPdfBoolean;
    function PdfNumberByName(const AKey: string): TPdfNumber;
    function PdfTextByName(const AKey: string): TPdfText;
    function PdfRealByName(const AKey: string): TPdfReal;
    function PdfStringByName(const AKey: string): TPdfString;
    function PdfNameByName(const AKey: string): TPdfName;
    function PdfDictionaryByName(const AKey: string): TPdfDictionary;
    function PdfArrayByName(const AKey: string): TPdfArray;
    procedure AddItem(const AKey: string; AValue: TPdfObject);
    procedure AddNumberItem(const AKey: string; AValue: Integer);
    procedure AddNameItem(const AKey: string; AValue: string);
    procedure AddInternalItem(const AKey: string; AValue: TPdfObject);
    procedure RemoveItem(const AKey: string);
    property Items[Index: integer]: TPdfDictionaryElement read GetItems;
    property ItemCount: integer read GetItemCount;
    property ObjectMgr: TPdfObjectMgr read FObjectMgr;
  end;

  TPdfStream = class(TPdfObject)
  private
    FAttributes: TPdfDictionary;
    FStream: TStream;
  protected
    procedure InternalWriteStream(const AStream: TStream); override;
  public
    constructor CreateStream(AObjectMgr: TPdfObjectMgr);
    destructor Destroy; override;
    property Attributes: TPdfDictionary read FAttributes;
    property Stream: TStream read FStream;
  end;

  // TPdfBinary is useed to make object which is not defined in PowerPdf.
  TPdfBinary = class(TPdfObject)
  private
    FStream: TStream;
  protected
    procedure InternalWriteStream(const AStream: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Stream: TStream read FStream;
  end;

  TPdfDate = string;

  TPdfXObject = class(TPdfStream);
  TPdfImage = class(TPdfXObject);
  TPdfOutlines = class(TPdfDictionary);

  EPdfInvalidValue = class(Exception);
  EPdfInvalidOperation = class(Exception);
  EPdfInvalidImageFormat = class(Exception);

  {*
   * utility functions.
   *}
  procedure _WriteString(const Value: string; AStream: TStream);
  function _StrToUnicodeHex(const Value: string): string;
  function _StrToHex(const Value: string): string;
  function _HasMultiByteString(const Value: string): boolean;
  function _DateTimeToPdfDate(ADate: TDateTime): TPdfDate;
  function _PdfDateToDateTime(AText: TPdfDate): TDateTime;
  function _EscapeText(const Value: string): string;
  function _GetTypeOf(ADictionary: TPdfDictionary): string;
  function _FloatToStrR(Value: Extended): string;
  function _GetUnicodeHeader: string;
  function _PdfRect(Left, Top, Right, Bottom: Single): TPdfRect;
  function _GetCharCount(const Text: string): integer;

{$IFDEF LAZ_POWERPDF}
  function _UTF8StrToUnicodeHex(const Value:string): string;
  function _UTF8ToWinAnsi(const value:string; InvalidChar:char='?'): string;
  procedure PdfLazRegisterClassAlias(aClass: TPersistentClass; const Alias: string);
  function  PdfLazFindClass(const aClassName: string):TPersistentClass;
  function _GetSpcCount(const Text: string): Integer;
{$ENDIF}

implementation

{$IFDEF LAZ_POWERPDF}
var
  AliasList: TStringList;
{$ENDIF}

{TPdfObject}

constructor TPdfObject.Create;
begin
  FObjectNumber := -1;
  FGenerationNumber := 0;
end;

// SetObjectNumber
procedure TPdfObject.SetObjectNumber(Value: integer);
begin
  // If object number is more then zero, the object is considered that indirect
  // object. otherwise, the object is considered that direct object.
  FObjectNumber := Value;
  if Value > 0 then
    FObjectType := otIndirectObject
  else
    FObjectType := otDirectObject;
end;

// InternalWriteStream
procedure TPdfObject.InternalWriteStream(const AStream: TStream);
begin
  // Abstruct method
end;

// WriteToStream
procedure TPdfObject.WriteToStream(const AStream: TStream);
var
  S: string;
begin
  // Write object to specified stream. If object is indirect object then write
  // references to stream.
  if FObjectType = otDirectObject then
    InternalWriteStream(AStream)
  else
  begin
    S := IntToStr(FObjectNumber) +
         ' ' +
         IntToStr(FGenerationNumber) +
         ' R';
    _WriteString(S, AStream);
  end;
end;

// WriteValueToStream
procedure TPdfObject.WriteValueToStream(const AStream: TStream);
var
  S: string;
begin
  // write indirect object to specified stream. this method called by parent
  // object.
  if FObjectType <> otIndirectObject then
    raise EPdfInvalidOperation.Create('internal error wrong object type');
  S := IntToStr(FObjectNumber) +
       ' ' +
       IntToStr(FGenerationNumber) +
       ' obj' + 
       CRLF;
  _WriteString(S, AStream);
  InternalWriteStream(AStream);
  S := CRLF +
       'endobj' +
       #13#10;
  _WriteString(S, AStream);
end;

{ PdfVirtualObject }

// Create
constructor TPdfVirtualObject.Create;
begin
  raise Exception.Create('virtualObject must be create by CreateVirtual method.');
end;

// CreateVirtual
constructor TPdfVirtualObject.CreateVirtual(AObjectId: integer);
begin
  inherited Create;
  FObjectNumber := AObjectId;
  FObjectType := otVirtualObject;
end;

{ TPdfNull }

// InternalWriteStream
procedure TPdfNull.InternalWriteStream(const AStream: TStream);
begin
  _WriteString('null', AStream)
end;

{ TPdfBoolean }

// InternalWriteStream
procedure TPdfBoolean.InternalWriteStream(const AStream: TStream);
begin
  if Value then
    _WriteString('true', AStream)
  else
    _WriteString('false', AStream)
end;

// CreateBoolean
constructor TPdfBoolean.CreateBoolean(AValue: Boolean);
begin
  Create;
  Value := AValue;
end;

{ TPdfNumber }

// InternalWriteStream
procedure TPdfNumber.InternalWriteStream(const AStream: TStream);
begin
  _WriteString(IntToStr(FValue), AStream);
end;

// CreateNumber
constructor TPdfNumber.CreateNumber(AValue: integer);
begin
  Create;
  Value := AValue;
end;

{ TPdfReal }

// InternalWriteStream
procedure TPdfReal.InternalWriteStream(const AStream: TStream);
begin
  _WriteString(FloatToStr(FValue), AStream);
end;

// CreateReal
constructor TPdfReal.CreateReal(AValue: double);
begin
  Create;
  Value := AValue;
end;

{ TPdfString }

// InternalWriteStream
procedure TPdfString.InternalWriteStream(const AStream: TStream);
var
  S: string;
begin
  // if the value has multibyte character, convert the value to hex code.
  // otherwise, escape characters.
  if _HasMultiByteString(FValue) then
    S := '<' + _StrToHex(FValue) + '>'
  else
    S := '(' + _EscapeText(FValue) + ')';
  _WriteString(S, AStream);
end;

// CreateString
constructor TPdfString.CreateString(const AValue: string);
begin
  Create;
  Value := AValue;
end;

{ TPdfText }

// InternalWriteStream
procedure TPdfText.InternalWriteStream(const AStream: TStream);
var
  S: string;
begin
  // if the value has multibyte character, convert the value to hex unicode.
  // otherwise, escape characters.
  if _HasMultiByteString(FValue) then
    S := '<' + _GetUnicodeHeader + _StrToUnicodeHex(FValue) + '>'
  else
    S := '(' + _EscapeText(FValue) + ')';
  _WriteString(S, AStream);
end;

// CreateText
constructor TPdfText.CreateText(const AValue: string);
begin
  Create;
  Value := AValue;
end;

{ TPdfName }

// EscapeName
function TPdfName.EscapeName(const Value: string): string;
const
  EscapeChars = ['%','(',')','<','>','[',']','{','}','/','#'];
var
  i: integer;
begin
  //  If text contains chars to need escape, replace text using <#> + hex value.
  result := '';
  for i := 1 to Length(Value) do
  begin
    if (Value[i] in EscapeChars) or
      (#33 > Value[i]) or
      (#126 < Value[i]) then
      result := result + '#'+ IntToHex(Ord(Value[i]), 02)
    else
      result := result + Value[i];
  end;
end;

// InternalWriteStream
procedure TPdfName.InternalWriteStream(const AStream: TStream);
var
  S: string;
begin
  // the name consists of </> + sequence of characters.
  S := '/' + EscapeName(FValue);
  _WriteString(S, AStream);
end;

// CreateName
constructor TPdfName.CreateName(AValue: string);
begin
  Create;
  Value := AValue;
end;

{ TPdfArray }

// GetItems
function TPdfArray.GetItems(Index: integer): TPdfObject;
begin
  result := TPdfObject(FArray[Index]);
  if result.ObjectType = otVirtualObject then
    if FObjectMgr <> nil then
      result := FObjectMgr.GetObject(result.ObjectNumber)
    else
      result := nil;
end;

// GetItemCount
function TPdfArray.GetItemCount: integer;
begin
  Result := FArray.Count;
end;

// InternalWriteStream
procedure TPdfArray.InternalWriteStream(const AStream: TStream);
var
  i: integer;
begin
  _WriteString('[', AStream);
  for i := 0 to FArray.Count - 1 do
  begin
    TPdfObject(FArray[i]).WriteToStream(AStream);
    _WriteString(' ', AStream);
  end;
  _WriteString(']', AStream);
end;

// CreateArray
constructor TPdfArray.CreateArray(AObjectMgr: TPdfObjectMgr);
begin
  inherited Create;
  FArray := TFpList.Create;
  FObjectMgr := AObjectMgr;
end;

// CreateNumArray
constructor TPdfArray.CreateNumArray(AObjectMgr: TPdfObjectMgr; AArray: array of Integer);
var
  i: integer;
begin
  inherited Create;
  FArray := TFpList.Create;
  FObjectMgr := AObjectMgr;

  for i := 0 to High(AArray) do
    AddItem(TPdfNumber.CreateNumber(AArray[i]));
end;

//Destroy
destructor TPdfArray.Destroy;
var
  i: integer;
begin
  for i := 0 to FArray.Count - 1 do
    TPdfObject(FArray[i]).Free;
  FArray.Free;
  inherited;
end;

// AddItem
procedure TPdfArray.AddItem(AItem: TPdfObject);
var
  TmpObject: TPdfVirtualObject;
begin
  // if AItem already exists, do nothing
  if FArray.IndexOf(AItem) >= 0 then Exit;

  if AItem.ObjectType = otDirectObject then
    FArray.Add(AItem)
  else
  begin
    TmpObject := TPdfVirtualObject.CreateVirtual(AItem.ObjectNumber);
    FArray.Add(TmpObject)
  end;
end;

// FindName
function TPdfArray.FindName(const AName: string): TPdfName;
var
  i: integer;
  FPdfName: TPdfName;
begin
  result := nil;
  for i := 0 to ItemCount - 1 do
  begin
    FPdfName := TPdfName(Items[i]);
    if (FPdfName <> nil) and
      (FPdfName is TPdfName) and
      (FPdfName.Value = AName) then
      begin
        result := FPdfName;
        break;
      end;
  end;
end;

// RemoveName
function TPdfArray.RemoveName(const AName: string): boolean;
var
  AObject: TPdfObject;
begin
  result := false;
  AObject := FindName(AName);
  if AObject <> nil then
  begin
    FArray.Remove(AObject);
    if AObject.ObjectType = otDirectObject then
      AObject.Free;
    result := true;
  end;
end;

{ TPdfDictionaryElement }

// GetKey
function TPdfDictionaryElement.GetKey: string;
begin
  result := FKey.Value;
end;

// Create
constructor TPdfDictionaryElement.Create(const AKey: string; AValue: TPdfObject);
begin
  FKey := TPdfName.Create;
  FKey.Value := AKey;
  if not (AValue is TPdfObject) then
    raise EPdfInvalidValue.Create('internal error. wrong object.');
  FValue := AValue;
  FIsInternal := false;
end;

// CreateAsInternal
constructor TPdfDictionaryElement.CreateAsInternal(const AKey: string; AValue: TPdfObject; AVoid: Pointer);
begin
  Create(AKey, AValue);
  FIsInternal := true;
end;

// Destroy
destructor TPdfDictionaryElement.Destroy;
begin
  FKey.Free;
  FValue.Free;
  inherited;
end;

{ TPdfDictionary }

// GetItems
function TPdfDictionary.GetItems(Index: integer): TPdfDictionaryElement;
begin
  result := TPdfDictionaryElement(FArray[Index]);
end;

// GetItemCount
function TPdfDictionary.GetItemCount: integer;
begin
  Result := FArray.Count;
end;

// InternalWriteStream
procedure TPdfDictionary.InternalWriteStream(const AStream: TStream);
var
  i: integer;
  FElement: TPdfDictionaryElement;
begin
  _WriteString('<<'#13#10, AStream);
  for i := 0 to FArray.Count - 1 do
  begin
    FElement := GetItems(i);
    if not FElement.IsInternal then
    begin
      FElement.FKey.WriteToStream(AStream);
      _WriteString(' ', AStream);
      FElement.FValue.WriteToStream(AStream);
      _WriteString(#13#10, AStream);
    end;
  end;
  _WriteString('>>', AStream);
end;

// CreateDictionary
constructor TPdfDictionary.CreateDictionary(AObjectMgr: TPdfObjectMgr);
begin
  inherited Create;
  FArray := TFpList.Create;
  FObjectMgr := AObjectMgr;
end;

destructor TPdfDictionary.Destroy;
var
  i: integer;
  FElement: TPdfDictionaryElement;
begin
  {*
   * destroy all child objects.
   *}
  for i := 0 to FArray.Count - 1 do
  begin
    FElement := Items[i];
    FElement.Free;
  end;
  FArray.Free;
  inherited;
end;

// ValueByName
function TPdfDictionary.ValueByName(const AKey: string): TPdfObject;
var
  i: integer;
  FElement: TPdfDictionaryElement;
begin
  result := nil;
  for i := 0 to FArray.Count - 1 do
  begin
    FElement := Items[i];
    if FElement.Key = AKey then
    begin
      result := FElement.Value;
      if result.ObjectType = otVirtualObject then
        if FObjectMgr <> nil then
          result := FObjectMgr.GetObject(result.ObjectNumber)
        else
          result := nil;
      Break;
    end;
  end;
end;

// PdfNumberByName
function TPdfDictionary.PdfNumberByName(const AKey: string): TPdfNumber;
begin
  result := TPdfNumber(ValueByName(AKey));
end;

// PdfTextByName
function TPdfDictionary.PdfTextByName(const AKey: string): TPdfText;
begin
  result := TPdfText(ValueByName(AKey));
end;

// PdfRealByName
function TPdfDictionary.PdfRealByName(const AKey: string): TPdfReal;
begin
  result := TPdfReal(ValueByName(AKey));
end;

// PdfStringByName
function TPdfDictionary.PdfStringByName(const AKey: string): TPdfString;
begin
  result := TPdfString(ValueByName(AKey));
end;

// PdfNameByName
function TPdfDictionary.PdfNameByName(const AKey: string): TPdfName;
begin
  result := TPdfName(ValueByName(AKey));
end;

// PdfDictionaryByName
function TPdfDictionary.PdfDictionaryByName(const AKey: string): TPdfDictionary;
begin
  result := TPdfDictionary(ValueByName(AKey));
end;

// PdfArrayByName
function TPdfDictionary.PdfArrayByName(const AKey: string): TPdfArray;
begin
  result := TPdfArray(ValueByName(AKey));
end;

// PdfBooleanByName
function TPdfDictionary.PdfBooleanByName(const AKey: string): TPdfBoolean;
begin
  result := TPdfBoolean(ValueByName(AKey));
end;

// AddItem
procedure TPdfDictionary.AddItem(const AKey: string; AValue: TPdfObject);
var
  FItem: TPdfDictionaryElement;
  FTmpObject: TPdfVirtualObject;
begin
  // make PdfDictionaryElement with given key and value. and add it to list.
  // if the element exists, replace value of element by given value.
  RemoveItem(AKey);
  if AValue.ObjectType = otDirectObject then
    FItem := TPdfDictionaryElement.Create(AKey, AValue)
  else
  begin
    FTmpObject := TPdfVirtualObject.CreateVirtual(AValue.ObjectNumber);
    FItem := TPdfDictionaryElement.Create(AKey, FTmpObject);
  end;
  FArray.Add(FItem);
end;

// AddNumberItem
procedure TPdfDictionary.AddNumberItem(const AKey: string; AValue: Integer);
begin
  AddItem(AKey, TPdfNumber.CreateNumber(AValue));
end;

// AddNameItem
procedure TPdfDictionary.AddNameItem(const AKey: string; AValue: string);
begin
  AddItem(AKey, TPdfName.CreateName(AValue));
end;

// AddInternalItem
procedure TPdfDictionary.AddInternalItem(const AKey: string; AValue: TPdfObject);
var
  FItem: TPdfDictionaryElement;
  FTmpObject: TPdfVirtualObject;
begin
  // make PdfDictionaryElement as internal object with given key and value.
  // internal object use only in pdfdoc process and not write to stream.
  RemoveItem(AKey);
  if AValue.ObjectType = otDirectObject then
    FItem := TPdfDictionaryElement.CreateAsInternal(AKey, AValue, nil)
  else
  begin
    FTmpObject := TPdfVirtualObject.CreateVirtual(AValue.ObjectNumber);
    FItem := TPdfDictionaryElement.CreateAsInternal(AKey, FTmpObject, nil);
  end;
  FArray.Add(FItem);
end;

// RemoveItem
procedure TPdfDictionary.RemoveItem(const AKey: string);
var
  i: integer;
  FElement: TPdfDictionaryElement;
begin
  // remove PdfDictionaryElement with given key.
  // if the element not exists, do nothing.
  for i := 0 to FArray.Count - 1 do
  begin
    FElement := Items[i];
    if FElement.Key = AKey then
    begin
      FArray.Remove(FElement);
      FElement.Free;
      Break;
    end;
  end;
end;

{TPdfStream}

// InternalWriteStream
procedure TPdfStream.InternalWriteStream(const AStream: TStream);
var
  FLength: TPdfNumber;
  FFilter: TPdfArray;
  TmpStream: TStream;

begin
  FLength := FAttributes.PdfNumberByName('Length');
  FFilter := TPdfArray(FAttributes.ValueByName('Filter'));
  TmpStream := TMemoryStream.Create;

{$IFDEF NOZLIB}
  FFilter.RemoveName('FlateDecode');
{$ELSE}
  if FFilter.FindName('FlateDecode') <> nil then
    with TCompressionStream.Create(clMax, TmpStream) do
    begin
      CopyFrom(FStream, 0);
      Free;
    end
  else
{$ENDIF}
    TmpStream.CopyFrom(FStream, 0);
  FLength.Value := TmpStream.Size;

  FAttributes.WriteToStream(AStream);
  _WriteString(#13#10'stream'#13#10, AStream);

  AStream.CopyFrom(TmpStream, 0);
  TmpStream.Free;

  _WriteString(#10'endstream', AStream);
end;

// CreateStream
constructor TPdfStream.CreateStream(AObjectMgr: TPdfObjectMgr);
begin
  inherited Create;
  FAttributes := TPdfDictionary.CreateDictionary(AObjectMgr);
  FAttributes.AddItem('Length', TPdfNumber.Create);
  FAttributes.AddItem('Filter', TPdfArray.CreateArray(AObjectMgr));
  FStream := TMemoryStream.Create;
end;

// Destroy
destructor TPdfStream.Destroy;
begin
  FStream.Free;
  FAttributes.Free;
  inherited;
end;

{ TPdfBinary }

// InternalWriteStream
procedure TPdfBinary.InternalWriteStream(const AStream: TStream);
begin
  AStream.CopyFrom(FStream, 0);
end;

// Create
constructor TPdfBinary.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
end;

// Destroy
destructor TPdfBinary.Destroy;
begin
  FStream.Free;
  inherited;
end;

{ utility functions }

// _DateTimeToPdfDate
function _DateTimeToPdfDate(ADate: TDateTime): TPdfDate;
begin
  result := FormatDateTime('"D:"yyyymmddhhnnss', now);
end;

// _PdfDateToDateTime
function _PdfDateToDateTime(AText: TPdfDate): TDateTime;
var
  yy, mm, dd, hh, nn, ss: Word;
begin
  if Length(AText) <> 16 then
    EConvertError.Create('');
  yy := StrToInt(Copy(AText, 3, 4));
  mm := StrToInt(Copy(AText, 7, 2));
  dd := StrToInt(Copy(AText, 9, 2));
  hh := StrToInt(Copy(AText, 11, 2));
  nn := StrToInt(Copy(AText, 13, 2));
  ss := StrToInt(Copy(AText, 15, 2));
  result := EncodeDate(yy, mm, dd) + EncodeTime(hh, nn, ss, 0);
end;

// _StrToUnicodeHex
function _StrToUnicodeHex(const Value: string): string;
var
  PW: Pointer;
  PByte: ^Byte;
  HiByte, LoByte: Byte;
  Len: integer;
  i: integer;
begin
  result := '';
  {$IFNDEF UNIX}
  Len := MultiByteToWideChar(0, CP_ACP,
    PChar(Value), Length(Value), nil, 0);
  GetMem(PW, Len * 2);
  Len := MultiByteToWideChar(0, CP_ACP,
    PChar(Value), Length(Value), PW, Len * 2);
  {$ELSE}
  Len := Length(Value);
  GetMem(PW, Len * 2);
  StringToWideChar(Value, PW, Len * 2);
  Len := Length(PWideChar(PW)^);
  {$ENDIF}
  PByte := Pw;
  i := 0;
  while i < Len do
  begin
    LoByte := PByte^;
    inc(PByte);
    HiByte := PByte^;
    inc(PByte);
    result := result + IntToHex(HiByte, 2) + IntToHex(LoByte, 2);
    inc(i);
  end;
  FreeMem(PW);
end;

// _StrToHex
function _StrToHex(const Value: string): string;
var
  i: integer;
begin
  // Return octal code for value.
  result := '';
  for i := 1 to Length(Value) do
    result := result + IntToHex(ord(Value[i]), 2);
end;

function _HasMultiByteString(const Value: string): boolean;
var
  i: integer;
begin
  result := false;
  for i := 1 to Length(Value) do
    if ByteType(Value, i) <> mbSingleByte then
    begin
      result := true;
      Break;
    end;
end;

//type
//  TConvFunc=function(const W:Word): char;

function CP1252(const W: Word; const InvalidChar: Char): Char;
begin
  case W of
    $00..$7F,$A0..$FF: result := char(W);
    $20AC: result := #$80;
    $201A: result := #$82;
    $0192: result := #$83;
    $201E: result := #$84;
    $2026: result := #$85;
    $2020: result := #$86;
    $2021: result := #$87;
    $02C6: result := #$88;
    $2030: result := #$89;
    $0160: result := #$8A;
    $2039: result := #$8B;
    $0152: result := #$8C;
    $017D: result := #$8E;
    $2018: result := #$91;
    $2019: result := #$92;
    $201C: result := #$93;
    $201D: result := #$94;
    $2022: result := #$95;
    $2013: result := #$96;
    $2014: result := #$97;
    $02DC: result := #$98;
    $2122: result := #$99;
    $0161: result := #$9A;
    $203A: result := #$9B;
    $0153: result := #$9C;
    $017E: result := #$9E;
    $0178: result := #$9F;
  else
    result:=InvalidChar;
  end;
end;

// _EscapeText
function _EscapeText(const Value: string): string;
const
  EscapeChars: string = '()\'#13#10#09#08#12;
  ReplaceChars: string = '()\rntbf';
var
  i, j: integer;
  flg: boolean;
  S: string;
begin
  //  If text contains chars to need escape, replace text using "\".
  //
  // TODO: implement UNICODE support in powerpdf. Currently we can't do
  // any better than converting utf-8 strings to unicode.
  result := '';
  {$IFDEF LAZ_POWERPDF}
  S := _UTF8ToWinAnsi(Value);
  {$ELSE}
  S := Value;
  {$ENDIF}
  for i := 1 to Length(S) do
  begin
    flg := false;
    for j := 1 to Length(EscapeChars) do
      if S[i] = EscapeChars[j] then
      begin
        result := result + '\' + ReplaceChars[j];
        flg := true;
        break;
      end;

    if not flg then
       result := result + S[i];
  end;
end;

// _GetTypeOf
function _GetTypeOf(ADictionary: TPdfDictionary): string;
var
  PdfName: TPdfName;
begin
  // return the type of the pdfdictionary object
  PdfName := ADictionary.PdfNameByName('Type');
  if PdfName <> nil then
    result := PdfName.Value
  else
    result := '';
end;

// _WriteString
procedure _WriteString(const Value: string; AStream: TStream);
begin
  AStream.Write(PChar(Value)^, Length(Value));
end;

// _FloatToStrR
function _FloatToStrR(Value: Extended): string;
var
  i: integer;
begin
  result := FloatToStr(Trunc(Value * 100 + 0.5) / 100);
  // Convert ','(Comma) to '.'(period)
  // -- for the area whose decimal parametor is ','
  if DecimalSeparator <> '.' then
  begin
    i := Pos(DecimalSeparator, result);
    if i > 0 then
      result[i] := '.';
  end;
end;

// _GetUnicodeHeader
function _GetUnicodeHeader: string;
begin
  result := Format(PDF_UNICODE_HEADER, [_StrToHex(PDF_LANG_STRING)]);
end;

// _PdfRect
function _PdfRect(Left, Top, Right, Bottom: Single): TPdfRect;
begin
  result.Left := Left;
  result.Top := Top;
  result.Right := Right;
  result.Bottom := Bottom;
end;

// _GetCharCount
function _GetCharCount(const Text: string): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to Length(Text) do
    if (ByteType(Text, i) = mbSingleByte) or
      (ByteType(Text, i) = mbLeadByte) then
      inc(result);
end;

{$IFDEF LAZ_POWERPDF}

function _UTF8StrToUnicodeHex(const Value: string): string;
var
  W: Widestring;
  i: Integer;
begin
  result := '';
  W := UTF8Decode(Value);
  for i:=1 to Length(W) do begin
    Result := Result + IntTohex(Word(W[i]), 4);
  end;
end;

function _UTF8ToWinAnsi(const value: string; InvalidChar:char='?'): string;
var
  W: widestring;
  i: Integer;
begin
  W := UTF8Decode(Value);
  result := '';
  for i:=1 to length(w) do
    result := result + CP1252(word(w[i]), InvalidChar);
end;

procedure PdfLazRegisterClassAlias(aClass: TPersistentClass; const Alias: string);
begin
  Classes.RegisterClass(aClass);
  if AliasList=nil then
    AliasList := TStringList.Create;
  AliasList.AddObject(Alias, TObject(aClass));
end;

function PdfLazFindClass(const aClassName: String): TPersistentClass;
var
  i: Integer;
begin
  result := Classes.GetClass(aClassName);
  if Result=nil then begin
    i := AliasList.IndexOf(aClassName);
    if i>=0 then
      Result := TPersistentClass(AliasList.Objects[i]);
  end;
  if not Assigned(Result) then
    raise EClassNotFound.CreateFmt('No class was found', [aClassName]);
end;

function _GetSpcCount(const Text: string): Integer;
var
  i: Integer;
  W: widestring;
begin
  result := 0;
  W := UTF8Decode(Text);
  for i:=1 to Length(W) do begin
    if W[i]=' ' then
      inc(result);
  end;
end;

initialization
  AliasList := nil;

finalization
  if AliasList<>nil then
    AliasList.Free;
  AliasList:=nil;
{$ENDIF}


end.

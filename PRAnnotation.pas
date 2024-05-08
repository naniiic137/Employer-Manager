{*
 * << P o w e r P d f >> -- PRAnnotation.pas
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
 * 2001.07.07 Create
 * 2001.08.12 Changed the implementation of annotation.
 * 2020.10.23 Added annotation type: Text or link annotation. (<stefan[at]smartsoftware[dot]com>)
 *
 *}
{$IFDEF LAZ_POWERPDF}
{$H+}
{$ENDIF}
unit PRAnnotation;

interface

uses
  {$ifdef LAZ_POWERPDF}
  LCLIntf, LCLType, LMessages,
  {$ELSE}
  Windows, Messages,
  {$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PReport, PdfDoc, PdfFonts, PdfTypes;

type
  TPRAnnotationType = TPdfAnnotationSubType;

  TPRAnnotation = class(TPRItem)
  private
    FSubType: TPRAnnotationType;
    FLines: TStrings;
    FOpened: boolean;
    FAction: TPRAction;
    procedure SetLines(Value: TStrings);
    procedure SetText(Value: string);
    function GetText: string;
    function GetLines: TStrings;
  protected
    {$IFDEF LAZ_POWERPDF}
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    {$ELSE}
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    {$ENDIF}
    procedure Paint; override;
    procedure Print(ACanvas: TPRCanvas; ARect: TRect); override;
    procedure SetAction(aValue: TPRAction); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Text: string read GetText write SetText;
  published
    property SubType: TPRAnnotationType read FSubType write FSubType default asTextNotes;
    property Caption;
    property Lines: TStrings read GetLines write SetLines;
    property Opened: boolean read FOpened write FOpened;
    property Action: TPRAction read FAction write SetAction;
  end;

implementation

{ TPRAnnotation }

// SetLines
procedure TPRAnnotation.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
  Invalidate;
end;

// GetLines
function TPRAnnotation.GetLines: TStrings;
begin
  result := FLines;
end;

// SetText
procedure TPRAnnotation.SetText(Value: string);
begin
  FLines.Text := Value;
end;

// GetText
function TPRAnnotation.GetText: string;
begin
  result := Trim(FLines.Text);
end;

// Create
constructor TPRAnnotation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLines := TStringList.Create;
  FAction := TPRAction.Create;
end;

// Destroy
destructor TPRAnnotation.Destroy;
begin
  FLines.Free;
  FAction.Free;
  inherited;
end;

// CMTextChanged
{$IFDEF LAZ_POWERPDF}
procedure TPRAnnotation.CMTextChanged(var Message: TLMessage);
{$ELSE}
procedure TPRAnnotation.CMTextChanged(var Message: TMessage);
{$ENDIF}
begin
  Invalidate;
end;

procedure TPRAnnotation.Paint;
var
  W: Integer;
  tmpRect: TRect;
  s: String;
const
  PDF_ANNOT_TITLE_HEIGHT = 15;
begin
  with Canvas do
  begin
    if SubType = asTextNotes then
      s := Text
    else
      s := Action.URI;
    tmpRect := GetClientRect;
    tmpRect.Top := PDF_ANNOT_TITLE_HEIGHT;
    InflateRect(tmpRect, -5, -1);
    tmpRect.Right := tmpRect.Right - 24;
    Brush.Color := clWindow;
    Font.Size := 10;
    Font.Style := [];
    Rectangle(0, PDF_ANNOT_TITLE_HEIGHT, self.Width-1, self.Height-1);
    DrawText(Handle, PChar(Text), -1, TmpRect, DT_WORDBREAK);

    Brush.Color := clYellow;
    Rectangle(0, 0, self.Width-1, PDF_ANNOT_TITLE_HEIGHT + 1);
    Font.Size := 8;
    Font.Style := [fsBold];
    W := TextWidth(Caption);
    TextOut((self.Width - W) div 2, 4, Caption);
  end;
end;

procedure TPRAnnotation.Print(ACanvas: TPRCanvas; ARect: TRect);
var
  FAnnotation: TPdfDictionary;
  S: string;
  APos: integer;
  NewRect: TRect;
begin
  // omitting LF charactors from CRLF sequence.
  if SubType = asTextNotes then begin
    S := Text;
    APos := pos(#13#10, S);
    while APos > 0 do
    begin
      S := Copy(S, 1, APos) + Copy(S, APos+2, Length(S) - APos-2);
      APos := pos(#13#10, S);
    end;
  end;

  // creating annotation object and setting properties.
  with NewRect do
  begin
    Top := Page.ClientHeight - ARect.Bottom;
    Bottom := Page.ClientHeight - ARect.Top;
    Left := ARect.Left;
    Right := ARect.Right;
  end;
  with NewRect do
    FAnnotation := ACanvas.PdfCanvas.Doc.CreateAnnotation(SubType,
                                            _PdfRect(Left, Top, Right, Bottom));
  if SubType = asTextNotes then begin
    // Subtype Text
    FAnnotation.AddItem('Contents', TPdfText.CreateText(S));
    // Title key is "T", not "S"
    //FAnnotation.AddItem('S', TPdfText.CreateText(Caption));
    FAnnotation.AddItem('T', TPdfText.CreateText(Caption));
    if Opened then
      FAnnotation.AddItem('Open', TPdfBoolean.CreateBoolean(true));
  end else begin
    // Subtype Link
    FAnnotation.AddItem('A', Action.GetPdfObj(ACanvas.PdfCanvas.Doc));
    FAnnotation.AddItem('Border', TPdfArray.CreateNumArray(nil, [0, 0, 0]));
  end;
end;

procedure TPRAnnotation.SetAction(aValue: TPRAction);
begin
  if not FAction.IsEqual(aValue) then
    FAction.Assign(aValue);
end;

end.

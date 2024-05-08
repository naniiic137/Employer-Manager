{*
 * << P o w e r P d f >> -- PdfImages.pas
 * << Standerd image classes defination >>
 *
 * Copyright (c) 1999-2001 T.KANNO. <takeshi_kanno@est.hi-ho.ne.jp>
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
 * 2001.03.14 Create.
 * 2001.07.30 Implemented Indexed color image.
 * 2001.08.26 changed some definations and methods to work with kylix.
 * 2001.09.01 changed the implementation of the image.
 *
 *}
{$IFDEF FPC}
{$MODE OBJFPC}{$H+}
{$ENDIF}

unit PdfImages;

interface

{$IFDEF UNIX}
  {$IFNDEF FPC}
  {$DEFINE USE_CLX}
  {$ENDIF}
{$ENDIF}

uses
  {$IFDEF LAZ_POWERPDF}
  SysUtils, LCLType, LCLIntf, Graphics, FPImage, IntfGraphics, GraphType, PdFImageLazTools,
  {$ELSE}
  {$IFNDEF USE_CLX}
  Windows, SysUtils, Graphics,
  {$ELSE}
  SysUtils, QGraphics, Qt,
  {$ENDIF}
  {$ENDIF}
  Classes, PdfTypes, PdfDoc;

type
  TPdfImageCreator = class(TPersistent)
  public
    function CreateImage(AImage: TGraphic; ObjectMgr: TPdfObjectMgr=nil): TPdfImage; virtual;
  end;

  { TPdfBitmapImage }

  TPdfBitmapImage = class(TPdfImageCreator)
  private
    {$IFNDEF LAZ_POWERPDF}
    function CreateIndexedColorArray(ABitmap: TBitmap): TPdfArray;
    {$ENDIF}
  public
    function CreateImage(AImage: TGraphic; ObjectMgr: TPdfObjectMgr=nil): TPdfImage; override;
  end;

  function CreatePdfImage(AImage: TGraphic; ImageClassName: string; ObjectMgr: TPdfObjectMgr=nil): TPdfImage;

implementation

function CreatePdfImage(AImage: TGraphic; ImageClassName: string; ObjectMgr: TPdfObjectMgr=nil): TPdfImage;
var
  PdfImageCreator: TPdfImageCreator;
begin
  Result := nil;
  {$IFDEF LAZ_POWERPDF}
  PdfImageCreator := TPdfImageCreator(PdfLazFindClass(ImageClassName).Create);
  {$ELSE}
  PdfImageCreator := TPdfImageCreator(FindClass(ImageClassName).Create);
  {$ENDIF}
  try
    if PdfImageCreator = nil then
      raise Exception.CreateFmt('AddImage --InvalidImageClassName:%s', [ImageClassName]);
    Result := PdfImageCreator.CreateImage(AImage, ObjectMgr);
  finally
    PdfImageCreator.Free;
  end;
end;

{ TPdfImageCreator }
function TPdfImageCreator.CreateImage(AImage: TGraphic; ObjectMgr: TPdfObjectMgr=nil): TPdfImage;
begin
  result := nil;
end;

{$IFDEF USE_CLX}
type
  TColorTable = array[0..MaxInt div SizeOf(QRgb)-1] of QRgb;
  PColorTable = ^TColorTable;
{$ENDIF}

{$IFDEF LAZ_POWERPDF}
function TPdfBitmapImage.CreateImage(AImage: TGraphic; ObjectMgr: TPdfObjectMgr=nil): TPdfImage;
var
  fpImg: TFPCustomImage;
  Alpha: TFPMemoryImage;
  x, y, z: integer;
  pb: PByteArray;
  b: Byte;
  aColor    : TFPColor;
  HasAlpha: Boolean;
  MaskImage: TPdfImage;

  procedure AddItems;
  begin
    with Result.Attributes do
    begin
      AddItem('Width', TPdfNumber.CreateNumber(aImage.Width));
      AddItem('Height', TPdfNumber.CreateNumber(aImage.Height));
      AddItem('BitsPerComponent', TPdfNumber.CreateNumber(8));
      if USE_ZLIB then
        PdfArrayByName('Filter').AddItem(TPdfName.CreateName('FlateDecode'));
    end;
  end;

begin

  result := TPdfImage.CreateStream(nil);
  with result do
  try

    with Attributes do
    begin
      AddItem('Type', TPdfName.CreateName('XObject'));
      AddItem('Subtype', TPdfName.CreateName('Image'));
    end;

    // Convert a LCL TGraphic into a TFPCustomImage
    ConvertGraphicToFPImage(AImage, fpImg);

    if fpImg.UsePalette and (fpImg.Palette.Count>0) then
    begin
      for y := 0 to fpImg.Height - 1 do
      begin
        new(pb);
        for x := 0 to fpImg.Width-1 do
          pb^[x] := fpImg.Pixels[x,y];
        Stream.Write(pb^, fpImg.Width);
        dispose(pb);
      end;
      Attributes.AddItem('ColorSpace', CreateIndexedColorArray(fpImg));
    end else
    begin

      Alpha := CreateAlphaImage(AImage.Width, AImage.Height);
      hasAlpha := false;

      for y := 0 to fpImg.Height - 1 do
      begin
        new(pb);
        for x := 0 to fpImg.Width-1 do
        begin
          aColor := fpImg.Colors[x,y];
          z:=1;
          pb^[ 0 ] := acolor.red shr 8;
          pb^[ z ] := acolor.green shr 8;
          pb^[ z+1 ] := acolor.blue shr 8;
          Stream.write(pb[ 0 ], 3);

          b := acolor.alpha shr 8;
          Alpha.Pixels[x,y] := b;

          if acolor.Alpha<>AlphaOpaque then
            HasAlpha := true;
        end;
        dispose(pb);
      end;

      if HasAlpha then begin
        MaskImage := CreateMaskStream(Alpha);
        if ObjectMgr<>nil then
          ObjectMgr.AddObject(MaskImage);
        Attributes.AddItem('SMask', MaskImage);
      end;

      Alpha.Free;

      Attributes.AddItem('ColorSpace', TPdfName.CreateName('DeviceRGB'));
    end;
    AddItems;

    fpImg.Free;

  except
    result.free;
    raise;
  end;
end;

{$ELSE}

function TPdfBitmapImage.CreateIndexedColorArray(ABitmap: TBitmap): TPdfArray;
var
  {$IFNDEF USE_CLX}
  PalEntries: array[0..255] of TPaletteEntry;
  {$ELSE}
  PalEntries: PColorTable;
  CRgb: Cardinal;
  pb: PByteArray;
  {$ENDIF}
  i: integer;
  ColorTable: TPdfBinary;
  NumOfColors: integer;
  S: string;
begin
  // creating color table from palette of bitmap.

  if ABitmap.PixelFormat <> pf8bit then
    raise EPdfInvalidImageFormat.Create('only 8 bit color image is allowed.');

  NumOfColors := 256;
  {$IFNDEF USE_CLX}
  if GetPaletteEntries(ABitmap.Palette, 0, NumOfColors + 1, PalEntries) = 0 then
    raise EPdfInvalidImageFormat.Create('failed to get Palette..');
  {$ELSE}
  PalEntries := PColorTable(ABitmap.ColorTable);
  {$ENDIF}
  ColorTable := TPdfBinary.Create;
  S := '<';

  {$IFNDEF USE_CLX}
  for i := 0 to NumOfColors - 1 do
    with PalEntries[i] do
      S := S + IntToHex(peRed, 2) +
           IntToHex(peGreen, 2) +
           IntToHex(peBlue, 2) +
           ' ';
  {$ELSE}
  for i := 0 to NumOfColors - 1 do
  begin
    CRgb := PalEntries[i];
    pb := PByteArray(@CRgb);
    S := S + IntToHex(pb[2], 2) +
         IntToHex(pb[1], 2) +
         IntToHex(pb[0], 2) +
           ' ';
  end;
  {$ENDIF}

  S := S + '>';
  ColorTable.Stream.Write(PChar(S)^, Length(S));

  result := TPdfArray.CreateArray(nil);
  with result do
  begin
    AddItem(TPdfName.CreateName('Indexed'));
    AddItem(TPdfName.CreateName('DeviceRGB'));
    AddItem(TPdfNumber.CreateNumber(NumOfColors - 1));
    AddItem(ColorTable);
  end;
end;

function TPdfBitmapImage.CreateImage(AImage: TGraphic; ObjectMgr: TPdfObjectMgr=nil): TPdfImage;
var
  ABitmap: TBitmap;
  x, y: integer;
  pb: PByteArray;
  b: Byte;
{$IFDEF USE_CLX}
const
  PIXEL_COLOR_SIZE = 4;
{$ENDIF}
begin

  result := TPdfImage.CreateStream(nil);
  with result do
  try
    with Attributes do
    begin
      AddItem('Type', TPdfName.CreateName('XObject'));
      AddItem('Subtype', TPdfName.CreateName('Image'));
    end;

    ABitmap := TBitmap.Create;
    
    with ABitmap do
    try
      Assign(AImage);

      // if bitmap image has less then 8 bit color, set PixelFormat to 8 bit.
      if (PixelFormat = pf1bit) or
         {$IFNDEF USE_CLX}
         (PixelFormat = pf4bit) or
         {$ENDIF}
         (PixelFormat = pf8bit) then
        PixelFormat := pf8bit
      else
        {$IFNDEF USE_CLX}
        PixelFormat := pf24Bit;
        {$ELSE}
        PixelFormat := pf32Bit;
        {$ENDIF}

      // translate TBitmap object to pdf image format.
      if PixelFormat = pf8bit then
      begin
        for y := 0 to Height - 1 do
        begin
          pb := ScanLine[y];
          Stream.Write(pb^, Width);
          end;
        Attributes.AddItem('ColorSpace', CreateIndexedColorArray(ABitmap));
      end
      else
      begin
        for y := 0 to Height - 1 do
        begin
          pb := ScanLine[y];
          x := 0;
          while x < Width * PIXEL_COLOR_SIZE - 1 do
          begin
            b := pb[x];
            pb[x] := pb[x+2];
            pb[x+2] := b;
            Stream.Write(pb[x], 3);
            x := x + PIXEL_COLOR_SIZE;
          end;
          Attributes.AddItem('ColorSpace', TPdfName.CreateName('DeviceRGB'));
        end;
        end;

      with Attributes do
      begin
        AddItem('Width', TPdfNumber.CreateNumber(abitmap.Width));
        AddItem('Height', TPdfNumber.CreateNumber(abitmap.Height));
        AddItem('BitsPerComponent', TPdfNumber.CreateNumber(8));
        if USE_ZLIB then
          PdfArrayByName('Filter').AddItem(TPdfName.CreateName('FlateDecode'));
      end;
    finally
      Free;
    end;
  except
    result.Free;
    raise;
  end;
end;
{$ENDIF}

initialization
  {$IFDEF LAZ_POWERPDF}
  PdfLazRegisterClassAlias(TPdfBitmapImage, 'Pdf-Bitmap');
  {$ELSE}
  RegisterClassAlias(TPdfBitmapImage, 'Pdf-Bitmap');
  {$ENDIF}

finalization
  UnRegisterClass(TPdfBitmapImage);

end.


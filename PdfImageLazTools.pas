unit PdfImageLazTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FPImage,
  FPReadBMP, FPReadPNG, FPReadJPEG, // make sure we have the basic readers
  Graphics, PDFTypes;

  function CreateAlphaImage(const aWidth, aHeight: Integer): TFPMemoryImage;
  procedure ConvertGraphicToFPImage(AImage: TGraphic; out fpImg: TFPCustomImage);
  function CreateMaskStream(AImage: TFPCustomImage): TPDfImage;
  function CreateIndexedColorArray(ABitmap: TFPCustomImage): TPdfArray;

implementation

type

  { TRasterImageHelper }

  TRasterImageHelper = class helper for TRasterImage
  public
    function RequestRawStream(out rawStream: TMemoryStream): boolean;
  end;

function CreateAlphaImage(const aWidth, aHeight: Integer): TFPMemoryImage;
var
  aColor: TFPColor;
  x: Integer;
begin
  result := TFPMemoryImage.Create(aWidth, aHeight);
  result.UsePalette := true;
  result.Palette.Count := 256;
  for x:=0 to $FF do
  begin
    aColor.Red:=x;
    aColor.Red:=(aColor.Red shl 8) + aColor.Red;
    aColor.Green:=aColor.Red;
    aColor.Blue:=aColor.Red;
    result.Palette.Color[x]:=aColor;
  end;
end;

procedure ConvertGraphicToFPImage(AImage: TGraphic; out fpImg: TFPCustomImage);
var
  rawImgStream: TMemoryStream = nil;
  useOriginalStream: boolean = false;
begin
  if (AImage is TRasterImage) then
    useOriginalStream := TRasterImage(AImage).RequestRawStream(rawImgStream);

  if not useOriginalStream then begin
    rawImgStream := TMemoryStream.Create;
    AImage.SaveToStream(rawImgStream);
    rawImgStream.Position := 0;
  end;

  try
    fpImg := TFPMemoryImage.Create(0, 0);
    fpImg.UsePalette := false;
    try
      fpImg.LoadFromStream(rawImgStream);
    except
      fpImg.Free;
      fpImg := nil;
      raise
    end;
  finally
    if not useOriginalStream then
      rawImgStream.Free;
  end;

end;

function CreateMaskStream(AImage: TFPCustomImage): TPDfImage;
var
  pb: PByteArray;
  y: Integer;
  x: Integer;
begin
  result := TPdfImage.CreateStream(nil);
  with result do
  try
    with Attributes do
    begin
      AddItem('Type', TPdfName.CreateName('XObject'));
      AddItem('Subtype', TPdfName.CreateName('Image'));
      AddItem('Width', TPdfNumber.CreateNumber(aImage.Width));
      AddItem('Height', TPdfNumber.CreateNumber(aImage.Height));
      AddItem('BitsPerComponent', TPdfNumber.CreateNumber(8));
      AddItem('ColorSpace',TPdfName.CreateName('DeviceGray'));
      if USE_ZLIB then
        PdfArrayByName('Filter').AddItem(TPdfName.CreateName('FlateDecode'));

      new(pb);
      for y := 0 to AImage.Height - 1 do
      begin
        for x := 0 to AImage.Width-1 do
          pb^[x] := AImage.Pixels[x,y];
        Stream.Write(pb^, AImage.Width);
      end;
      dispose(pb);
    end;

  finally
  end;
end;

function CreateIndexedColorArray(ABitmap: TFPCustomImage): TPdfArray;
var
  i: integer;
  ColorTable: TPdfBinary;
  NumOfColors: integer;
  S: string;

  procedure AddColor(Red,Green,Blue:byte);
  begin
    S := S + IntToHex(Red, 2) +
         IntToHex(Green, 2) +
         IntToHex(Blue, 2) +
         ' ';
  end;

begin
  // creating color table from palette of bitmap.
  if (not ABitmap.UsePalette) then
    raise EPdfInvalidImageFormat.Create('Expected indexed color image');

  NumOfColors := ABitmap.Palette.Count;

  // get/check palette entries
  if ABitmap.Palette.Count=0 then
    raise EPdfInvalidImageFormat.Create('failed to get Palette..');

  ColorTable := TPdfBinary.Create;
  S := '<';

  for i := 0 to NumOfColors - 1 do
    if i<ABitmap.Palette.Count then
      with aBitmap.palette.color[i] do
        AddColor(Red,Green,Blue)
    else
      AddColor(0,0,0);

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

{ TRasterImageHelper }

function TRasterImageHelper.RequestRawStream(out rawStream: TMemoryStream
  ): boolean;
begin
  // make direct use of the saved original stream to avoid re-copying
  // this should be ok as it is very unlikely to change in the future.
  rawStream := FSharedImage.SaveStream;
  result := rawStream<>nil;
  if result then
    rawStream.Position := 0;
end;

end.


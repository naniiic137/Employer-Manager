{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pack_powerpdf;

{$warn 5023 off : no warning about unused units}
interface

uses
  PdfTypes, PdfDoc, PdfJpCMap, PdfJPFonts, PdfGBFonts, PdfFonts, PdfImages, 
  PReport, PdfJpegImage, PRJpegImage, PRAnnotation, PowerPdf, 
  PdfImageLazTools, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PowerPdf', @PowerPdf.Register);
end;

initialization
  RegisterPackage('pack_powerpdf', @Register);
end.

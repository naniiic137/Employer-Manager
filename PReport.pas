{*
 * << P o w e r P d f >> -- PReport.pas
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
 * 2001.01.28 create
 * 2001.06.24 added strech property to TPRImage.
 * 2001.06.30 added chinese font(Experimental).
 *            fixed TPRImage bug.
 *            move TPRText.GetInternalDoc method to TPRItem.
 * 2001.07.20 fixed font setting bugs.
 * 2001.07.25 changed TPRPage text width routines.
 * 2001.08.01 added TPReport.PageLayout.
 * 2001.08.08 changed the algorithm of the free XObject name.
 * 2001.08.10 changed the text width routine(bugs when large font size).
 * 2001.08.15 added TPROutline and TPRDestination.
 * 2001.09.01 changed the implementation of the image.
 * 2001.09.08 added OpenAction function.
 *            added AlignJustified property to TPRLabel.
 * 2001.09.13 added ViewerPreference functions.
 *            added check functions to TPReport.
 * 2020.10.23 added TPRAction class. (<stefan[at]smartsoftware[dot]com>)
 *
 *}
unit PReport;

interface

{$IFDEF LAZ_POWERPDF}
{$mode objfpc}{$H+}
{$ENDIF}

//{$DEFINE USE_JPFONTS}
//{$DEFINE USE_GBFONTS}

uses
  {$IFDEF LAZ_POWERPDF}
  LCLType, LMessages, LCLIntf, GraphType, FPCanvas, LazUTF8, LCLProc,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, GraphMath, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, PdfDoc, PdfFonts, PdfTypes, PdfImages
  {$IFDEF USE_JPFONTS}
  , PdfJPFonts
  {$ENDIF}
  {$IFDEF USE_GBFONTS}
  , PdfGBFonts
  {$ENDIF}
  ;

const
  POWER_PDF_VERSION_STR = POWER_PDF_VERSION_TEXT;
  POWER_PDF_COPYRIGHT = 'copyright (c) 1999-2001 takeshi kanno';

  {$IFDEF LAZ_POWERPDF}
const
  psInsideFrame = FPcanvas.psInsideFrame;
  {$ENDIF}

type
  TPRFontName = (fnFixedWidth
               , fnArial
               , fnTimesRoman
               {$IFDEF USE_JPFONTS}
               , fnGothic
               , fnMincyo
               , fnPGothic
               , fnPMincyo
               {$ELSE}
               {$IFDEF USE_GBFONTS}
               , fnChinese
               {$ENDIF}
               {$ENDIF}
               );
  TPRPage = class;
  TPRCanvas = class;
  TPRPanel = class;
  TPRItem = class;
  TPROutlineEntry = class;
  TPRDestination = class;
  TPRAction = class;
  TPROutlineRoot = class;

  TPRPrintPageEvent = procedure(Sender: TObject;
                              ACanvas: TPRCanvas) of object;
  TPRPrintPanelEvent = procedure(Sender: TObject; ACanvas: TPRCanvas;
                              Rect: TRect) of object;
  TPRPrintItemEvent = TPRPrintPanelEvent;
  TPRPrintChildPanelEvent = procedure(Sender: TObject; ACanvas: TPRCanvas;
                              ACol, ARow: integer; Rect: TRect) of object;
  TPrintDirection = (pdHorz, pdVert);
  TPRActionType = TPdfActionType;
  TPRDestinationType = TPdfDestinationType;
  TPRPageLayout = TPdfPageLayout;
  TPRPageMode = TPdfPageMode;
  TPRCompressionMethod = TPdfCompressionMethod;
  TPRViewerPreference = TPdfViewerPreference;
  TPRViewerPreferences = TPdfViewerPreferences;
  TPRPoint = record
    x,y: single;
  end;
  TPRPointArray = array of TPRPoint;

  { TPReport }
  TPReport = class(TAbstractPReport)
  private
    FFileName: string;
    FPage: integer;
    FAuthor: string;
    FCreationDate: TDateTime;
    FCreator: string;
    FKeywords: string;
    FModDate: TDateTime;
    FSubject: string;
    FTitle: string;
    FCanvas: TPRCanvas;
    FDoc: TPdfDoc;
    FPageMode: TPRPageMode;
    FNonFullScreenPageMode: TPRPageMode;
    FPageLayout: TPRPageLayout;
    FCompressionMethod: TPRCompressionMethod;
    FUseOutlines: boolean;
    FOutlineRoot: TPROutlineRoot;
    FOpenAction: TPRDestination;
    FViewerPreference: TPRViewerPreferences;
    procedure SetOpenAction(ADest: TPRDestination);
    procedure SetAuthor(Value: string);
    procedure SetCreationDate(Value: TDateTime);
    procedure SetCreator(Value: string);
    procedure SetKeyWords(Value: string);
    procedure SetModDate(Value: TDateTime);
    procedure SetSubject(Value: string);
    procedure SetTitle(Value: string);
    procedure SetPageLayout(Value: TPRPageLayout);
    procedure SetPageMode(Value: TPRPageMode);
    procedure SetNonFullScreenPageMode(Value: TPRPageMode);
    procedure SetUseOutlines(Value: boolean);
    procedure SetViewerPreference(Value: TPRViewerPreferences);
    function GetOpenAction: TPRDestination;
    function GetOutlineRoot: TPROutlineRoot;
  protected
    { Protected }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginDoc;
    procedure Print(APage: TPRPage);
    procedure EndDoc;
    procedure Abort;
    function CreateDestination: TPRDestination;
    function GetPdfDoc: TPdfDoc;
    property PageNumber: integer read FPage;
    property OutlineRoot: TPROutlineRoot read GetOutlineRoot;
    property OpenAction: TPRDestination read GetOpenAction write SetOpenAction;
  published
    property FileName: string read FFileName write FFileName;
    property Author: string read FAuthor write SetAuthor;
    property CreationDate: TDateTime read FCreationDate write SetCreationDate;
    property Creator: string read FCreator write SetCreator;
    property Keywords: string read FKeyWords write SetKeyWords;
    property ModDate: TDateTime read FModDate write SetModDate;
    property Subject: string read FSubject write SetSubject;
    property Title: string read FTitle write SetTitle;
    property PageLayout: TPRPageLayout read FPageLayout
                           write SetPageLayout default plSinglePage;
    property PageMode: TPRPageMode read FPageMode
                           write SetPageMode default pmUseNone;
    property NonFullScreenPageMode: TPRPageMode read FNonFullScreenPageMode
                           write SetNonFullScreenPageMode default pmUseNone;
    property CompressionMethod: TPRCompressionMethod
       read FCompressionMethod write FCompressionMethod default cmNone;
    property UseOutlines: boolean read FUseOutlines write SetUseOutlines;
    property ViewerPreference: TPRViewerPreferences
                           read FViewerPreference write SetViewerPreference;
  end;

  { TPRCanvas }
  TPRCanvas = class(TPersistent)
  private
    FCanvas: TPdfCanvas;
    procedure SetPdfCanvas(ACanvas: TPdfCanvas);
    function GetPageHeight: integer;
    function GetPageWidth: integer;
  protected
  public
    constructor Create;
    function TextWidth(Text: string): Single;
    procedure SetCharSpace(charSpace: Single);
    procedure SetWordSpace(wordSpace: Single);
    procedure SetHorizontalScaling(hScaling: Word);
    procedure SetLeading(leading: Single);
    procedure SetFont(fontname: string; size: Single);
    procedure SetTextRenderingMode(mode: TTextRenderingMode);
    procedure SetTextRise(rise: Word);
    procedure TextOut(X, Y: Single; Text: string);
    procedure TextRect(ARect: TRect; Text: string;
                            Alignment: TAlignment; Clipping: boolean);
    property PdfCanvas: TPdfCanvas read FCanvas write SetPdfCanvas;
    property PageHeight: integer read GetPageHeight;
    property PageWidth: integer read GetPageWidth;
  end;

  { TPRPage }
  TPRPage = class(TCustompanel)
  private
    FDoc: TPdfDoc;
    FMarginTop: integer;
    FMarginLeft: integer;
    FMarginRight: integer;
    FMarginBottom: integer;
    FPrintPageEvent: TPRPrintPageEvent;
    procedure SetMarginTop(Value: integer);
    procedure SetMarginLeft(Value: integer);
    procedure SetMarginRight(Value: integer);
    procedure SetMarginBottom(Value: integer);
  protected
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure Paint; override;
    procedure Print(ACanvas: TPRCanvas);
    property InternalDoc: TPdfDoc read FDoc;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnPrintPage: TPRPrintPageEvent
                     read FPrintPageEvent write FPrintPageEvent;
    property MarginTop: integer read FMarginTop write SetMarginTop;
    property MarginLeft: integer read FMarginLeft write SetMarginLeft;
    property MarginRight: integer read FMarginRight write SetMarginRight;
    property MarginBottom: integer read FMarginBottom write SetMarginBottom;
    property Visible;
  end;

  { TPRPanel }
  TPRPanel = class(TCustomPanel)
  private
    function GetPage: TPRPage;
    function GetAbsoluteRect: TRect;
  protected
    procedure Paint; override;
    procedure Print(ACanvas: TPRCanvas; ARect: TRect); virtual;
  public
    property Page: TPRPage read GetPage;
    constructor Create(AOwner: TComponent); override;
  end;

  { TPRChildPanel }
  TPRChildPanel = class(TPRPanel)
  private
  protected
  end;

  { TPRLayoutPanel }
  TPRLayoutPanel = class(TPRPanel)
  private
    FAfterPrint: TPRPrintPanelEvent;
    FBeforePrint: TPRPrintPanelEvent;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Print(ACanvas: TPRCanvas; ARect: TRect); override;
  published
    property Align;
    property BeforePrint: TPRPrintPanelEvent
                                read FBeforePrint write FBeforePrint;
    property AfterPrint: TPRPrintPanelEvent
                                read FAfterPrint write FAfterPrint;
  end;

  { TRRGridPanel }
  TPRGridPanel = class(TPRPanel)
  private
    FAfterPrint: TPRPrintPanelEvent;
    FBeforePrint: TPRPrintPanelEvent;
    FBeforePrintChild: TPRPrintChildPanelEvent;
    FAfterPrintChild: TPRPrintChildPanelEvent;
    FColCount: integer;
    FRowCount: integer;
    FChildPanel: TPRChildPanel;
    FPrintDirection: TPrintDirection;
    procedure SetColCount(Value: integer);
    procedure SetRowCount(Value: integer);
  protected
    procedure Print(ACanvas: TPRCanvas; ARect: TRect); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
    function GetChildParent: TComponent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColCount: integer read FColCount write SetColCount;
    property RowCount: integer read FRowCount write SetRowCount;
    property Align;
    property PrintDirection: TPrintDirection
                        read FPrintDirection write FPrintDirection default pdHorz;
    property BeforePrint: TPRPrintPanelEvent
                        read FBeforePrint write FBeforePrint;
    property AfterPrint: TPRPrintPanelEvent
                        read FAfterPrint write FAfterPrint;
    property BeforePrintChild: TPRPrintChildPanelEvent
                        read FBeforePrintChild write FBeforePrintChild;
    property AfterPrintChild: TPRPrintChildPanelEvent
                        read FAfterPrintChild write FAfterPrintChild;
  end;

  { TPRItem }
  TPRItem = class(TGraphicControl)
  private
    FPrintable: boolean;
    function GetPage: TPRPage;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Print(ACanvas: TPRCanvas; ARect: TRect); virtual;
    function GetInternalDoc: TPdfDoc;
    property Page: TPRPage read GetPage;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Printable: boolean read FPrintable write FPrintable default true;
  end;

  { TPRCustomLabel }
  TPRCustomLabel = class(TPRItem)
  private
    FFontColor: TColor;
    FFontName: TPRFontName;
    FFontSize: Single;
    FFontBold: boolean;
    FFontItalic: boolean;
    FCharSpace: Single;
    FFontUnderLine: boolean;
    FWordSpace: Single;
    procedure SetCharSpace(Value: Single);
    procedure SetFontUnderline(const Value: boolean);
    procedure SetWordSpace(Value: Single);
    procedure SetFontColor(Value: TColor);
    function GetFontClassName: string;
    procedure SetFontName(Value: TPRFontName);
    procedure SetFontItalic(Value: boolean);
    procedure SetFontBold(Value: boolean);
    procedure SetFontSize(Value: Single);
  protected
    function InternalTextout(APdfCanvas: TPdfCanvas;
                        S: string; X, Y: integer): Single;
    {$IFDEF LAZ_POWERPDF}
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    {$ELSE}
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FontColor: TColor read FFontColor write SetFontColor default clBlack;
    property FontName: TPRFontName read FFontName write SetFontName;
    property FontSize: Single read FFontSize write SetFontSize;
    property FontBold: boolean read FFontBold write SetFontBold default false;
    property FontItalic: boolean read FFontItalic write SetFontItalic default false;
    property FontUnderline: boolean read FFontUnderLine write SetFontUnderline default false;
    property CharSpace: Single read FCharSpace write SetCharSpace;
    property WordSpace: Single read FWordSpace write SetWordSpace;
  end;

  { TPRLabel }
  TPRLabel = class(TPRCustomLabel)
  private
    FAlignment: TAlignment;
    FClipping: boolean;
    FAlignJustified: boolean;
    FAngle: integer;
    procedure SetAlignment(Value: TAlignment);
    procedure SetAlignJustified(Value: boolean);
    procedure SetCanvasProperties(ACanvas: TPdfCanvas);
  protected
    procedure Paint; override;
    procedure Print(ACanvas: TPRCanvas; ARect: TRect); override;
  public
    property Angle: integer read Fangle write Fangle;
  published
    function GetTextWidth: Single;
    property Caption;
    property Clipping: boolean read FClipping write FClipping default false;
    property Alignment: TAlignment read FAlignment
                     write SetAlignment default taLeftJustify;
    property AlignJustified: boolean read FAlignJustified write SetAlignJustified default false;
  end;

  { TPRText }
  TPRText = class(TPRCustomLabel)
  private
    FWordwrap: boolean;
    FLeading: Single;
    FLines: TStrings;
    FAngle: integer;
    procedure SetLeading(Value: Single);
    procedure SetWordwrap(Value: boolean);
    procedure SetLines(Value: TStrings);
    procedure SetText(Value: string);
    function GetText: string;
    function GetLines: TStrings;
  protected
    procedure Paint; override;
    procedure Print(ACanvas: TPRCanvas; ARect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Text: string read GetText write SetText;
    property Angle: integer read Fangle write Fangle;
  published
    property Leading: Single read FLeading write SetLeading;
    property Lines: TStrings read GetLines write SetLines;
    property WordWrap: boolean read FWordWrap write SetWordwrap default false;
  end;

  { TPRShape }
  TPRShape = class(TPRItem)
  private
    FGradientDirection: TGradientDirection;
    FLineWidth: Single;
    FLineColor: TColor;
    FLineStyle: TPenStyle;
    FFillColor: TColor;
    FGradientColor: TColor;
    procedure SetGradientDirection(AValue: TGradientDirection);
    procedure SetLineColor(Value: TColor);
    procedure SetFillColor(Value: TColor);
    procedure SetLineWidth(Value: Single);
    procedure SetLineStyle(Value: TPenStyle);
    procedure SetGradientColor(AValue: TColor);
    procedure StdFillOrStroke(ACanvas: TPdfCanvas);
  protected
    procedure SetDash(ACanvas: TPdfCAnvas; APattern: TPenStyle);
    function  IsFillable: boolean; virtual;
    property GradientColor: TColor read FGradientColor write SetGradientColor default clNone;
    property GradientDirection: TGradientDirection read FGradientDirection write SetGradientDirection default gdVertical;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property LineWidth: Single read FLineWidth write SetLineWidth;
    property LineColor: TColor read FLineColor write SetLineColor default clBlack;
    property LineStyle: TPenStyle read FLineStyle write SetLineStyle;
    property FillColor: TColor read FFillColor write SetFillColor default clNone;
  end;
  TPRShapeClass = class of TPRShape;

  { TPRRect }
  TPRRect = class(TPRShape)
  private
    FRadius: Single;
    FCorners: TPdfCorners;
    function GetRadius: single;
    procedure SetCorners(AValue: TPdfCorners);
    procedure SetRadius(const AValue: single);
  protected
    procedure Paint; override;
    procedure Print(ACanvas: TPRCanvas; ARect: TRect); override;
  published
    property GradientColor;
    property GradientDirection;
    property Radius: single read GetRadius write SetRadius;
    property SquaredCorners: TPdfCorners read FCorners write SetCorners default [];
  end;

  { TPREllipse }
  TPREllipse = class(TPRShape)
  protected
    procedure Paint; override;
    procedure Print(ACanvas: TPRCanvas; ARect: TRect); override;
  end;

  { TPRPolygon }
  TPRPolygon = class(TPRShape)
  protected
    procedure Print(prCanvas: TPRCanvas; ARect: TRect); override;
    function  IsFillable: boolean; override;
  public
    Points: TPRPointArray;
  end;

  { TPRImage }
  TPRImage = class(TPRItem)
  private
    FSharedName: string;
    function GetScaleX: Single;
    function GetScaleY: Single;
    procedure SetStretch(Value: boolean);
    procedure SetProportional(AValue: boolean);
  protected
    FPicture: TPicture;
    FSharedImage: boolean;
    FStretch: boolean;
    FProportional: boolean;
    FScaleX,FScaleY: Single;
    procedure SetPicture(Value: TPicture); virtual;
    procedure Paint; override;
    procedure Print(ACanvas: TPRCanvas; ARect: TRect); override;
    procedure CalcProportionalBounds(var AWidth, AHeight: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ScaleX: Single read GetScaleX write FScaleX;
    Property ScaleY: Single read GetScaleY write FScaleY;
    property SharedName: string read FSharedName write FSharedName;
  published
    property Picture: TPicture read FPicture write SetPicture;
    property SharedImage: boolean read FSharedImage write FSharedImage;
    property Stretch: boolean read FStretch write SetStretch default true;
    property Proportional: boolean read FProportional write SetProportional;
  end;

  { TPRAction }
  TPRAction = class(TPersistent)
  private
    FSubType: TPRActionType;
    FDest: TPRDestination;
    FURI: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    function GetPdfObj(aPdfDoc: TPdfDoc): TPdfDictionary; virtual;
    function IsEqual(aValue: TPRAction): Boolean;
  published
    property SubType: TPRActionType read FSubType write FSubType default atURI;
    //property Dest: TPRDestination read FDest;
    Property URI: String read FURI write FURI;
  end;

  { TPRDestination }
  TPRDestination = class(TObject)
  private
    FData: TPdfDestination;
    procedure SetType(Value: TPRDestinationType);
    function GetType: TPRDestinationType;
    procedure SetElement(Index: integer; Value: Integer);
    procedure SetZoom(Value: Single);
    function GetElement(Index: integer): Integer;
    function GetZoom: Single;
  protected
    constructor Create(AData: TPdfDestination);
  public
    property Data: TPdfDestination read FData;
    property DestinationType: TPRDestinationType read GetType write SetType;
    property Left: Integer index 0 read GetElement write SetElement;
    property Top: Integer index 1 read GetElement write SetElement;
    property Right: Integer index 2 read GetElement write SetElement;
    property Bottom: Integer index 3 read GetElement write SetElement;
    property Zoom: Single read GetZoom write SetZoom;
  end;

  { TPROutlineEntry }
  TPROutlineEntry = class(TObject)
  private
    FData: TPdfOutlineEntry;
    function GetParent: TPROutlineEntry;
    function GetNext: TPROutlineEntry;
    function GetPrev: TPROutlineEntry;
    function GetFirst: TPROutlineEntry;
    function GetLast: TPROutlineEntry;
    function GetDest: TPRDestination;
    function GetTitle: string;
    function GetOpened: boolean;
    procedure SetDest(Value: TPRDestination);
    procedure SetTitle(Value: string);
    procedure SetOpened(Value: boolean);
  public
    function AddChild: TPROutlineEntry;
    property Parent: TPROutlineEntry read GetParent;
    property Next: TPROutlineEntry read GetNext;
    property Prev: TPROutlineEntry read GetPrev;
    property First: TPROutlineEntry read GetFirst;
    property Last: TPROutlineEntry read GetLast;
    property Dest: TPRDestination read GetDest write SetDest;
    property Title: string read GetTitle write SetTitle;
    property Opened: boolean read GetOpened write SetOpened;
  end;

  { TPROutlineRoot }
  TPROutlineRoot = class(TPROutlineEntry)
  protected
    constructor CreateRoot(ADoc: TPdfDoc);
  end;


  function PRPoint(x,y:single): TPRPoint;

const
  LINE_PITCH: integer = 378;
  LINE_COLOR: TColor = clSilver;
  DEFAULT_MARGIN = 32;
  PROTECT_AREA_COLOR: TColor = $00EFEFEF;
  MIN_PANEL_SIZE = 10;
  MAX_IMAGE_NUMBER = 65535;
{$IFDEF USE_JPFONTS}
  PDFFONT_CLASS_NAMES: array[0..6] of string = (
                           'FixedWidth',
                           'Arial',
                           'Times-Roman',
                           'Gothic',
                           'Mincyo',
                           'PGothic',
                           'PMincyo');
  PDFFONT_CLASS_BOLD_NAMES: array[0..6] of string = (
                           'FixedWidth-Bold',
                           'Arial-Bold',
                           'Times-Bold',
                           'Gothic,Bold',
                           'Mincyo,Bold',
                           'PGothic,Bold',
                           'PMincyo,Bold');
  PDFFONT_CLASS_ITALIC_NAMES: array[0..6] of string = (
                           'FixedWidth-Italic',
                           'Arial-Italic',
                           'Times-Italic',
                           'Gothic,Italic',
                           'Mincyo,Italic',
                           'PGothic,Italic',
                           'PMincyo,Italic');
  PDFFONT_CLASS_BOLDITALIC_NAMES: array[0..6] of string = (
                           'FixedWidth-BoldItalic',
                           'Arial-BoldItalic',
                           'Times-BoldItalic',
                           'Gothic,BoldItalic',
                           'Mincyo,BoldItalic',
                           'PGothic,BoldItalic',
                           'PMincyo');
  ITEM_FONT_NAMES: array[0..6] of string = (
                           'Courier New',
                           'Arial',
                           'Times New Roman',
                           #130#108#130#114#32#131#83#131#86#131#98#131#78,
                           #130#108#130#114#32#150#190#146#169,
                           #130#108#130#114#32#130#111#131#83#131#86#131#98#131#78,
                           #130#108#130#114#32#130#111#150#190#146#169);
  ITEM_FONT_CHARSETS: array[0..6] of TFontCharset = (
                           ANSI_CHARSET,
                           ANSI_CHARSET,
                           ANSI_CHARSET,
                           SHIFTJIS_CHARSET,
                           SHIFTJIS_CHARSET,
                           SHIFTJIS_CHARSET,
                           SHIFTJIS_CHARSET);
{$ELSE}
{$IFDEF USE_GBFONTS}
  PDFFONT_CLASS_NAMES: array[0..3] of string = (
                           'FixedWidth',
                           'Arial',
                           'Times-Roman',
                           'Chinese');
  PDFFONT_CLASS_BOLD_NAMES: array[0..3] of string = (
                           'FixedWidth-Bold',
                           'Arial-Bold',
                           'Times-Bold',
                           'Chinese');
  PDFFONT_CLASS_ITALIC_NAMES: array[0..3] of string = (
                           'FixedWidth-Italic',
                           'Arial-Italic',
                           'Times-Italic',
                           'Chinese');
  PDFFONT_CLASS_BOLDITALIC_NAMES: array[0..3] of string = (
                           'FixedWidth-BoldItalic',
                           'Arial-BoldItalic',
                           'Times-BoldItalic',
                           'Chinese');
  ITEM_FONT_NAMES: array[0..3] of string = (
                           'Courier New',
                           'Arial',
                           'TimesNewRoman',
                           'Chinese');
  ITEM_FONT_CHARSETS: array[0..3] of TFontCharset = (
                           ANSI_CHARSET,
                           ANSI_CHARSET,
                           ANSI_CHARSET,
                           GB2312_CHARSET);
{$ELSE}
  PDFFONT_CLASS_NAMES: array[0..2] of string = (
                           'FixedWidth',
                           'Arial',
                           'Times-Roman');
  PDFFONT_CLASS_BOLD_NAMES: array[0..2] of string = (
                           'FixedWidth-Bold',
                           'Arial-Bold',
                           'Times-Bold');
  PDFFONT_CLASS_ITALIC_NAMES: array[0..2] of string = (
                           'FixedWidth-Italic',
                           'Arial-Italic',
                           'Times-Italic');
  PDFFONT_CLASS_BOLDITALIC_NAMES: array[0..2] of string = (
                           'FixedWidth-BoldItalic',
                           'Arial-BoldItalic',
                           'Times-BoldItalic');
  ITEM_FONT_NAMES: array[0..2] of string = (
                           'Courier New',
                           'Arial',
                           'Times New Roman');
  ITEM_FONT_CHARSETS: array[0..2] of TFontCharset = (
                           ANSI_CHARSET,
                           ANSI_CHARSET,
                           ANSI_CHARSET);
{$ENDIF}
{$ENDIF}

implementation

{ common routines }

procedure PaintGrid(Canvas: TCanvas; aWidth, aHeight: integer;
  OffsetX, OffsetY: integer);
var
  LinePos: integer;
  LineCount: integer;
  LineFlg: boolean;

  // sub routine to set pen style
  procedure SetPen(Canvas: TCanvas; flg: boolean);
  begin
    Canvas.Pen.Color := LINE_COLOR;
    if flg then
      Canvas.Pen.Style := psSolid
    else
      Canvas.Pen.Style := psDot;
  end;

begin
  with Canvas do
  begin
    // drawing vertical lines.
    LineCount := 0;
    LineFlg := true;
    LinePos := - OffsetX;
    while LinePos < aWidth do
    begin
      if LinePos > 0 then
      begin
        MoveTo(LinePos, 0);
        SetPen(Canvas, LineFlg);
        LineTo(LinePos, aHeight - 1);
      end;
      inc(LineCount);
      LineFlg := not LineFlg;
      LinePos := trunc(LineCount * LINE_PITCH / 20) - OffsetX;
    end;

    // drawing horizontal lines.
    LineCount := 0;
    LineFlg := true;
    LinePos := - OffsetY;
    while LinePos < aHeight do
    begin
      if LinePos > 0 then
      begin
        MoveTo(0, LinePos);
        SetPen(Canvas, LineFlg);
        LineTo(aWidth - 1, LinePos);
      end;
      inc(LineCount);
      LineFlg := not LineFlg;
      LinePos := trunc(LineCount * LINE_PITCH / 20) - OffsetY;
    end;
  end;
end;

function PRPoint(x, y: single): TPRPoint;
begin
  result.x := x;
  result.y := y;
end;


procedure MixedRoundRect(Canvas:TCanvas; X1, Y1, X2, Y2: integer; RX, RY: integer;
  SqrCorners: TPdfCorners);
var
  Pts: PPoint;
  c: Integer;
  Mx,My: Integer;

  procedure Corner(Ax,Ay,Bx,By,Cx,Cy:Integer);
  begin
    ReallocMem(Pts, SizeOf(TPoint)*(c+3));
    Pts[c].x:=ax; Pts[c].y:=ay; inc(c);
    Pts[c].x:=bx; Pts[c].y:=by; inc(c);
    Pts[c].x:=cx; Pts[c].y:=cy; inc(c);
  end;

begin

  X2 := X2-1;
  Y2 := Y2-1;

  // basic checks
  if X1>X2 then
  begin
    c :=X2;
    X2 := X1;
    X1 := c;
  end;
  if Y1>Y2 then
  begin
    c := Y2;
    Y2 := Y1;
    Y1 := c;
  end;
  if RY>(Y2-Y1) then
    RY:=(Y2-Y1);
  if RX>(X2-X1) then
    RX :=(X2-X1);

  MX := RX div 2;
  MY := RY div 2;

  c := 0;
  Pts := nil;
  if pcTopLeft in SqrCorners then
    Corner(X1+MX,Y1, X1,Y1, X1,Y1+MY)
  else
    BezierArcPoints(X1,Y1,RX,RY, 90*16, 90*16, 0, Pts, c);
  if pcBottomLeft in SqrCorners then
    Corner(X1,Y2-MY,X1,Y2,X1+MX,Y2)
  else
    BezierArcPoints(X1,Y2-RY,RX,RY, 180*16, 90*16, 0, Pts, c);
  if pcBottomRight in SqrCorners then
    Corner(X2-MX,Y2, X2,Y2, X2, Y2-MY)
  else
    BezierArcPoints(X2-RX,Y2-RY,RX,RY, 270*16, 90*16, 0, Pts, c);
  if pcTopRight in SqrCorners then
    Corner(X2,Y1+MY, X2,Y1, X2-MX,Y1)
  else
    BezierArcPoints(X2-RX,Y1,RX,RY, 0, 90*16, 0, Pts, c);

  Canvas.Polygon(Pts, c);
  ReallocMem(Pts, 0);
end;

{ TPReport }

// Create
constructor TPReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileName := 'default.pdf';
  FCreationDate := now;
  FDoc := nil;
  FCanvas := TPRCanvas.Create;
end;

// Destroy
destructor TPReport.Destroy;
begin
  FCanvas.Free;
  if FDoc <> nil then Abort;
  inherited;
end;

// BeginDoc
procedure TPReport.BeginDoc;
begin
  if FDoc <> nil then Abort;
  FDoc := TPdfDoc.Create;
  with FDoc do
  begin
    UseOutlines := Self.UseOutlines;
    CompressionMethod := FCompressionMethod;
    NewDoc;
    if UseOutlines then
      FOutlineRoot := TPROutlineRoot.CreateRoot(FDoc);
    Root.PageMode := PageMode;
    Root.PageLayout := PageLayout;
    if NonFullScreenPageMode <> pmUseNone then
      Root.NonFullScreenPageMode := NonFullScreenPageMode;
    if ViewerPreference <> [] then
      Root.ViewerPreference := ViewerPreference;
    Info.Author := Author;
    Info.CreationDate := CreationDate;
    Info.Creator := Creator;
    Info.Keywords := Keywords;
    Info.ModDate := ModDate;
    Info.Subject := Subject;
    Info.Title := Title;
  end;
  FPage := 0;
end;

// Print
procedure TPReport.Print(APage: TPRPage);
begin
  FDoc.AddPage;
  inc(FPage);
  FCanvas.PdfCanvas := FDoc.Canvas;
  APage.Print(FCanvas);
end;

// EndDoc
procedure TPReport.EndDoc;
var
  FStream: TStream;
begin
  if FDoc <> nil then
  begin
    FStream := TFileStream.Create(FFileName, fmCreate);
    FDoc.SaveToStream(FStream);
    FStream.Free;
    FDoc.Free;
    FDoc := nil;
    FOutlineRoot := nil;
  end
  else
    raise EInvalidOperation.Create('document is null..');
end;

// Abort
procedure TPReport.Abort;
begin
  if FDoc <> nil then
  begin
    FDoc.Free;
    FDoc := nil;
    FOutlineRoot := nil;
  end
end;

// SetOpenAction
procedure TPReport.SetOpenAction(ADest: TPRDestination);
begin
  if (FDoc = nil) or not (FDoc.HasDoc) then
    raise EPdfInvalidOperation.Create('SetOpenAction --invalid operation.')
  else
  begin
    FDoc.Root.OpenAction := ADest.FData;
    FOpenAction := ADest;
  end;
end;

// SetAuthor
procedure TPReport.SetAuthor(Value: string);
begin
  if FDoc <> nil then
    raise EPdfInvalidOperation.Create('SetAuthor --invalid operation.');
  FAuthor := Value;
end;

// SetCreationDate
procedure TPReport.SetCreationDate(Value: TDateTime);
begin
  if FDoc <> nil then
    raise EPdfInvalidOperation.Create('SetCreationDate --invalid operation.');
  FCreationDate := Value;
end;

// SetCreator
procedure TPReport.SetCreator(Value: string);
begin
  if FDoc <> nil then
    raise EPdfInvalidOperation.Create('SetCreator --invalid operation.');
  FCreator := Value;
end;

// SetKeyWords
procedure TPReport.SetKeyWords(Value: string);
begin
  if FDoc <> nil then
    raise EPdfInvalidOperation.Create('SetKeyWords --invalid operation.');
  FKeyWords := Value;
end;

// SetModDate
procedure TPReport.SetModDate(Value: TDateTime);
begin
  if FDoc <> nil then
    raise EPdfInvalidOperation.Create('SetModDate --invalid operation.');
  FModDate := Value;
end;

// SetSubject
procedure TPReport.SetSubject(Value: string);
begin
  if FDoc <> nil then
    raise EPdfInvalidOperation.Create('SetSubject --invalid operation.');
  FSubject := Value;
end;

// SetTitle
procedure TPReport.SetTitle(Value: string);
begin
  if FDoc <> nil then
    raise EPdfInvalidOperation.Create('SetTitle --invalid operation.');
  FTitle := Value;
end;

// SetPageLayout
procedure TPReport.SetPageLayout(Value: TPRPageLayout);
begin
  if FDoc <> nil then
    raise EPdfInvalidOperation.Create('SetPageLayout --invalid operation.');
  FPageLayout := Value;
end;

// SetPageMode
procedure TPReport.SetPageMode(Value: TPRPageMode);
begin
  if FDoc <> nil then
    raise EPdfInvalidOperation.Create('SetPageMode --invalid operation.');
  FPageMode := Value;
end;

// SetNonFullScreenPageMode
procedure TPReport.SetNonFullScreenPageMode(Value: TPRPageMode);
begin
  if FDoc <> nil then
    raise EPdfInvalidOperation.Create('SetNonFullScreenPageMode --invalid operation.');
  if Value = pmFullScreen then
    FNonFullScreenPageMode := pmUseNone
  else
    FNonFullScreenPageMode := Value;
end;

// SetUseOutlines
procedure TPReport.SetUseOutlines(Value: boolean);
begin
  if FDoc <> nil then
    raise EPdfInvalidOperation.Create('SetUseOutlines --invalid operation.');
  FUseOutlines := Value;
end;

// SetViewerPreference
procedure TPReport.SetViewerPreference(Value: TPRViewerPreferences);
begin
  if FDoc <> nil then
    raise EPdfInvalidOperation.Create('SetViewerPreference --invalid operation.');
  FViewerPreference := Value;
end;

// GetOpenAction
function TPReport.GetOpenAction: TPRDestination;
begin
  if (FDoc = nil) or not (FDoc.HasDoc) then
    raise EPdfInvalidOperation.Create('GetOpenAction --invalid operation.')
  else
    result := FOpenAction;
end;

// GetPdfDoc
function TPReport.GetPdfDoc: TPdfDoc;
begin
  result := FDoc;
end;

// GetOutlineRoot
function TPReport.GetOutlineRoot: TPROutlineRoot;
begin
  if (FDoc = nil) or not (FDoc.HasDoc) or not (FUseOutlines) then
    raise EPdfInvalidOperation.Create('GetOutlineRoot --invalid operation.')
  else
    result := FOutlineRoot;
end;

// CreateDestination
function TPReport.CreateDestination: TPRDestination;
begin
  if (FDoc = nil) or not (FDoc.HasDoc) then
    raise EPdfInvalidOperation.Create('CreateDestination --invalid operation.')
  else
  begin
    result := TPRDestination.Create(FDoc.CreateDestination);
    result.Top := -10;
    result.Zoom := 1;
  end;
end;

{ TPRCanvas }

// Create
constructor TPRCanvas.Create;
begin
  inherited;
  FCanvas := nil;
end;

// SetPdfCanvas
procedure TPRCanvas.SetPdfCanvas(ACanvas: TPdfCanvas);
begin
  FCanvas := ACanvas;
end;

// GetPageHeight
function TPRCanvas.GetPageHeight: integer;
begin
  result := PdfCanvas.PageHeight;
end;

// GetPageWidth
function TPRCanvas.GetPageWidth: integer;
begin
  result := PdfCanvas.PageWidth;
end;

// SetCharSpace
procedure TPRCanvas.SetCharSpace(charSpace: Single);
begin
  PdfCanvas.SetCharSpace(charSpace);
end;

// SetWordSpace
procedure TPRCanvas.SetWordSpace(wordSpace: Single);
begin
  PdfCanvas.SetWordSpace(wordSpace);
end;

// SetHorizontalScaling
procedure TPRCanvas.SetHorizontalScaling(hScaling: Word);
begin
  PdfCanvas.SetHorizontalScaling(hScaling);
end;

// SetLeading
procedure TPRCanvas.SetLeading(leading: Single);
begin
  PdfCanvas.SetLeading(leading);
end;

// SetFont
procedure TPRCanvas.SetFont(fontname: string; size: Single);
begin
  PdfCanvas.SetFont(fontname, size);
end;

// SetTextRenderingMode
procedure TPRCanvas.SetTextRenderingMode(mode: TTextRenderingMode);
begin
  PdfCanvas.SetTextRenderingMode(mode);
end;

// SetTextRise
procedure TPRCanvas.SetTextRise(rise: Word);
begin
  PdfCanvas.SetTextRise(rise);
end;

// TextOut
procedure TPRCanvas.TextOut(X, Y: Single; Text: string);
begin
  with PdfCanvas do
    TextOut(X, PageHeight - Y - Attribute.FontSize * 0.85, Text);
end;

// TextRect
procedure TPRCanvas.TextRect(ARect: TRect; Text: string;
                            Alignment: TAlignment; Clipping: boolean);
begin
  with ARect, PdfCanvas do
    TextRect(_PdfRect(Left, PageHeight - Top, Right,
      PageHeight - Bottom), Text, TPdfAlignment(ord(Alignment)), Clipping);
end;

// TextWidth
function TPRCanvas.TextWidth(Text: string): Single;
begin
  result := PdfCanvas.TextWidth(Text);
end;

{ TPRPage }

// Create
constructor TPRPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := PDF_DEFAULT_PAGE_WIDTH;
  Height := PDF_DEFAULT_PAGE_HEIGHT;
  FMarginTop := DEFAULT_MARGIN;
  FMarginLeft := DEFAULT_MARGIN;
  FMarginRight := DEFAULT_MARGIN;
  FMarginBottom := DEFAULT_MARGIN;
  // create internel doc
  FDoc := TPdfDoc.Create;
  FDoc.SetVirtualMode;
end;

// Destroy
destructor TPRPage.Destroy;
begin
  FDoc.Free;
  inherited;
end;

// AlignControls
procedure TPRPage.AlignControls(AControl: TControl; var ARect: TRect);
begin
  ARect := Rect(ARect.Left + FMarginLeft, ARect.Top + FMarginTop,
    ARect.Right - FMarginRight, ARect.Bottom - FMarginBottom);
  inherited AlignControls(AControl, ARect);
end;

// Paint
procedure TPRPage.Paint;
var
  LinePos: integer;
  LineCount: Integer;
begin
  inherited Paint;

  with Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(GetClientRect);
    PaintGrid(Canvas, self.Width-1, self.Height-1, 0, 0);
    Font.Size := 8;
    Font.Color := clSilver;

    LineCount := 0;
    LinePos := 0;
    while LinePos < self.Width do
    begin
      TextOut(LinePos + 1, 1, IntToStr(LineCount));
      inc(LineCount);
      LinePos := trunc(LineCount * LINE_PITCH / 10);
    end;
    LineCount := 0;
    LinePos := 0;
    while LinePos < self.Height do
    begin
      TextOut(1, LinePos + 1, IntToStr(LineCount));
      inc(LineCount);
      LinePos := trunc(LineCount * LINE_PITCH / 10);
    end;

    Font := Self.Font;
    TextOut(4, 4, Name);
  end;
end;

// Print
procedure TPRPage.Print(ACanvas: TPRCanvas);
var
  i: integer;
begin
  with ACanvas.PdfCanvas do
  begin
    PageHeight := self.Height;
    PageWidth := self.Width;
  end;
  if Assigned(FPrintPageEvent) then
    FPrintPageEvent(Self, ACanvas);
  for i := 0 to ControlCount - 1 do
  begin
    if (Controls[i] is TPRPanel) then
      with (Controls[i] as TPRPanel) do
        Print(ACanvas, BoundsRect);
  end;
end;

// SetMarginTop
procedure TPRPage.SetMarginTop(Value: integer);
var
  Rect: TRect;
begin
  if (FMarginTop <> Value) and (Value >= 0) and (Value < Width div 2) then
  begin
    Rect := ClientRect;
    FMarginTop := Value;
    AlignControls(nil, Rect);
  end;
end;

// SetMarginLeft
procedure TPRPage.SetMarginLeft(Value: integer);
var
  Rect: TRect;
begin
  if (FMarginLeft <> Value) and (Value >= 0) and (Value < Width div 2) then
  begin
    Rect := ClientRect;
    FMarginLeft := Value;
    AlignControls(nil, Rect);
  end;
end;

// SetMarginRight
procedure TPRPage.SetMarginRight(Value: integer);
var
  Rect: TRect;
begin
  if (FMarginRight <> Value) and (Value >= 0) and (Value < Width div 2) then
  begin
    Rect := ClientRect;
    FMarginRight := Value;
    AlignControls(nil, Rect);
  end;
end;

// SSetMarginBottom
procedure TPRPage.SetMarginBottom(Value: integer);
var
  Rect: TRect;
begin
  if (FMarginBottom <> Value) and (Value >= 0) and (Value < Width div 2) then
  begin
    Rect := ClientRect;
    FMarginBottom := Value;
    AlignControls(nil, Rect);
  end;
end;

{ TPRPanel }

// Create
constructor TPRPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  Align := alTop;
  Height := 100;
  BevelOuter := bvNone;
  Color := clWindow;
  BorderStyle := bsNone;
end;

// GetPage
function TPRPanel.GetPage: TPRPage;
begin
  if (Parent is TPRPage) then
    result := TPRPage(Parent)
  else
    result := (Parent as TPRPanel).GetPage;
end;

// Paint
procedure TPRPanel.Paint;
var
  TmpRect: TRect;
begin
  with Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(Rect(0,0,self.Width,self.Height));
    TmpRect := GetAbsoluteRect;
    PaintGrid(Canvas, Self.Width-1, Self.Height-1, TmpRect.Left, TmpRect.Top);
    TextOut(2, 2, Name);
    Pen.Color := clGreen;
    Pen.Style := psDot;
    MoveTo(0,0);
    LineTo(self.Width-1,0);
    LineTo(self.Width-1,self.Height-1);
    LineTo(0,self.Height-1);
    LineTo(0,0);
  end;
end;

// GetAbsoluteRect
function TPRPanel.GetAbsoluteRect: TRect;
begin
  // return absolute position which based on TPRPage.
  if (Parent is TPRPanel) then
  begin
    result := TPRPanel(Parent).GetAbsoluteRect;
    OffsetRect(result, Left, Top);
  end
  else
    result := Rect(Left, Top, Left+Width, Top+Height);
end;

// Print
procedure TPRPanel.Print(ACanvas: TPRCanvas; ARect: TRect);
var
  i: integer;
  tmpRect: TRect;
begin
  for i := 0 to ControlCount - 1 do
  begin
    tmpRect := Controls[i].BoundsRect;
    OffsetRect(tmpRect, ARect.Left, ARect.Top);
    if (Controls[i] is TPRPanel) then
      TPRPanel(Controls[i]).Print(ACanvas, tmpRect)
    else
    if (Controls[i] is TPRItem) then
      if TPRItem(Controls[i]).Printable then
        TPRItem(Controls[i]).Print(ACanvas, tmpRect);
  end;
end;

{ TPRLayoutPanel }

// SetParent
procedure TPRLayoutPanel.SetParent(AParent: TWinControl);
begin
  if (AParent <> nil) and
   (not (AParent is TPRPanel) and not (AParent is TPRPage)) then
    raise Exception.Create('TPRPage can not set on ' + AParent.ClassName);
  if (AParent is TPRGridPanel) then
    AParent := TPRGridPanel(AParent).FChildPanel;
  inherited SetParent(AParent);
end;

// Print
procedure TPRLayoutPanel.Print(ACanvas: TPRCanvas; ARect: TRect);
begin
  if Assigned(FBeforePrint) then
    FBeforePrint(Self, ACanvas, ARect);
    
  inherited Print(ACanvas, ARect);

  if Assigned(FAfterPrint) then
    FAfterPrint(Self, ACanvas, ARect);
end;

{ TPRGridPanel }

// Create
constructor TPRGridPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColCount := 1;
  FRowCount := 1;
  FChildPanel := TPRChildPanel.Create(Self);
  FChildPanel.Align := alClient;
  FChildPanel.Parent := Self;
end;

// Destroy
destructor TPRGridPanel.Destroy;
begin
  FChildPanel.Free;
  inherited;
end;

// GetChildParent
function TPRGridPanel.GetChildParent: TComponent;
begin
  Result := FChildPanel;
end;

// GetChildren
procedure TPRGridPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  FChildPanel.GetChildren(Proc, Root);
end;

// AlignControls
procedure TPRGridPanel.AlignControls(AControl: TControl; var ARect: TRect);
begin
  if FColCount > 1 then
    ARect.Right := ARect.Left + (ARect.Right-ARect.Left) div ColCount;
  if FRowCount > 1 then
    ARect.Bottom := ARect.Top + (ARect.Bottom-ARect.Top) div RowCount;
  inherited AlignControls(AControl, ARect);
end;

// SetColCount
procedure TPRGridPanel.SetColCount(Value: integer);
var
  Rect: TRect;

  procedure raiseex;
  begin
    raise Exception.Create('invalid colcount');
  end;

begin
  if Value <> FColCount then
  begin
    if (Value < 1) then
      raiseex;

    FColCount := Value;

    if HandleAllocated then begin
      if ((Width div Value) < MIN_PANEL_SIZE) then
        raiseex;

      Rect := GetClientRect;
      AlignControls(nil, Rect);
      Invalidate;
    end;
  end;
end;

// SetRowCount
procedure TPRGridPanel.SetRowCount(Value: integer);
var
  Rect: TRect;

  procedure raiseex;
  begin
    raise Exception.CreateFmt('invalid rowcount=%d',[Value]);
  end;

begin
  if Value <> FRowCount then
  begin
    if (Value < 1) then
      raiseex;

    FRowCount := Value;

    if handleallocated then begin
      if ((Height div Value) < MIN_PANEL_SIZE) then
        raiseex;

      Rect := GetClientRect;
      AlignControls(nil, Rect);
      Invalidate;
    end;

  end;
end;

// Paint
procedure TPRGridPanel.Paint;
var
  TmpRect: TRect;
  TmpWidth, TmpHeight: integer;
  i: integer;
begin
  with Canvas do
  begin
    if (FColCount > 1) or (FRowCount > 1) then
    begin
      Brush.Color := PROTECT_AREA_COLOR;
      FillRect(GetClientRect);
    end;
    TmpWidth := Trunc(self.Width / FColCount);
    TmpHeight := Trunc(self.Height / FRowCount);
    Brush.Color := clWhite;
    FillRect(Rect(0,0,TmpWidth,TmpHeight));
    TmpRect := GetAbsoluteRect;
    PaintGrid(Canvas, self.Width, self.Height, TmpRect.Left, TmpRect.Top);

    // draw ruled line
    Pen.Color := clBlue;
    Pen.Style := psDot;
    for i := 0 to FRowCount do
    begin
      TmpHeight := Trunc(Self.Height*i/FRowCount);
      if TmpHeight = self.Height then
        dec(TmpHeight);
      MoveTo(0,TmpHeight);
      LineTo(self.Width,TmpHeight);
    end;
    for i := 0 to FColCount do
    begin
      TmpWidth := Trunc(self.Width*i/FColCount);
      if TmpWidth = self.Width then
        dec(TmpWidth);
      MoveTo(TmpWidth,0);
      LineTo(TmpWidth,self.Height);
    end;

    FChildPanel.Repaint;
  end;
end;

// Print
procedure TPRGridPanel.Print(ACanvas: TPRCanvas; ARect: TRect);
var
  i, j: integer;

  procedure PrintSubPanel(ACol, ARow: integer);
  var
    tmpRect: TRect;
    OffsetY, OffsetX: Integer;
  begin
    tmpRect := ARect;
    OffsetY := Trunc(Height * ARow / FRowCount);
    OffsetX := Trunc(Width * ACol / FColCount);
    tmpRect.Right := tmpRect.Left + FChildPanel.Width;
    tmpRect.Bottom := tmpRect.Top + FChildPanel.Height;
    OffsetRect(tmpRect, OffsetX, OffsetY);
    if Assigned(FBeforePrintChild) then
      FBeforePrintChild(Self, ACanvas, ACol, ARow, tmpRect);
    FChildPanel.Print(ACanvas, tmpRect);
    if Assigned(FAfterPrintChild) then
      FAfterPrintChild(Self, ACanvas, ACol, ARow, tmpRect);
  end;
begin
  if Assigned(FBeforePrint) then
    FBeforePrint(Self, ACanvas, ARect);
  // printing FChildPanel each row and col.
  if FPrintDirection = pdVert then
    for i := 0 to FColCount - 1 do
      for j := 0 to FRowCount - 1 do
        PrintSubPanel(j, i)
  else
    for j := 0 to FRowCount - 1 do
      for i := 0 to FColCount - 1 do
        PrintSubPanel(i, j);
  if Assigned(FAfterPrint) then
    FAfterPrint(Self, ACanvas, ARect);
end;

// SetParent
procedure TPRGridPanel.SetParent(AParent: TWinControl);
begin
  if (AParent <> nil) and
   (not (AParent is TPRPanel) and not (AParent is TPRPage)) then
    raise Exception.Create('TPRPage can not set on ' + AParent.ClassName);
  inherited SetParent(AParent);
end;

{ TPRItem }

constructor TPRItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 30;
  FPrintable := true;
end;

// SetParent
procedure TPRItem.SetParent(AParent: TWinControl);
begin
  if (AParent <> nil) and
   (not (AParent is TPRPanel)) then
    raise Exception.Create('this component must set on TPRPanel');
  if (AParent is TPRGridPanel) then
    AParent := TPRGridPanel(AParent).FChildPanel;
  inherited SetParent(AParent);
end;

// Print
procedure TPRItem.Print(ACanvas: TPRCanvas; ARect: TRect);
begin
  // abstract method..
end;

// GetPage
function TPRItem.GetPage: TPRPage;
begin
  result := (Parent as TPRPanel).Page;
end;

// GetInternalDoc
function TPRItem.GetInternalDoc: TPdfDoc;
begin
  result := Page.InternalDoc;
end;

{ TPRCustomLabel }

constructor TPRCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Canvas.Brush.Style := bsClear;
  FFontName := fnArial;
  FFontSize := 12;
  FFontBold := false;
  FFontItalic := false;
  {$IFDEF USE_JPFONTS}
  FFontName := fnGothic;
  {$ENDIF}
  Font.Name := ITEM_FONT_NAMES[ord(FFontName)];
  Font.CharSet := ITEM_FONT_CHARSETS[ord(FFontName)];
  Font.Size := Round(FFontSize*0.75);
  ParentFont := false;
end;

// SetFontName
procedure TPRCustomLabel.SetFontName(Value: TPRFontName);
begin
  if FFontName <> Value then
  begin
    FFontName := Value;
    Font.Name := ITEM_FONT_NAMES[ord(Value)];
    Font.CharSet := ITEM_FONT_CHARSETS[ord(Value)];
    Invalidate;
  end;
end;

// SetFontItalic
procedure TPRCustomLabel.SetFontItalic(Value: boolean);
begin
  if FFontItalic <> Value then
  begin
    FFontItalic := Value;
    if Value then
      Font.Style := Font.Style + [fsItalic]
    else
      Font.Style := Font.Style - [fsItalic];
    Invalidate;
  end;
end;

// SetFontBold
procedure TPRCustomLabel.SetFontBold(Value: boolean);
begin
  if FFontBold <> Value then
  begin
    FFontBold := Value;
    if Value then
      Font.Style := Font.Style + [fsBold]
    else
      Font.Style := Font.Style - [fsBold];
    Invalidate;
  end;
end;

// SetFontSize
procedure TPRCustomLabel.SetFontSize(Value: Single);
begin
  if (FFontSize <> Value) and (Value > 0) then
  begin
    FFontSize := Value;
    Font.Size := Round(Value*0.75);
    Invalidate;
  end;
end;

// SetWordSpace
procedure TPRCustomLabel.SetWordSpace(Value: Single);
begin
  if (Value <> FWordSpace) and (Value >= 0) then
  begin
    FWordSpace := Value;
    Invalidate;
  end;
end;

// CMTextChanged
{$IFDEF LAZ_POWERPDF}
procedure TPRCustomLabel.CMTextChanged(var Message: TLMessage);
{$ELSE}
procedure TPRCustomLabel.CMTextChanged(var Message: TMessage);
{$ENDIF}
begin
  Invalidate;
end;

// InternalTextout
{$IFDEF LAZ_POWERPDF}
function TPRCustomLabel.InternalTextout(APdfCanvas: TPdfCanvas;
                       S: string; X, Y: integer): Single;
var
  Pos: Double;
  i: integer;
  Word: string;
begin
  Pos := X;
  for i:=1 to UTF8Length(S) do begin
    Word := UTF8Copy(s, i, 1);
    Canvas.TextOut(Round(Pos), Y, Word);
    with APdfCanvas do begin
      Pos := Pos + TextWidth(Word) + Attribute.CharSpace;
      if Word=' ' then
        Pos := Pos + Attribute.WordSpace;
    end;
  end;
  result := Pos;
end;
{$ELSE}
function TPRCustomLabel.InternalTextout(APdfCanvas: TPdfCanvas;
                       S: string; X, Y: integer): Single;
var
  Pos: Double;
  i: integer;
  Word: string;
  ln: integer;
begin
  // printing text and the end point of canvas.
  i := 1;
  Pos := X;
  ln := Length(S);

  if ((ln >= 2) and (S[ln] = #10) and (S[ln-1] = #13)) then
    ln := ln - 2;

  while true do
  begin
    if i > ln then
      Break;
    if ByteType(S, i) = mbLeadByte then
    begin
      Word := Copy(S, i, 2);
      inc(i);
    end
    else
      Word := S[i];
    Canvas.TextOut(Round(Pos), Y, Word);
    with APdfCanvas do
      Pos := Pos + TextWidth(Word) + Attribute.CharSpace;
    if S[i] = ' ' then
      Pos := Pos + FWordSpace;
    inc(i);
  end;
  result := Pos;
end;
{$ENDIF}

// GetFontClassName
function TPRCustomLabel.GetFontClassName: string;
begin
  if FFontBold then
    if FFontItalic then
      result := PDFFONT_CLASS_BOLDITALIC_NAMES[ord(FFontName)]
    else
      result := PDFFONT_CLASS_BOLD_NAMES[ord(FFontName)]
  else
    if FFontItalic then
      result := PDFFONT_CLASS_ITALIC_NAMES[ord(FFontName)]
    else
      result := PDFFONT_CLASS_NAMES[ord(FFontName)];
end;

{ TPRLabel }

// SetAlignment
procedure TPRLabel.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

// SetAlignJustified
procedure TPRLabel.SetAlignJustified(Value: boolean);
begin
  if Value <> FAlignJustified then
  begin
    FAlignJustified := Value;
    Invalidate;
  end;
end;

// Paint
procedure TPRLabel.Paint;
var
  PdfCanvas: TPdfCanvas;
  FText: string;
  tmpWidth: Single;
  XPos: integer;
begin
  if Length(Caption) = 0 then Exit;

  PdfCanvas := GetInternalDoc.Canvas;

  // setting canvas attribute to the internal doc(to get font infomation).
  SetCanvasProperties(PdfCanvas);

  with Canvas do
  begin
    Font := Self.Font;
    FText := Caption;

    // calculate text width
    tmpWidth := PdfCanvas.TextWidth(FText);

    case FAlignment of
      taCenter: XPos := Round((Self.Width - tmpWidth) / 2);
      taRightJustify: XPos :=Self.Width - Round(tmpWidth);
    else
      XPos := 0;
    end;
    InternalTextout(PdfCanvas, FText, XPos, 0);
  end;
end;

// Print
procedure TPRLabel.Print(ACanvas: TPRCanvas; ARect: TRect);
begin
  if Length(Caption) = 0 then Exit;

  SetCanvasProperties(ACanvas.PdfCanvas);

  // Only one line of text rotated for now. It's the begining
  if Angle=90 then
    ACanvas.PdfCanvas.TextOutRotatedUp(ARect.Left + FontSize,  GetPage.Height - ARect.Top, Caption)
  else
    ACanvas.TextRect(ARect, Caption, FAlignment, Clipping);
end;

function TPRLabel.GetTextWidth: Single;
begin
  with GetInternalDoc do
  begin
    SetCanvasProperties(Canvas);
    Result := Canvas.TextWidth(Caption);
  end;
end;

procedure TPRLabel.SetCanvasProperties(ACanvas: TPdfCanvas);
var
  tmpWidth: Single;
  tmpCharSpace: Single;
  CharCount: integer;
  {$IFDEF LAZ_POWERPDF}
  str: string;
  {$ENDIF}
begin
  // setting canvas attribute to the internal doc(to get font infomation).
  with ACanvas do
  begin
    SetFont(GetFontClassName, FontSize);
    SetRGBFillColor(FontColor);
    SetWordSpace(WordSpace);
    if AlignJustified then
    begin
      SetCharSpace(0);
      {$IFDEF LAZ_POWERPDF}
      str := UTF8Trim(Caption, [u8tKeepStart]);
      tmpWidth := TextWidth(str);
      CharCount := _GetSpcCount(str);
      if CharCount>0 then begin
        tmpCharSpace := (Self.Width - tmpWidth) / CharCount;
        SetWordSpace(tmpCharSpace);
      end;
      {$ELSE}
      tmpWidth := TextWidth(Caption);
      CharCount := _GetCharCount(Caption);
      if CharCount > 1 then
        tmpCharSpace := (self.Width - tmpWidth) / (CharCount - 1)
      else
        tmpCharSpace := 0;
      if tmpCharSpace > 0 then
        SetCharSpace(tmpCharSpace);
      {$ENDIF}
    end
    else
      SetCharSpace(CharSpace);
  end;
end;

{ TPRText }

// SetLines
procedure TPRText.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
  Invalidate;
end;

// GetLines
function TPRText.GetLines: TStrings;
begin
  result := FLines;
end;

// SetText
procedure TPRText.SetText(Value: string);
begin
  FLines.Text := Value;
end;

// GetText
function TPRText.GetText: string;
begin
  result := TrimRight(FLines.Text);
end;

// Create
constructor TPRText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLeading := 14;
  FLines := TStringList.Create;
end;

// Destroy
destructor TPRText.Destroy;
begin
  FLines.Free;
  inherited;
end;

// Paint
procedure TPRText.Paint;
var
  i: integer;
  S1, S2: string;
  XPos: Single;
  TmpXPos: Double;
  ARect: TRect;
  ln: integer;
  PdfCanvas: TPdfCanvas;
  FText: string;
  ForceReturn: boolean;
  tmpWidth: Single;

  procedure DrawRect;
  begin
    with Canvas do
    begin
      Pen.Color := clNavy;
      Pen.Style := psDot;
      MoveTo(0, 0);
      LineTo(self.Width-1, 0);
      LineTo(self.Width-1, self.Height-1);
      LineTo(0, self.Height-1);
      LineTo(0, 0);
    end;
  end;

begin
  // this is useless way, but I don't think of more smart way.
  PdfCanvas := GetInternalDoc.Canvas;

  // setting canvas attribute to the internal doc(to get font infomation).
  with PdfCanvas do
  begin
    SetFont(GetFontClassName, FontSize);
    SetLeading(Leading);
    SetWordSpace(WordSpace);
    SetCharSpace(CharSpace);
  end;

  with Canvas do
  begin
    Font := Self.Font;
    ARect := ClientRect;
    FText := Lines.Text;
    i := 1;
    S2 := PdfCanvas.GetNextWord(FText, i);
    XPos := ARect.Left + PdfCanvas.TextWidth(S2);
    if (S2 <> '') and (S2[Length(S2)] = ' ') then
      XPos := XPos + WordSpace;

    while i <= Length(FText) do
    begin
      ln := Length(S2);
      if (ln >= 2) and (S2[ln] = #10) and (S2[ln-1] = #13) then
      begin
        S2 := Copy(S2, 1, ln - 2);
        ForceReturn := true;
      end
      else
        ForceReturn := false;

      S1 := PdfCanvas.GetNextWord(FText, i);

      tmpWidth := PdfCanvas.TextWidth(S1);
      TmpXPos := XPos + tmpWidth;

      if (FWordWrap and (TmpXPos > ARect.Right)) or
        ForceReturn then
      begin
        if S2 <> '' then
          InternalTextOut(PdfCanvas, S2, ARect.Left, ARect.Top);
        S2 := '';
        ARect.Top := ARect.Top + Round(Leading);
        if ARect.Top > ARect.Bottom - FontSize then
          Break;
        XPos := ARect.Left;
      end;
      XPos := XPos + tmpWidth;
      if S1[Length(S1)] = ' ' then
        XPos := XPos + WordSpace;
      S2 := S2 + S1;
    end;

    if S2 <> '' then
      InternalTextout(PdfCanvas, S2, ARect.Left, ARect.Top);
  end;

  DrawRect;
end;

// Print
procedure TPRText.Print(ACanvas: TPRCanvas; ARect: TRect);
begin
  with ACanvas.PdfCanvas do
  begin
    SetFont(GetFontClassName, FontSize);
    SetRGBFillColor(FontColor);
    SetCharSpace(CharSpace);
    SetWordSpace(WordSpace);
    SetLeading(Leading);
    Attribute.FontUnderline:=FontUnderline;
    // Only one line of text rotated for now. It's the begining
    if Angle=90 then
      TextOutRotatedUp(Left + FontSize,  GetPage.Height- Top, Text)
    else
    with ARect do
      MultilineTextRect(_PdfRect(Left, GetPage.Height- Top, Right, GetPage.Height- Bottom),
        Text, WordWrap);
  end;
end;

// SetCharSpace
procedure TPRCustomLabel.SetCharSpace(Value: Single);
begin
  if (Value <> FCharSpace) then
  begin
    FCharSpace := Value;
    Invalidate;
  end;
end;

procedure TPRCustomLabel.SetFontUnderline(const Value: boolean);
begin
  if FFontUnderLine <> Value then
  begin
    FFontUnderLine := Value;
    if Value then
      Font.Style := Font.Style + [fsUnderline]
    else
      Font.Style := Font.Style - [fsUnderline];
    Invalidate;
  end;
end;

// SetLeading
procedure TPRText.SetLeading(Value: Single);
begin
  if (Value <> FLeading) and (Value >= 0) then
  begin
    FLeading := Value;
    Invalidate;
  end;
end;

// SetWordwrap
procedure TPRText.SetWordwrap(Value: boolean);
begin
  if Value <> FWordwrap then
  begin
    FWordwrap := Value;
    Invalidate;
  end;
end;

// SetFontColor
procedure TPRCustomLabel.SetFontColor(Value: TColor);
begin
  if Value > $0FFFFFFF then
    raise EPdfInvalidValue.Create('the color you selected is not allowed.');
  if (Value <> FFontColor) then
  begin
    FFontColor := Value;
    Font.Color := Value;
    Invalidate;
  end;
end;

{ TPRShape }

// Create
constructor TPRShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineColor := clBlack;
  FFillColor := clNone;
  FGradientColor := clNone;
end;

// SetLineColor
procedure TPRShape.SetLineColor(Value: TColor);
begin
  if Value <> FLineColor then
  begin
    FLineColor := Value;
    Invalidate;
  end;
end;

procedure TPRShape.SetGradientDirection(AValue: TGradientDirection);
begin
  if FGradientDirection = AValue then Exit;
  FGradientDirection := AValue;
  Invalidate;
end;

// SetLineStyle
procedure TPRShape.SetLineStyle(Value: TPenStyle);
begin
  if Value <> FLineStyle then
  begin
    FLineStyle := Value;
    Invalidate;
  end;
end;

procedure TPRShape.SetGradientColor(AValue: TColor);
begin
  if FGradientColor = AValue then Exit;
  FGradientColor := AValue;
  Invalidate;
end;

procedure TPRShape.StdFillOrStroke(ACanvas: TPdfCanvas);
begin
  with ACanvas do
  begin
    if (FillColor <> clNone) and IsFillable then
      SetRGBFillColor(FFillColor);

    if LineColor <> clNone then
    begin
      SetRGBStrokeColor(FLineColor);
      SetLineWidth(FLineWidth);
    end;

    if FillColor <> clNone then
      if IsFillable then
        if (LineColor <> clNone) and (LineStyle <> psClear) then
          ClosepathFillStroke
        else
        begin
          Closepath;
          Fill;
        end
      else
      begin
        Stroke;
        Newpath;
      end
    else
      if IsFillable then
        ClosePathStroke
      else
      begin
        Stroke;
        Newpath;
      end;
  end;
end;

// SetFillColor
procedure TPRShape.SetFillColor(Value: TColor);
begin
  if Value <> FFillColor then
  begin
    FFillColor := Value;
    Invalidate;
  end;
end;

// SetLineWidth
procedure TPRShape.SetLineWidth(Value: Single);
begin
  if (Value <> FLineWidth) and (Value >= 0) then
  begin
    FLineWidth := Value;
    Invalidate;
  end;
end;

// SetDash
procedure TPRShape.SetDash(ACanvas: TPdfCAnvas; APattern: TPenStyle);
begin
  // emurate TPenStyle
  with ACanvas do
    case APattern of
      psSolid, psInsideFrame: SetDash([0], 0);
      psDash: SetDash([16, 8], 0);
      psDashDot: SetDash([8, 7, 2, 7], 0);
      psDashDotDot: SetDash([8, 4, 2, 4, 2, 4], 0);
      psDot: SetDash([3], 0);
    end;
end;

function TPRShape.IsFillable: boolean;
begin
  result := (self.Width > 1) and (self.Height > 1);
end;


{ TPRRect }

function TPRRect.GetRadius: single;
begin
  if ClientHeight<ClientWidth then
    Result := ClientHeight
  else
    Result := ClientWidth;

  if FRadius<0.0 then
    Result := Result/8  // min(w,h)/4 is default diamter
  else
    Result := FRadius;
end;

procedure TPRRect.SetCorners(AValue: TPdfCorners);
begin
  if FCorners=AValue then Exit;
  FCorners:=AValue;
  Invalidate;
end;

procedure TPRRect.SetRadius(const AValue: single);
begin
  if AValue<>FRadius then begin
    FRadius := AValue;
    if FRadius<0.0 then
      FRadius := -1.0;
    Invalidate;
  end;
end;

// Paint
procedure TPRRect.Paint;
var
  ARect: TRect;
  ARadius: Integer;
begin
  ARect := ClientRect;
  with Canvas, ARect do
  begin
    if self.Height > 1 then
      Bottom := Bottom - 1;
    if self.Width > 1 then
      Right := Right - 1;

    ARadius := round(Radius);

    if FillColor <> clNone then
    begin
      Brush.Color := FFillColor;
      Brush.Style := bsSolid;
      if ARadius=0 then begin
        if GradientColor <> clNone then
          GradientFill(ARect, ColorToRGB(FFillColor), ColorToRGB(GradientColor), GradientDirection)
        else
          FillRect(ARect);
      end;
    end
    else
      Brush.Style := bsClear;

    if LineColor <> clNone then
    begin
      Pen.Style := FLineStyle;
      Pen.Width := Round(FLineWidth);
      Pen.Color := FLineColor;
      if ARadius=0 then
        Polyline([Point(Left,Top), Point(Right,Top),
          Point(Right,Bottom), Point(Left,Bottom)]);
    end;

    if ARadius<>0 then
      MixedRoundRect(Canvas, Left,Top,Right,Bottom,ARadius*2,ARadius*2,
                     SquaredCorners);
  end;
end;

// Print
procedure TPRRect.Print(ACanvas: TPRCanvas; ARect: TRect);
var
  PageHeight: integer;
  ARadius: single;
begin
  PageHeight := GetPage.Height;
  with ARect do
  begin
    Top := PageHeight - Top;
    if self.Height > 1 then
      Bottom := PageHeight - Bottom + 1
    else
      Bottom := PageHeight - Bottom;
    if self.Width > 1 then
      Right := Right - 1;

    if (self.Height <= 1) and (self.Width <= 1) then Exit;

    if (LineColor = clNone) or (LineStyle = psClear) then
      if (self.Height <= 1) or (self.Width <= 1) then Exit;

    SetDash(ACanvas.PdfCanvas, FLineStyle);

    ARadius := Radius;

    with ACanvas.PdfCanvas do
    begin

      if self.GradientColor<>clNone then
        case Self.GradientDirection of
          gdVertical:
            SetGradientFill(2, ColorToRGB(FillColor), ColorToRGB(GradientColor), [0,Top,0,Bottom]);
          gdHorizontal:
            SetGradientFill(2, ColorToRGB(FillColor), ColorToRGB(GradientColor), [Left,0,Right,0]);
        end;

      if ARadius<>0.0 then
        RoundRect(Left, Bottom, Right-Left, Top-Bottom, ARadius, ARadius,
          SquaredCorners)
      else
      begin
        MoveTo(Left, Top);

        if self.Width > 1 then
        begin
          LineTo(Right, Top);
          if self.Height > 1 then
            LineTo(Right, Bottom);
        end;
        if self.Height > 1 then
          LineTo(Left, Bottom);
      end;
    end;

    if self.GradientColor<>clNone then
    begin
      if (LineColor = clNone) or (LineStyle = psClear) then
      begin
        ACanvas.PdfCanvas.Closepath;
        ACanvas.PdfCanvas.Fill;
      end else
        ACanvas.PdfCanvas.ClosepathFillStroke
    end
    else
      StdFillOrStroke(ACanvas.PDFCanvas);
  end;
end;

{ TPREllipse }

// Paint
procedure TPREllipse.Paint;
var
  ARect: TRect;
begin
  ARect := ClientRect;
  with ARect, Canvas do
  begin
    if self.Height > 1 then
      Bottom := Bottom - 1;
    if self.Width > 1 then
      Right := Right - 1;

    if FillColor <> clNone then
    begin
      Brush.Color := FFillColor;
      Brush.Style := bsSolid;
    end
    else
      Brush.Style := bsClear;

    if (LineColor <> clNone) and (LineStyle <> psClear) then
    begin
      Pen.Style := FLineStyle;
      Pen.Width := Round(FLineWidth);
      Pen.Color := FLineColor;
    end
    else
      Pen.Style := psClear;

    Ellipse(Left, Top, Right, Bottom);
  end;
end;

// Print
procedure TPREllipse.Print(ACanvas: TPRCanvas; ARect: TRect);
var
  PageHeight: integer;
begin
  PageHeight := GetPage.Height;
  with ARect do
  begin
    Top := PageHeight - Top;
    if self.Height > 1 then
      Bottom := PageHeight - Bottom + 1
    else
      Bottom := PageHeight - Bottom;
    if self.Width > 1 then
      Right := Right - 1;

    if (self.Height <= 1) and (self.Width <= 1) then Exit;

    if (LineColor = clNone) or (LineStyle = psClear) then
      if (self.Height <= 1) or (self.Width <= 1) then Exit;

    SetDash(ACanvas.PdfCanvas, FLineStyle);

    with ACanvas.PdfCanvas do
    begin
      with ARect do
        Ellipse(Left, Top, Right - Left, Bottom - Top);

      StdFillOrStroke(ACanvas.PdfCanvas);
    end;
  end;
end;

{ TPRImage }

// Paint
procedure TPRImage.Paint;
var
  R: TRect;
  AWidth,AHeight: Integer;
  RatioH,RatioW: Double;
begin
  if (FPicture = nil) or (FPicture.Graphic = nil) or
   (FPicture.Graphic.Empty) then
    with Canvas do
    begin
      Brush.Style := bsClear;
      TextOut(4, 4, Name);
      Pen.Color := clBlue;
      Pen.Style := psDot;
      Polygon([Point(0, 0), Point(self.Width-1, 0),
        Point(self.Width-1, self.Height-1), Point(0, self.Height-1)]);
    end
  else
  if FStretch then
  begin
    R := GetClientRect;

    AWidth := R.Right-R.Left;
    AHeight := R.Bottom-R.Top;
    if FProportional then
      CalcProportionalBounds(AWidth, AHeight);
    R.Right := R.Left + AWidth;
    R.Bottom := R.Top + AHeight;

    Canvas.StretchDraw(R, FPicture.Graphic)
  end
  else
    Canvas.Draw(0, 0, FPicture.Graphic);
end;

// Print
procedure TPRImage.Print(ACanvas: TPRCanvas; ARect: TRect);
var
  FDoc: TPdfDoc;
  FXObjectName: string;
  i: integer;
  FIdx: integer;
  AWidth,AHeight: Integer;
  WidthF,HeightF: Single;
begin
  if (FPicture = nil) or (FPicture.Graphic = nil) or
   (FPicture.Graphic.Empty) {or not (FPicture.Graphic is TFPImageBitmap)} then
    Exit;
  FDoc := ACanvas.PdfCanvas.Doc;
  if SharedImage then
  begin
    FXObjectName := Self.Name;
    if FXObjectName='' then
      FXObjectName := FSharedName;
    if FDoc.GetXObject(FXObjectName) = nil then
      FDoc.AddXObject(FXObjectName, CreatePdfImage(FPicture.Graphic, 'Pdf-Bitmap', FDoc.ObjectMgr));
  end
  else
  begin
    FIdx := Random(MAX_IMAGE_NUMBER - 1);
    for i := 0 to MAX_IMAGE_NUMBER - 1 do
    begin
      FXObjectName := Self.Name + IntToStr(FIdx);
      if FDoc.GetXObject(FXObjectName) = nil then Break;
      if i = MAX_IMAGE_NUMBER then
        raise Exception.Create('image count over max value..');
      inc(FIdx);
      if FIdx >= MAX_IMAGE_NUMBER then
        FIdx := 0;
    end;
    FDoc.AddXObject(FXObjectName, CreatePdfImage(FPicture.Graphic, 'Pdf-Bitmap', FDoc.ObjectMgr));
  end;
  with ARect, ACanvas.PdfCanvas do
    if FStretch then begin
      AWidth := self.Width;
      AHeight := self.Height;
      if FProportional then
        CalcProportionalBounds(AWidth, AHeight);
      DrawXObject(Left, GetPage.Height - Top - AHeight, AWidth, AHeight, FXObjectName)
    end
    else begin
      WidthF := FPicture.Width * ScaleX;
      HeightF := FPicture.Height * ScaleY;
      DrawXObjectEx(Left, GetPage.Height - Top - HeightF, WidthF, HeightF,
            Left, GetPage.Height - Top - self.Height, self.Width, self.Height, FXObjectName);
    end;
end;

procedure TPRImage.CalcProportionalBounds(var AWidth, AHeight: Integer);
var
  RatioW,RatioH: Double;
begin
  if (FPicture.Height<>0) and (FPicture.Width<>0) then begin
    RatioW := AWidth/FPicture.Width;
    RatioH := AHeight/FPicture.Height;
    if RatioH<RatioW then
      RatioW := RatioH;
    AWidth := Round(RatioW * FPicture.Width);
    AHeight := Round(RatioW * FPicture.Height);
  end;
end;

// Create
constructor TPRImage.Create(AOwner: TComponent);
begin
  inherited;
  FPicture := TPicture.Create;
  FSharedImage := true;
  FStretch := true;
  FScaleX := 1.0;
  FScaleY := 1.0;
  Randomize;
end;

// SetPicture
procedure TPRImage.SetPicture(Value: TPicture);
begin
  if (Value = nil) or (Value.Graphic = nil) or (Value.Graphic is TBitmap) then
  begin
    FPicture.Assign(Value);
    Invalidate;
  end
  else
    raise exception.Create('only bitmap image is allowed.');
end;

// SetStretch
procedure TPRImage.SetStretch(Value: boolean);
begin
  if Value = FStretch then Exit;
  FStretch := Value;
  Invalidate;
end;

function TPRImage.GetScaleX: Single;
begin
  if FScaleX<=0 then
    result := 1.0
  else
    result := FScaleX;
end;

function TPRImage.GetScaleY: Single;
begin
  if FScaleY<=0 then
    result := 1.0
  else
    result := FScaleY;
end;

procedure TPRImage.SetProportional(AValue: boolean);
begin
  if AValue <> FProportional then
  begin
    FProportional := AValue;
    Invalidate;
  end;
end;

// Destroy
destructor TPRImage.Destroy;
begin
  FPicture.Free;
  inherited;
end;

{ TPRAction }
procedure TPrAction.AssignTo(Dest: TPersistent);
var
  destAction: TPrAction;
begin
  if Dest is TPrAction then begin
    destAction := Dest as TPrAction;
    destAction.SubType:=SubType;
    destAction.URI:=URI;
  end;
end;

function TPrAction.IsEqual(aValue: TPRAction): Boolean;
begin
  result := Assigned(aValue) and
            (aValue.SubType = SubType) and
            (aValue.URI = URI);
end;

function TPrAction.GetPdfObj(aPdfDoc: TPdfDoc): TPdfDictionary;
var
  objMgr: TPdfObjectMgr;
begin
  if Assigned(aPdfDoc) then
    objMgr := aPdfDoc.ObjectMgr
  else
    objMgr := nil;
  result := TPdfDictionary.CreateDictionary(objMgr);
  if Assigned(objMgr) then
    // create indirect object
    objMgr.AddObject(result);
  result.AddItem('Type', TPdfName.CreateName('Action'));
  result.AddItem('S', TPdfName.CreateName(PDF_ACTION_TYPE_NAMES[Ord(SubType)]));
  case SubType of
    atURI:
      begin
        result.AddItem('URI', TPdfText.CreateText(URI))
      end;
    else
      Exception.Create('Subtype ' + PDF_ACTION_TYPE_NAMES[Ord(SubType)] + ' not implemented.');
  end;
end;

{ TPRDestination }
procedure TPRDestination.SetType(Value: TPRDestinationType);
begin
  FData.DestinationType := Value;
end;

function TPRDestination.GetType: TPRDestinationType;
begin
  result := FData.DestinationType;
end;

procedure TPRDestination.SetElement(Index: integer; Value: Integer);
begin
  case Index of
    0: FData.Left := Value;
    1: FData.Top := FData.PageHeight - Value;
    2: FData.Right := Value;
    3: FData.Bottom := FData.PageHeight - Value;
  end;
end;

procedure TPRDestination.SetZoom(Value: Single);
begin
  FData.Zoom := Value;
end;

function TPRDestination.GetElement(Index: integer): Integer;
begin
  case Index of
    0: Result := FData.Left;
    1: Result := FData.Top;
    2: Result := FData.Right;
  else
    Result := FData.Bottom;
  end;
end;

function TPRDestination.GetZoom: Single;
begin
  Result := FData.Zoom;
end;

constructor TPRDestination.Create(AData: TPdfDestination);
begin
  inherited Create;
  FData := AData;
  AData.Reference := Self;
end;

{ TPROutlineEntry }
function TPROutlineEntry.GetParent: TPROutlineEntry;
begin
  if FData.Parent <> nil then
    Result := TPROutlineEntry(FData.Parent.Reference)
  else
    Result := nil;
end;

function TPROutlineEntry.GetNext: TPROutlineEntry;
begin
  if FData.Next <> nil then
    Result := TPROutlineEntry(FData.Next.Reference)
  else
    Result := nil;
end;

function TPROutlineEntry.GetPrev: TPROutlineEntry;
begin
  if FData.Prev <> nil then
    Result := TPROutlineEntry(FData.Prev.Reference)
  else
    Result := nil;
end;

function TPROutlineEntry.GetFirst: TPROutlineEntry;
begin
  if FData.First <> nil then
    Result := TPROutlineEntry(FData.First.Reference)
  else
    Result := nil;
end;

function TPROutlineEntry.GetLast: TPROutlineEntry;
begin
  if FData.Last <> nil then
    Result := TPROutlineEntry(FData.Last.Reference)
  else
    Result := nil;
end;

function TPROutlineEntry.GetDest: TPRDestination;
begin
  if FData.Dest <> nil then
    Result := TPRDestination(FData.Dest.Reference)
  else
    Result := nil;
end;

function TPROutlineEntry.GetTitle: string;
begin
  Result := FData.Title;
end;

function TPROutlineEntry.GetOpened: boolean;
begin
  Result := FData.Opened;
end;

procedure TPROutlineEntry.SetDest(Value: TPRDestination);
begin
  if FData.Doc <> Value.FData.Doc then
    raise EPdfInvalidOperation.Create('SetDest --internal docs are not equal.');
  FData.Dest := Value.FData;
end;

procedure TPROutlineEntry.SetTitle(Value: string);
begin
  FData.Title := Value;
end;

procedure TPROutlineEntry.SetOpened(Value: boolean);
begin
  FData.Opened := Value;
end;

function TPROutlineEntry.AddChild: TPROutlineEntry;
begin
  Result := TPROutlineEntry.Create;
  Result.FData := Self.FData.AddChild;
  Result.FData.Reference := Result;
end;

{ TPROutlineRoot }
constructor TPROutlineRoot.CreateRoot(ADoc: TPdfDoc);
begin
  inherited Create;
  FData := ADoc.OutlineRoot;
  ADoc.OutlineRoot.Reference := Self;
end;

{ TPRPolygon }

procedure TPRPolygon.Print(prCanvas: TPRCanvas; ARect: TRect);
var
  ACanvas: TPDFCanvas;
  i,h: Integer;
  Pts: TPRPointArray;
begin
  if Length(Points)<2 then
    exit;

  h := GetPage.Height;

  SetLength(Pts, Length(Points));
  for i:=0 to Length(Points)-1 do
  begin
    Pts[i].x := Points[i].x;
    Pts[i].y := h - Points[i].y;
  end;

  ACanvas := prCanvas.PDFCanvas;

  SetDash(ACanvas, FLineStyle);

  ACanvas.MoveTo(Pts[0].x, Pts[0].y);
  for i:=1 to Length(Pts)-1 do
    ACanvas.LineTo(Pts[i].x, Pts[i].y);

  StdFillOrStroke(ACanvas);
end;

function TPRPolygon.IsFillable: boolean;
begin
  result := (Length(Points)>2);
end;

end.




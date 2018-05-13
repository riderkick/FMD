unit uEpub;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TStreams = specialize TFPGList<TStream>;

  TEpubBuilder = class
  private
    FTitle, FUuid: String;
    FMimeType, FContainer, FStyle, FContent, FToc: TStringStream;
    FImageNames: TStringList;
    FPages: TStreams;
    function CreateXHTMLPage(const imageName, pageTitle: String): TStringStream;
    function CreateContent: TStringStream;
    function CreateToc: TStringStream;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddImage(const path: String);
    procedure SaveToStream(const stream: TStream);
    property Title: String read FTitle write FTitle;
  end;

implementation

uses Zipper, htmlelements, uBaseUnit;

const
  CONTAINER: String =
    '<?xml version="1.0" encoding="UTF-8" ?>' + LineEnding +
    '<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">' + LineEnding +
    '  <rootfiles>' + LineEnding +
    '    <rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/>' + LineEnding +
    '  </rootfiles>' + LineEnding +
    '</container>' + LineEnding;

  CONTENT: String =
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<package xmlns="http://www.idpf.org/2007/opf" xmlns:dc="http://purl.org/dc/elements/1.1/" ' +
      'xmlns:opf="http://www.idpf.org/2007/opf" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
      'xmlns:dcterms="http://purl.org/dc/terms/" unique-identifier="bookid" version="2.0">' + LineEnding +
    '  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:opf="http://www.idpf.org/2007/opf">' + LineEnding +
    '    <dc:title>%s</dc:title>' + LineEnding +
    '    <dc:language>und</dc:language>' + LineEnding +
    '    <dc:identifier id="bookid" opf:scheme="UUID">urn:uuid:%s</dc:identifier>' + LineEnding +
    '  </metadata>' + LineEnding +
    '  <manifest>' + LineEnding +
    '    <item id="_toc" href="toc.ncx" media-type="application/x-dtbncx+xml"/>' + LineEnding +
    '    <item id="_style" href="style.css" media-type="text/css"/>' + LineEnding +
    '%s' + LineEnding +
    '  </manifest>' + LineEnding +
    '  <spine toc="_toc">'  + LineEnding +
    '%s' + LineEnding +
    '  </spine>' + LineEnding +
    '</package>';

  CONTENT_ITEM: String =
    '    <item id="%s" href="%s" media-type="%s"/>' + LineEnding;

  CONTENT_ITEMREF: String =
    '    <itemref idref="%s"/>' + LineEnding;

  TOC: String =
    '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
    '<ncx xmlns="http://www.daisy.org/z3986/2005/ncx/" version="2005-1">' + LineEnding +
    '  <head>' + LineEnding +
    '    <meta name="dtb:uid" content="urn:uuid:%s"/>' + LineEnding +
    '  </head>' + LineEnding +
    '  <docTitle>' + LineEnding +
    '    <text>%s</text>' + LineEnding +
    '  </docTitle>' + LineEnding +
    '  <navMap>' + LineEnding +
    '%s' + LineEnding +
    '  </navMap>' + LineEnding +
    '</ncx>' + LineEnding;

  NAV_POINT: String =
    '    <navPoint id="toc_%d" playOrder="%d">' + LineEnding +
    '      <navLabel>' + LineEnding +
    '        <text>Page %d</text>' + LineEnding +
    '      </navLabel>' + LineEnding +
    '      <content src="%s"/>' + LineEnding +
    '    </navPoint>' + LineEnding;

  PAGE: String =
    '<?xml version="1.0" encoding="utf-8"?>' + LineEnding +
    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">' + LineEnding +
    '<html xmlns="http://www.w3.org/1999/xhtml">' + LineEnding +
    '<head>' + LineEnding +
    '  <link href="style.css" rel="stylesheet" type="text/css"/>' + LineEnding +
    '  <title>%s</title>' + LineEnding +
    '</head>' + LineEnding +
    '<body>' + LineEnding +
    '  <div><img src="images/%s"/></div>' + LineEnding +
    '</body>' + LineEnding +
    '</html>' + LineEnding;

  STYLE: String =
    'body {' + LineEnding +
    '}' + LineEnding;

function NewUUID: String;
var
  guid: TGuid;
begin
  CreateGUID(guid);
  SetLength(Result, 36);
  StrLFmt(PChar(Result), 36, '%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',
    [
     Longint(GUID.D1), GUID.D2, GUID.D3,
     GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
     GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]
    ]);
  Result := LowerCase(Result);
end;

function GetImageName(const index: Integer; const fileName: String): String;
begin
  Result := Format('%.4d%s', [index+1, ExtractFileExt(fileName)]);
end;

function TEpubBuilder.CreateXHTMLPage(const imageName, pageTitle: String): TStringStream;
begin
  Result := TStringStream.Create(Format(PAGE, [EscapeHTML(pageTitle), imageName]));
end;

function TEpubBuilder.CreateContent: TStringStream;
var
  pageId, imageId, imageName, pageName: String;
  items: String = '';
  refs: String = '';
  i: Integer;
begin
  for i := 0 to FImageNames.Count-1 do begin
    pageId := Format('page%.4d', [i+1]);
    imageId := Format('image%.4d', [i+1]);
    imageName := 'images/' + GetImageName(i, FImageNames[i]);
    pageName := Format('%.4d.xhtml', [i+1]);
    items += Format(CONTENT_ITEM, [imageId, imageName, GetMimeType(FImageNames[i])]);
    items += Format(CONTENT_ITEM, [pageId, pageName, 'application/xhtml+xml']);
    refs += Format(CONTENT_ITEMREF, [pageId]);
  end;
  Result := TStringStream.Create(Format(CONTENT, [EscapeHTML(Title), FUuid, items, refs]));
end;

function TEpubBuilder.CreateToc: TStringStream;
var
  navPoints: String = '';
  i: Integer;
begin
  for i := 0 to FImageNames.Count-1 do begin
    navPoints += Format(NAV_POINT, [i+1, i+1, i+1, Format('%.4d.xhtml', [i+1])]);
  end;
  Result := TStringStream.Create(Format(TOC, [FUuid, Title, navPoints]));
end;

procedure TEpubBuilder.AddImage(const path: String);
begin
  FImageNames.Add(path);
end;

procedure TEpubBuilder.SaveToStream(const stream: TStream);
var
  zip: TZipper;
  i: Integer;
  tmp: TStream;
  imageName: String;
begin
  zip := TZipper.Create;
  try
    // add mimetype
    FMimeType := TStringStream.Create('application/epub+zip');
    zip.Entries.AddFileEntry(FMimeType, 'mimetype');

    // add META-INF/container.xml
    FContainer := TStringStream.Create(CONTAINER);
    zip.Entries.AddFileEntry(FContainer, 'META-INF/container.xml');

    // add css
    FStyle := TStringStream.Create(STYLE);
    zip.Entries.AddFileEntry(FStyle, 'OEBPS/style.css');

    for i := 0 to FImageNames.Count-1 do begin
      // add image file
      imageName := GetImageName(i, FImageNames[i]);
      zip.Entries.AddFileEntry(FImageNames[i], 'OEBPS/images/' + imageName);

      // add xhtml file
      tmp := CreateXHTMLPage(imageName, Format('%s - %.4d', [Title, i+1]));
      zip.Entries.AddFileEntry(tmp, Format('OEBPS/%.4d.xhtml', [i+1]));
      FPages.Add(tmp);
    end;

    // add content.opf
    FContent := CreateContent();
    zip.Entries.AddFileEntry(FContent, 'OEBPS/content.opf');

    // add toc.ncx
    FToc := CreateToc();
    zip.Entries.AddFileEntry(FToc, 'OEBPS/toc.ncx');

    zip.SaveToStream(stream);
  finally
    zip.Free;
  end;
end;

constructor TEpubBuilder.Create;
begin
  FMimeType := nil;
  FContainer := nil;
  FStyle := nil;
  FContent := nil;
  FToc := nil;
  FImageNames := TStringList.Create;
  FPages := TStreams.Create;
  FUuid := NewUUID;
end;

destructor TEpubBuilder.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPages.Count-1 do
    FPages[i].Free;
  FreeAndNil(FPages);
  FreeAndNil(FMimeType);
  FreeAndNil(FContainer);
  FreeAndNil(FImageNames);
  FreeAndNil(FStyle);
  FreeAndNil(FContent);
  FreeAndNil(FToc);
  inherited;
end;

end.


function GetNameAndLink()
  if http.GET(module.RootURL .. '/changeMangaList?type=text') then
    x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//li/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function GetInfo();
  mangainfo.url = MaybeFillHost(module.RootURL, url)
  if http.GET(mangainfo.url) then
    x = TXQuery.Create(http.Document)
    mangainfo.coverLink = MaybeFillHost(module.RootURL, x.XPathString('//div[@class="boxed"]/img/@src'))
    if mangainfo.title == '' then 
      mangainfo.title = x.XPathString('//h2[@class="widget-title"]')
    end
    if module.Website == 'MangaDenizi' then
      mangainfo.status = MangaInfoStatusIfPos(x.XPathString('//dt[.="Durum:"]/following-sibling::dd[1]'), 'Devam Ediyor', 'Tamamlandı')
    else
      mangainfo.status = MangaInfoStatusIfPos(x.XPathString('//dt[.=("Status","Estado")]/following-sibling::dd[1]'))
    end
    mangainfo.authors = x.XPathStringAll('//dt[.=("Author(s)","Yazar & Çizer:","Autor(es)")]/following-sibling::dd[1]/string-join(*,", ")')
    mangainfo.artists = x.XPathStringAll('//dt[.="Artist(s)"]/following-sibling::dd[1]/string-join(*,", ")')
    mangainfo.genres = x.XPathStringAll('//dt[.=("Categories","Kategoriler:","Categorías")]/following-sibling::dd[1]/string-join(*,", ")')
    mangainfo.summary = x.XPathString('//div[@class="well"]/p')
    v = x.Xpath('//ul[@class="chapters"]/li/*[self::h5 or self::h3]')
    for i = 1, v.Count do
      v2 = v.Get(i)
      mangainfo.chapterLinks.Add(x.XPathString('a/@href', v2))
      mangainfo.chapterNames.Add(x.XPathString('normalize-space(.)', v2))
    end
    InvertStrings(mangainfo.chapterLinks, mangainfo.chapterNames)
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  if http.GET(MaybeFillHost(module.RootURL, url)) then
    x = TXQuery.Create(http.Document)
    x.XPathStringAll('//div[@id="all"]/img/@data-src', task.PageLinks)
    if task.PageLinks.Count == 0 then
      x.XPathStringAll('//div[@id="all"]/img/@src', task.PageLinks)
    end
    return true
  else
    return false
  end
end

function AddWebsiteModule(name, url, cat)
  local m = NewModule()
  m.category = cat
  m.Website = name
  m.RootURL = url
  m.LastUpdated = 'February 12, 2018'
  m.OnGetNameAndLink = 'GetNameAndLink'
  m.OnGetInfo = 'GetInfo'
  m.OnGetPageNumber = 'GetPageNumber'
  return m
end

function Init()
  AddWebsiteModule('Komikid', 'http://www.komikid.com', 'Indonesian');
  AddWebsiteModule('MangaDenizi', 'http://www.mangadenizi.com', 'Turkish');
  AddWebsiteModule('MangaDoor', 'http://mangadoor.com', 'Spanish');
  AddWebsiteModule('MangaID', 'http://mangaid.co', 'Indonesian');
  AddWebsiteModule('FallenAngelsScans','http://manga.fascans.com', 'English-Scanlation');
  AddWebsiteModule('MangaDesu','http://mangadesu.net', 'Indonesian');
  AddWebsiteModule('ChibiManga','http://www.cmreader.info', 'English-Scanlation');
end

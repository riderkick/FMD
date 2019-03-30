Modules = {}

function Modules.myReaderMangaCMS()
  local myReaderMangaCMS = {}
  
  function myReaderMangaCMS:new()
    local obj = {}
    setmetatable(obj, self)
    self.__index = self
    return obj
  end
  
  function myReaderMangaCMS:getinfo()
    mangainfo.url = MaybeFillHost(module.RootURL, url)
    if http.GET(mangainfo.url) then
      local x = TXQuery.Create(http.Document)
      mangainfo.coverLink = MaybeFillHost(module.RootURL, x.XPathString('//div[@class="boxed"]/img/@src'))
      if mangainfo.title == '' then 
        mangainfo.title = x.XPathString('//*[(self::h2 or self::h1) and contains(@class,"widget-title")]')
        if mangainfo.title == '' then
          mangainfo.title = mangainfo.url:match('/([^/]+)$')
        end
      end
      mangainfo.status = MangaInfoStatusIfPos(x.XPathString('//dt[.=("Status","Estado","Statut")]/following-sibling::dd[1]'))
      mangainfo.authors = x.XPathStringAll('//dt[.=("Author(s)","Yazar & Çizer:","Autor(es)","Auteur(s)")]/following-sibling::dd[1]/string-join(*,", ")')
      mangainfo.artists = x.XPathStringAll('//dt[.=("Artist(s)","Artiste(s)")]/following-sibling::dd[1]/string-join(*,", ")')
      mangainfo.genres = x.XPathStringAll('//dt[.=("Categories","Kategoriler:","Categorías","Catégories")]/following-sibling::dd[1]/string-join(*,", ")')
      mangainfo.summary = x.XPathString('//div[@class="well"]/p')
      local v = x.Xpath('//ul[@class="chapters"]/li/*[self::h5 or self::h3]')
      for i = 1, v.Count do
        local v2 = v.Get(i)
        mangainfo.chapterLinks.Add(x.XPathString('a/@href', v2))
        mangainfo.chapterNames.Add(x.XPathString('normalize-space(.)', v2))
      end
      InvertStrings(mangainfo.chapterLinks, mangainfo.chapterNames)
      return no_error
    end
    return net_problem
  end
  
  function myReaderMangaCMS:getpagenumber()
    local u = MaybeFillHost(module.RootURL, url)
    if http.get(u) then
      local x = TXQuery.Create(http.document)
      x.xpathstringall('//div[@id="all"]/img/@data-src', task.pagelinks)
      if task.pagelinks.Count == 0 then
        x.xpathstringall('//div[@id="all"]/img/@src', task.pagelinks)
      end
      task.pagecontainerlinks.text = u
      return true
    end
    return false
  end
  
  function myReaderMangaCMS:getnameandlink()
    if http.get(module.rooturl .. self.getdirurl()) then
      local x = TXQuery.create(http.document)
      x.xpathhrefall('//li/a', links, names)
      return no_error
    end
    return net_problem
  end
  
  function myReaderMangaCMS:getdirurl()
    return '/changeMangaList?type=text'
  end
  
  function myReaderMangaCMS:beforedownloadimage()
    http.reset()
    http.headers.values['Referer'] = task.pagecontainerlinks.text
    return true
  end
  
  return myReaderMangaCMS
end

function Modules.WhiteCloudPavilion()
  local WhiteCloudPavilion = {}
  setmetatable(WhiteCloudPavilion, { __index = Modules.myReaderMangaCMS() })
  
  function WhiteCloudPavilion:getdirurl()
    return '/manga/free' .. Modules.myReaderMangaCMS().getdirurl()
  end
  
  return WhiteCloudPavilion
end

function Modules.GodsRealmScan()
  local GodsRealmScan = {}
  setmetatable(GodsRealmScan, { __index = Modules.myReaderMangaCMS() })
  
  function GodsRealmScan:getdirurl()
    return '/public' .. Modules.myReaderMangaCMS().getdirurl()
  end
  
  return GodsRealmScan
end

function Modules.MangaDenizi()
  local MangaDenizi = {}
  setmetatable(MangaDenizi, { __index = Modules.myReaderMangaCMS() })
  
  function MangaDenizi:getinfo()
    Modules.myReaderMangaCMS().getinfo()
    local x=TXQuery.Create(http.document)
    mangainfo.status = MangaInfoStatusIfPos(x.XPathString('//dt[.="Durum:"]/following-sibling::dd[1]'), 'Devam Ediyor', 'Tamamlandı')
  end
  
  return MangaDenizi
end

function Modules.FallenAngelsScans()
  local FallenAngelsScans = {}
  setmetatable(FallenAngelsScans, { __index = Modules.myReaderMangaCMS() })
  
  function FallenAngelsScans:getinfo()
    Modules.myReaderMangaCMS().getinfo()
    if mangainfo.coverLink:match('^//') ~= nil then
      mangainfo.coverLink = 'https:' .. mangainfo.coverLink
    end
  end
  
  return FallenAngelsScans
end

function Modules.KomikGue()
  local KomikGue = {}
  setmetatable(KomikGue, { __index = Modules.myReaderMangaCMS() })
  
  function KomikGue:getinfo()
    Modules.myReaderMangaCMS().getinfo()
    local x=TXQuery.Create(http.document)
    mangainfo.artists = x.XPathStringAll('//dt[.="Artist(s)"]/following-sibling::dd[1]')
    mangainfo.summary = x.XPathString('//div[@class="well"]/div')
    local v = x.xpath('//div[@class="chapter-wrapper"]/table//td[@class="chapter"]/a')
    for i = 1, v.Count do
      local v2 = v.Get(i)
      mangainfo.chapterLinks.Add(v2.getAttribute('href'))
      mangainfo.chapterNames.Add(x.XPathString('normalize-space(.)', v2))
    end
    InvertStrings(mangainfo.chapterLinks, mangainfo.chapterNames)
  end
  
  return KomikGue
end

function Modules.ScanFR()
  local ScanFR = {}
  setmetatable(ScanFR, { __index = Modules.myReaderMangaCMS() })
  
  function ScanFR:getinfo()
    Modules.myReaderMangaCMS().getinfo()
    local x=TXQuery.Create(http.document)
	local v = x.xpath('//div[@class="col-lg-12"]//ul[@class="chapterszz"]/li/h5/a')
	for i = 1, v.Count do
      local v2 = v.Get(i)
      mangainfo.chapterLinks.Add(v2.getAttribute('href'))
      mangainfo.chapterNames.Add(x.XPathString('normalize-space(.)', v2))
    end
  end
  
  return ScanFR
end


-------------------------------------------------------------------------------

function createInstance()
  local m = Modules[module.website]
  if m ~= nil then
    return m():new()
  else
    return Modules.myReaderMangaCMS():new()
  end
end

-------------------------------------------------------------------------------

function getinfo()
  return createInstance():getinfo()
end

function getpagenumber()
  return createInstance():getpagenumber()
end

function getnameandlink()
  return createInstance():getnameandlink()
end

function beforedownloadimage()
  return createInstance():beforedownloadimage()
end

function AddWebsiteModule(name, url, cat)
  local m = NewModule()
  m.category = cat
  m.website = name
  m.rooturl = url
  m.lastupdated='March 30, 2019'
  m.ongetnameandlink = 'getnameandlink'
  m.ongetinfo = 'getinfo'
  m.ongetpagenumber = 'getpagenumber'
  m.onbeforedownloadimage='beforedownloadimage'
  return m
end

function Init()
  local c='Spanish'
  AddWebsiteModule('MangaDoor', 'http://mangadoor.com', c);
  AddWebsiteModule('MangAs', 'https://mang.as', c);
  
  c='Indonesian'
  AddWebsiteModule('Komikid', 'https://www.komikid.com', c);
  AddWebsiteModule('KomikGue', 'https://www.komikgue.com', c);
  
  c='Raw'
  AddWebsiteModule('RawMangaUpdate', 'http://rawmangaupdate.com', c);
  AddWebsiteModule('MangaRawOnline', 'http://mangaraw.online', c);
  AddWebsiteModule('RawMangaSite', 'https://rawmanga.site', c);
  
  c='Turkish'
  AddWebsiteModule('MangaDenizi', 'http://www.mangadenizi.com', c);
  AddWebsiteModule('MangaVadisi', 'http://manga-v2.mangavadisi.org', c);

  c='English-Scanlation'
  AddWebsiteModule('FallenAngelsScans','https://manga.fascans.com', c);
  AddWebsiteModule('WhiteCloudPavilion','https://whitecloudpavilion.com', c);
  AddWebsiteModule('HatigarmScans', 'https://www.hatigarmscans.net', c)
  AddWebsiteModule('WoweScans', 'https://wowescans.net', c)

  c='Spanish-Scanlation'
  AddWebsiteModule('DarkSkyScan', 'https://darkskyprojects.org', c);
  AddWebsiteModule('CoYuHi', 'http://www.universoyuri.com', c);
  AddWebsiteModule('SOSScanlation', 'https://sosscanlation.com', c);
  
  c='French'
  AddWebsiteModule('ScanFR', 'https://www.scan-fr.io', c);
  AddWebsiteModule('ScanOP', 'http://www.scan-op.com', c);
end

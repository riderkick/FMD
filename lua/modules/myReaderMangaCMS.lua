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
  m.ongetnameandlink = 'getnameandlink'
  m.ongetinfo = 'getinfo'
  m.ongetpagenumber = 'getpagenumber'
  m.onbeforedownloadimage='beforedownloadimage'
  return m
end

function Init()
  local c='Raw'
  AddWebsiteModule('MangaRawOnline', 'http://mangaraw.online', c);
  AddWebsiteModule('RawMangaSite', 'https://rawmanga.site', c);

  c='Spanish-Scanlation'
  AddWebsiteModule('SOSScanlation', 'https://sosscanlation.com', c);
  AddWebsiteModule('SamuraiScan', 'https://samuraiscan.com', c);
end

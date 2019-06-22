Modules = {}

function Modules.MangaShiroBase()
  local MangaShiroBase = {}

  function MangaShiroBase:new()
    local obj = {}
    setmetatable(obj, self)
    self.__index = self
    return obj
  end

  function MangaShiroBase:getinfo()
    mangainfo.url=MaybeFillHost(module.RootURL, url)
    if http.get(mangainfo.url) then
      local x=TXQuery.Create(http.document)
      if mangainfo.title == '' then
        mangainfo.title=x.xpathstring('//h1[@itemprop="name"]')
        if mangainfo.title == '' then
          mangainfo.title=x.xpathstring('//h1')
        end
        mangainfo.title = mangainfo.title:gsub('Bahasa Indonesia$', '')
      end
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="imgdesc"]/img/@src'))
      mangainfo.authors=x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Author")]/substring-after(.,":")')
      mangainfo.genres=x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Genre")]/substring-after(.,":")')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Status")]'))
      mangainfo.summary=x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")')
      x.xpathhrefall('//div[@class="cl"]//li/span[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
      InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
      return no_error
    end
    return net_problem
  end

  function MangaShiroBase:getpagenumber()
    task.pagenumber=0
    task.pagelinks.clear()
    if http.get(MaybeFillHost(module.rooturl,url)) then
      TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]//img/@data-lazy-src', task.pagelinks)
      if task.pagelinks.count < 1 then
        TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]//img/@src', task.pagelinks)
      end
      return true
    end
  end

  function MangaShiroBase:getnameandlink()
    if http.get(module.rooturl .. self.getdirurl()) then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="soralist"]//a', links, names)
      return no_error
    end
    return net_problem
  end

  function MangaShiroBase:getdirurl()
    return '/manga-list/'
  end

  return MangaShiroBase;
end

function Modules.KomikCast()
  local KomikCast = {}
  setmetatable(KomikCast, { __index = Modules.MangaShiroBase() })

  function KomikCast:getdirurl()
    return '/list/'
  end

  function KomikCast:getinfo()
    mangainfo.url=MaybeFillHost(module.RootURL, url)
    if http.get(mangainfo.url) then
      local x=TXQuery.Create(http.document)
      if mangainfo.title == '' then
        mangainfo.title=x.xpathstring('//div[@class="mangainfo"]/h1')
      end
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="topinfo"]/img/@src'))
      mangainfo.authors=x.xpathstring('//div[@class="topinfo"]//tr[contains(th, "Author")]/td')
      mangainfo.genres=x.xpathstringall('//div[@class="topinfo"]//tr[contains(th, "Genres")]/td/a')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="topinfo"]//tr[contains(th, "Status")]/td'))
      mangainfo.summary=x.xpathstringall('//*[@class="sin"]/p/text()', '')
      x.xpathhrefall('//div[@class="cl"]//li/span[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
      InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
      return no_error
    end
    return net_problem
  end

  function KomikCast:getnameandlink()
    if http.get(module.rooturl .. self.getdirurl()) then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="jdlbar"]//a',links,names)
      return no_error
    end
    return net_problem
  end

  return KomikCast
end

function Modules.WestManga()
  local WestManga = {}
  setmetatable(WestManga, { __index = Modules.KomikCast() })

  function WestManga:getdirurl()
    return '/manga-list/?list'
  end

  return WestManga
end

function Modules.MangaShiro()
  local MangaShiro = {}
  setmetatable(MangaShiro, { __index = Modules.MangaShiroBase() })

  function MangaShiro:getinfo()
    mangainfo.url=MaybeFillHost(module.RootURL, url)
    if http.get(mangainfo.url) then
      local x=TXQuery.Create(http.document)
      if mangainfo.title == '' then
        mangainfo.title=x.xpathstring('//h1[@itemprop="headline"]')
        mangainfo.title=mangainfo.title:gsub('Bahasa Indonesia$', '')
      end
      local img = x.xpathstring('//div[@itemprop="image"]/img/@data-lazy-src')
      if img == '' then
        img = x.xpathstring('//div[@itemprop="image"]/img/@src')
      end
      mangainfo.coverlink=MaybeFillHost(module.RootURL, img)
      mangainfo.authors=x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Author")]/substring-after(.,":")')
      mangainfo.genres=x.xpathstringall('//div[contains(@class,"animeinfo")]/div[@class="gnr"]/a')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Status")]'))
      mangainfo.summary=x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")')
      x.xpathhrefall('//div[@class="bxcl"]//li//div[@class="lch"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
      InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
      return no_error
    end
    return net_problem
  end

  function MangaShiro:getdirurl()
    return '/daftar-manga/?list'
  end
  
  return MangaShiro
end

function Modules.Kiryuu()
  local Kiryuu = {}
  setmetatable(Kiryuu, { __index = Modules.MangaShiro() })

  function Kiryuu:getdirurl()
    return '/manga-lists/?list'
  end

  return Kiryuu
end

function Modules.MangaIndoNet()
  local MangaIndoNet = {}
  setmetatable(MangaIndoNet, { __index = Modules.MangaShiro() })

  function MangaIndoNet:getdirurl()
    return '/manga-list/?list'
  end

  return MangaIndoNet
end

function Modules.KomikIndo()
  local KomikIndo = {}
  setmetatable(KomikIndo, { __index = Modules.MangaShiro() })

  function KomikIndo:getdirurl()
    return '/manga-list/?list'
  end

  return KomikIndo
end

function Modules.PecintaKomik()
  local PecintaKomik = {}
  setmetatable(PecintaKomik, { __index = Modules.MangaShiroBase() })

  function PecintaKomik:getinfo()
    mangainfo.url=MaybeFillHost(module.RootURL, url)
    if http.get(mangainfo.url) then
      local x=TXQuery.Create(http.document)
      if mangainfo.title == '' then
        mangainfo.title=x.xpathstring('//h1[@itemprop="headline"]/*')
      end
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@itemprop="image"]/img/@src'))
      mangainfo.authors=x.xpathstring('//table[@class="listinfo"]//tr[contains(th, "Penulis")]/td')
      mangainfo.genres=x.xpathstringall('//table[@class="listinfo"]//tr[contains(th, "Genre")]/td/a')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//table[@class="listinfo"]//tr[contains(th, "Status")]/td'))
      mangainfo.summary=x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")')
      x.xpathhrefall('//div[@class="bxcl"]//li//*[@class="lchx"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
      InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
      return no_error
    end
    return net_problem
  end

  function PecintaKomik:getdirurl()
    return '/daftar-manga/?list'
  end

  return PecintaKomik
end

function Modules.MangaKita()
  local MangaKita = {}
  setmetatable(MangaKita, { __index = Modules.MangaShiroBase() })

  function MangaKita:getinfo()
    mangainfo.url=MaybeFillHost(module.RootURL, url)
    if http.get(mangainfo.url) then
      local x=TXQuery.Create(http.document)
      if mangainfo.title == '' then
        mangainfo.title=x.xpathstring('//h1')
      end
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[contains(@class,"leftImage")]/img/@src'))
      mangainfo.authors=x.xpathstring('//span[@class="details"]//div[starts-with(.,"Author")]/substring-after(.,":")')
      mangainfo.genres=x.xpathstringall('//span[@class="details"]//div[starts-with(.,"Genre")]/a')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//span[@class="details"]//div[starts-with(.,"Status")]'))
      mangainfo.summary=x.xpathstringall('//*[@class="description"]/text()', '')
      local v = x.xpath('//div[contains(@class, "chapter-list")]/a')
      for i = 1, v.count do
        local v1 = v.get(i)
        mangainfo.chapterlinks.add(v1.getAttribute('href'))
        mangainfo.chapternames.add(x.xpathstring('./span', v1))
      end
      InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
      return no_error
    end
    return net_problem
  end

  function MangaKita:getnameandlink()
    if http.get(module.rooturl .. self.getdirurl()) then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="jdlbar"]//a',links,names)
      return no_error
    end
    return net_problem
  end

  return MangaKita
end

function Modules.KomikStation()
  local KomikStation = {}
  setmetatable(KomikStation, { __index = Modules.MangaShiroBase() })

  function KomikStation:getnameandlink()
    if http.get(module.rooturl .. self.getdirurl()) then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="daftarkomik"]//a',links,names)
      return no_error
    end
    return net_problem
  end

  function KomikStation:getdirurl()
    return '/daftar-komik/'
  end

  return KomikStation
end

function Modules.MangaKid()
  local MangaKid = {}
  setmetatable(MangaKid, { __index = Modules.MangaShiroBase() })

  function MangaKid:getdirurl()
    return '/manga-lists/'
  end

  return MangaKid
end

function Modules.MangaID()
  local MangaID = {}
  setmetatable(MangaID, { __index = Modules.PecintaKomik() })
  
  function MangaID:getinfo()
    Modules.PecintaKomik().getinfo()
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title=x.xpathstring('//h1[@itemprop="headline"]')
      mangainfo.title=mangainfo.title:gsub('Bahasa Indonesia$', '')
    end
    mangainfo.authors=x.xpathstring('//table[@class="listinfo"]//tr[contains(th, "Author")]/td')
  end

  function MangaID:getdirurl()
    return '/daftar-manga/?list'
  end

  return MangaID
end

function Modules.OtakuFile()
  local OtakuFile = {}
  setmetatable(OtakuFile, { __index = Modules.MangaShiroBase() })
  
  function OtakuFile:getinfo()
    Modules.MangaShiroBase().getinfo()
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//h1[@itemprop="name"]')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="imgdesc"]/a/img/@src'))
    x.xpathhrefall('//div[@class="epl"]//li/span[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
  end
  
  function OtakuFile:getpagenumber()
    task.pagenumber=0
    task.pagelinks.clear()
    if http.get(MaybeFillHost(module.rooturl,url)) then
      TXQuery.Create(http.Document).xpathstringall('//*[@id="wrap"]/p//img/@src', task.pagelinks)
      return true
    end
  end
  
  function OtakuFile:getnameandlink()
    if http.get(module.rooturl .. self.getdirurl()) then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="anilist-diatur"]//a',links,names)
      return no_error
    end
    return net_problem
  end

  function OtakuFile:getdirurl()
    return '/daftar-komik/'
  end

  return OtakuFile
end

-------------------------------------------------------------------------------

function createInstance()
  local m = Modules[module.website]
  if m ~= nil then
    return m():new()
  else
    return Modules.MangaShiroBase():new()
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

function AddWebsiteModule(site, url)
  local m=NewModule()
  m.category='Indonesian'
  m.website=site
  m.rooturl=url
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  return m
end

function Init()
  AddWebsiteModule('MangaShiro', 'https://mangashiro.net')
  AddWebsiteModule('MangaKita', 'http://www.mangakita.net')
  AddWebsiteModule('KomikStation', 'https://www.komikstation.com')
  AddWebsiteModule('MangaKid', 'http://mgku.net')
  AddWebsiteModule('KomikCast', 'https://komikcast.com')
  AddWebsiteModule('WestManga', 'https://westmanga.info')
  AddWebsiteModule('Kiryuu', 'https://kiryuu.co')
  AddWebsiteModule('KomikOtaku', 'https://komikotaku.net')
  AddWebsiteModule('PecintaKomik', 'https://www.pecintakomik.com')
  AddWebsiteModule('MangaIndoNet', 'https://mangaindo.net')
  AddWebsiteModule('KomikIndo', 'https://komikindo.co')
  AddWebsiteModule('MangaID', 'https://mangaid.me')
  AddWebsiteModule('OtakuFile', 'https://otakufile.com')
end

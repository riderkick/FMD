function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.Document)
    mangainfo.title=x.XPathString('//div[@id="main"]//h1')
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('//div[@id="main"]//div[@class="description"]/img/@src'))
    mangainfo.summary = x.XPathString('//div[@id="main"]//div[@class="description"]/p')
    mangainfo.artists = x.XPathStringAll('//div[@id="main"]//div[@class="content" and contains(div[@class="header"], "Arte")]/div[@class="description"]/a')
    mangainfo.authors = x.XPathStringAll('//div[@id="main"]//div[@class="content" and contains(div[@class="header"], "Autor")]/div[@class="description"]/a')
    mangainfo.genres = x.XPathStringAll('//div[@id="main"]//div[@class="content" and contains(div[@class="header"], "Gênero")]/div[@class="description"]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@id="main"]//div[@class="content" and contains(div[@class="header"], "Status")]/div[@class="description"]/a'), 'Em publicação', 'Completo')
    x.XPathHREFAll('//table/tbody/tr/td/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterLinks, mangainfo.chapterNames)
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  task.pagelinks.clear()
  task.pagenumber=0
  if http.get(MaybeFillHost(module.rooturl,url)) then  
    local x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(., "urlSulfix")]')
    local suffix = GetBetween("var urlSulfix = '", "';", s)
    local pages = GetBetween("var pages =", ";", s)
    x.parsehtml(pages)
    local v = x.xpath('json(*)()');
    for i = 1, v.count do
      local v1=v.get(i)
      task.pagelinks.add(suffix .. v1.ToString .. '.jpg')
    end
    return true
  else
    return false
  end
end

function BeforeDownloadImage()
  http.headers.values['Referer'] = module.rooturl
  return true
end

function GetNameAndLink()
  if http.get(module.rooturl..'/titulos/filtro/*/p/'..IncStr(url)) then
    x=TXQuery.Create(http.Document)
    x.XPathHREFAll('//div[contains(@class, "list")]/div[contains(@class, "item")]/div[@class="content"]//a', links, names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. '/titulos') then
    x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('(//div[contains(@class, "grid")]/div/a[contains(@class, "button")])[last()]'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Portuguese'
  m.website='CentralDeMangas'
  m.rooturl='http://centraldemangas.online'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end 
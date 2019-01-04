function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//div[@class="info-title"]/h1')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[contains(@class, "info-img")]/img/@src'))
    mangainfo.authors=x.xpathstringall('//div[contains(b, "Author")]/span/a')
    mangainfo.artists=x.xpathstringall('//div[contains(b, "Artist")]/span/a')
    mangainfo.genres=x.xpathstringall('//div[contains(b, "Genre")]/span/a')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[contains(b, "Status")]/span/a'))
    mangainfo.summary=x.xpathstring('//h3[contains(.,"Summary")]/following-sibling::*')
    x.xpathhrefall('//div[@class="list-chapter"]/table//td[@class="name"]/a',mangainfo.chapterlinks,mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function beforedownloadimage()
  http.headers.values['Referer'] = module.rooturl
  return true
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//div[@class="img"]/img/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl .. '/directory.html?page=' .. IncStr(url)) then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//ul[@class="list_manga"]/li//a[@class="name-manga"]', links, names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. '/directory.html') then
    x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('//div[@class="pagination-ct"]/a[last()]/substring-after(@href,"=")'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'MangaHasu'
  m.rooturl = 'http://mangahasu.se'
  m.category = 'English'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.onbeforedownloadimage='beforedownloadimage'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
end
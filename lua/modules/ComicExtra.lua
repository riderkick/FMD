function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//h1[1]/span')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="movie-l-img"]/img/@src'))
    mangainfo.authors=x.xpathstring('//dt[contains(., "Author")]/following-sibling::dd')
    mangainfo.genres=x.xpathstringall('//dt[contains(., "Genre")]/following-sibling::dd/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//dt[contains(., "Status")]/following-sibling::dd'))
    mangainfo.summary=x.xpathstring('//div[@id="film-content"]')
    x.xpathhrefall('//tbody[@id="list"]/tr/td/a',mangainfo.chapterlinks,mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  if http.get(MaybeFillHost(module.rooturl, url .. '/full')) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//img[@class="chapter_img"]/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl .. '/comic-list') then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//div[@class="serie-box"]/ul/li/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'ComicExtra'
  m.rooturl = 'http://www.comicextra.com'
  m.category = 'English'
  m.lastupdated='April 26, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

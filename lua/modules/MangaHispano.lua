function getinfo()
  mangainfo.url = MaybeFillHost(module.RootURL, url)
  if http.GET(mangainfo.url) then
    x = TXQuery.Create(http.Document)
    mangainfo.coverLink = MaybeFillHost(module.RootURL, x.XPathString('//div[@class="boxed"]/img/@src'))
    if mangainfo.title == '' then 
      mangainfo.title = x.XPathString('//h1[contains(@class,"widget-title")]')
    end
    mangainfo.status = MangaInfoStatusIfPos(x.XPathString('//span[contains(., "Estado")]/span'), 'progres', 'final')
    mangainfo.authors = x.XPathStringAll('//span[contains(., "Autor")]/span')
    mangainfo.genres = x.XPathStringAll('//dd[@class="sintesis-cat"]/a')
    mangainfo.summary = x.XPathString('//div[@class="well"]/p')
    v = x.Xpath('//ul/li/*[self::h5 or self::h3 and contains(@class, "chapter-title")]')
    for i = 1, v.Count do
      v2 = v.Get(i)
      mangainfo.chapterLinks.Add(x.XPathString('a/@href', v2))
      mangainfo.chapterNames.Add(x.XPathString('a||": "||em', v2))
    end
    InvertStrings(mangainfo.chapterLinks, mangainfo.chapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    x = TXQuery.Create(http.Document)
    s = x.xpathstring('//script[contains(., "var pages")]')
    s = GetBetween('pages =', ';', s)
    x.parsehtml(s)
    x.XPathStringAll('json(*)().page_image', task.PageLinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl .. '/mangas') then
    local x = TXQuery.Create(http.Document)
    x.xpathhrefall('//div[@class="caption"]/h3/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.category = 'Spanish'
  m.Website = 'MangaHispano'
  m.RootURL = 'https://mangahis.com'
  m.lastupdated='June 14, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end
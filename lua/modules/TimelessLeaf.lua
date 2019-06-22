function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//h1[@class="entry-title"]')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//article//img/@src'))
    mangainfo.summary=x.xpathstringall('//div[@class="entry-content page-content"]/p[not(contains(., "Chapter"))]/text()', '')
    x.xpathhrefall('//div[@class="entry-content page-content"]/p/a',mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//figure[@class="gallery-item"]//img/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl .. '/comic-list') then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//a[.="Manga"]/following-sibling::ul//li/a[not(contains(@href,"more"))]', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'TimelessLeaf'
  m.rooturl = 'https://timelessleaf.com'
  m.category = 'English-Scanlation'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end
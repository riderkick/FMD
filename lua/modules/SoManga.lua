function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//h1')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[contains(@class, "manga")]//img/@src'))
    mangainfo.authors=x.xpathstringall('//div[contains(@class, "manga")]//h5[contains(*, "Autor")]/text()', '')
    mangainfo.artists=x.xpathstringall('//div[contains(@class, "manga")]//h5[contains(*, "Artista")]/text()', '')
    mangainfo.genres=x.xpathstringall('//div[contains(@class, "manga")]//h5[contains(*, "Genero")]/span')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[contains(@class, "manga")]//h5[contains(*, "Status")]/text()'), 'Ativo', 'Completo')
    mangainfo.summary=x.xpathstring('//div[contains(@class, "manga")]//div[contains(@style, "justify")]')
    local v=x.xpath('//ul[@class="capitulos"]/li/a')
    for i=1,v.count do
      local v1=v.get(i)
      mangainfo.chapterlinks.add(v1.getattribute('href'))
      mangainfo.chapternames.add(x.xpathstring('div/text()', v1))
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber=0
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//img[contains(@class, "img-manga")]/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl .. '/php/busca-mangas.php') then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//ul/li[@class="mangas"]/div/a', links, names)
    for i=1,v.count do
      local v1=v.get(i)
      links.add(v1.getattribute('href'))
      names.add(x.xpathstring('div/h3', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'SoManga'
  m.rooturl = 'http://somangas.net'
  m.category = 'Portuguese'
  m.lastupdated='March 1, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

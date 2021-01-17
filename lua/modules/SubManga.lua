function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = Trim(x.XPathString('//div[contains(@class, "col-md-3")]//h3/text()'))
    end
    mangainfo.coverlink = x.xpathstring('//div[contains(@class, "col-md-3")]//img[@class="img-responsive"]/@src')
    mangainfo.authors=Trim(x.xpathstring('//span[@class="list-group-item" and contains(., "Autor")]/a'))
    mangainfo.artists=Trim(x.xpathstring('//span[@class="list-group-item" and contains(., "Artist")]/a'))
    mangainfo.genres=Trim(x.xpathstringall('//span[@class="list-group-item" and contains(., "Categorías:")]/a'))
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//span[@class="list-group-item" and contains(., "Estado")]'))
    mangainfo.summary = x.xpathstringall('//span[@class="list-group-item" and contains(., "Resumen")]/text()', '')
    x.xpathhrefall('//table//tr/td[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    http.reset()
    http.headers.values['Referer'] = mangainfo.url
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagecontainerlinks.clear()
  local url = MaybeFillHost(module.rooturl, url);
  if http.get(url) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//*[@id="all"]/img/@data-src', task.pagelinks)
    task.pagecontainerlinks.text = url
  else
    return false
  end
  return true
end

function BeforeDownloadImage()
  http.headers.values['Referer'] = task.pagecontainerlinks.text
  return true
end

function getnameandlink()
  if http.GET(module.RootURL .. '/changeMangaList?type=text') then
    TXQuery.Create(http.Document).XPathHREFAll('//li/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'SubManga'
  m.rooturl = 'https://submangas.net'
  m.category = 'Spanish'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end

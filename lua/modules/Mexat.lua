function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.XPathString('//h1[@class="page-title"]')
    end
    mangainfo.summary = x.xpathstring('//div[@class="archive-meta"]')
    x.xpathhrefall('//div[@class="entry"]/table//tr/td[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagecontainerlinks.clear()
  task.pagenumber = 0
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//select[@id="manga_pid"]/option/@value', task.pagecontainerlinks)
    task.pagenumber = task.pagecontainerlinks.count
  else
    return false
  end
  return true
end

function getimageurl()
  if http.GET(AppendURLDelim(MaybeFillHost(module.RootURL, url)) .. '?pid=' .. task.pagecontainerlinks[workid]) then
    local x = TXQuery.Create(http.Document)        
    task.PageLinks[workid] = x.xpathstring('//div[@class="pic"]/a/img/@src')
    return true
  else
    return false
  end
end 

function getnameandlink()
  if http.GET(module.RootURL .. '/%D9%82%D8%A7%D8%A6%D9%85%D8%A9-%D8%A7%D9%84%D9%85%D8%A7%D9%86%D8%AC%D8%A7/') then
    TXQuery.Create(http.Document).XPathHREFAll('//ul[@class="MangaList"]/li//div[@class="SeriesName"]/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'Mexat'
  m.rooturl = 'http://manga.mexat.com'
  m.category = 'Arabic-Scanlation'
  m.lastupdated='June 23, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.OnGetImageURL = 'getimageurl'
end

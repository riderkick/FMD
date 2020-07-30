function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)	
    mangainfo.title=x.xpathstring('//h1/text()')
    mangainfo.coverlink=x.xpathstring('//main//img/@src')
    mangainfo.artists=x.xpathstringall('//main//table//tr[td="Artist:"]/td/a')
    mangainfo.authors=x.xpathstringall('//main//table//tr[td="Author:"]/td/a')
    mangainfo.genres=x.xpathstringall('//main//table//tr[td="Tags:"]/td/a')
    mangainfo.summary=x.xpathstring('//main//pre')
    x.xpathhrefall('//main//ul/li/span/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  if http.get(MaybeFillHost(module.rooturl,url..'/manifest.json')) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('json(*).readingOrder().href',task.pagelinks)
  else
    return false
  end
  return true
end

function getdirectorypagenumber()
  if http.get(module.rooturl..'/directory/') then
    page=tonumber(TXQuery.Create(http.document).xpathstring('//div[@class="pagination"]//a[contains(., "last")]/@href'):match('/(%d+)/'))
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  if http.get(module.rooturl..'/directory/'..IncStr(url)) then
    TXQuery.Create(http.document).xpathhrefall('//div[@class="mdc-card__media-title"]/a',links,names)
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = category
  m.ongetinfo = 'getinfo'
  m.OnGetPageNumber = 'getpagenumber'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
  m.OnGetNameAndLink = 'getnameandlink'
  return m
end

function Init()
  local cat = 'English-Scanlation'
  AddWebsiteModule('LetItGoScans', 'https://reader.letitgo.scans.today', cat)
  AddWebsiteModule('ProjectTime', 'https://read.ptscans.com', cat)
  AddWebsiteModule('WhimSubs', 'https://whimsubs.xyz', cat)
end

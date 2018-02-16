function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)	
    mangainfo.title=x.xpathstring('//*[@class="tag-title"]')
    mangainfo.coverlink=MaybeFillHost(module.rooturl,x.xpathstring('//img[@class="thumbnail"]/@src'))
    mangainfo.authors=x.xpathstring('//a[contains(@href,"/title/")]')
    mangainfo.genres=x.xpathstringall('//*[@class="label" or @class="doujin_tags"]')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//small'))
    mangainfo.summary=x.xpathstring('//*[@class="description"]')
    x.xpathhrefall('//dl[@class="chapter-list"]/dd/a',mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_error
  end
end

function getpagenumber()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    TXQuery.Create(http.Document).xpathstringall('json(//script[contains(.,"var pages")]/substring-after(substring-before(.,";")," = "))()/concat("'..module.rooturl..'",./image)',task.pagelinks)
    return true
  else
    return false
  end
  return true
end

local diruris={
    '/anthologies',
    '/doujins',
    '/issues',
    '/series'
    }

function getdirectorypagenumber()
  page=#diruris
  return no_error
end

function getnameandlink()
  print(diruris[tonumber(url)+1])
  if http.get(module.rooturl..diruris[tonumber(url)+1]) then
    TXQuery.Create(http.document).xpathhrefall('//dd/a',links,names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='English-Scanlation'
  m.website='DynastyScans'
  m.rooturl='https://dynasty-scans.com'
  m.lastupdated='February 17, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
  m.ongetnameandlink='getnameandlink'
end

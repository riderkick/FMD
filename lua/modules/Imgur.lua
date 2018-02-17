function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)		
    mangainfo.title=x.xpathstring('//h1[@class="post-title"]')	
    mangainfo.chapterlinks.add(url)
    mangainfo.chapternames.add(mangainfo.title)
    return no_error
  else
    return net_problem
  end
end

function taskstart()
  task.pagelinks.clear()
  return true
end

function getpagenumber()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    x.xpathStringAll('//div[contains(@class,"post-image")]/a/img/concat("https:",@src)', task.pagelinks)
    return true
  else
    return false
  end
  return true
end

function Init()
  m=NewModule()
  m.website='Imgur'
  m.rooturl='https://imgur.com'
  m.lastupdated='February 6, 2018'
  m.OnGetInfo='getinfo'
  m.OnTaskStart='taskstart'
  m.OnGetPageNumber='getpagenumber'
end

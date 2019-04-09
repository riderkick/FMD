function getinfo()
  if url:find('/episodes') then
    mangainfo.url=MaybeFillHost(module.RootURL, url)
  else
    mangainfo.url=MaybeFillHost(module.RootURL, url..'/episodes')
  end
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    mangainfo.title = x.xpathstring('//div[@class="detail-top-left"]/h1')
    mangainfo.coverlink=x.xpathstring('//div[@class="detail-top-right"]/img/@src')
    mangainfo.authors=x.xpathstring('//div[@class="detail-top-left"]//div[@class="created-by"]')
	  mangainfo.genres=x.xpathstring('//div[@class="detail-top-left"]//div[@class="top-comics-type]')
	  local chapters = x.xpath('//div[@class="episodes-wrap"]/a')
	  for i = 1, chapters.count do
		  local v1 = chapters.get(i)
		  mangainfo.chapterlinks.add(v1.getAttribute('href'))
		  mangainfo.chapternames.add(x.xpathstring('.//div[@class="episode-title"]', v1))
	  end
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagenumber=0
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    TXQuery.Create(http.Document).xpathstringall('//div[@class="pictures"]//img/@src', task.pagelinks)
    return true
  else
    return false
  end
end

function getnameandlink()
  if http.get(module.rooturl..'/en/genre?page=' .. IncStr(url)) then
    local x=TXQuery.Create(http.Document)
	  TXQuery.Create(http.document).XPathHREFAll('//ul[contains(@class, "ret-search-list")]/li//h3/a',links,names)
	  local v = x.xpath('//div[@class="items"]/a')
	  for i = 1, v.count do
      local v1 = v.get(i)
	    names.Add(x.xpathstring('.//div[@class="content-title"]', v1));
      links.Add(v1.getAttribute('href')..'/episodes');
    end	
    p = tonumber(20)
    if p ~= nil then
      updatelist.CurrentDirectoryPageNumber = p
    end
	  return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='English'
  m.website='MangaToon'
  m.rooturl='https://mangatoon.mobi'
  m.lastupdated='April 09, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end
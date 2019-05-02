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
    
    if task.pagenumber == 1 then
      return false
    end

    return true
  else
    return false
  end
end

function getnameandlink()
  
  local dirurl = '/genre?page='
  if module.website == 'MangaToon' then
	 dirurl = '/en/genre?page='
  end
  
  if http.get(module.rooturl..dirurl.. IncStr(url)) then
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

function AddWebsiteModule(site, url, cat)
  local m=NewModule()
  m.category=cat
  m.website=site
  m.rooturl=url
  m.lastupdated='April 09, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  return m
end

function Init()
local cat = 'English'
      AddWebsiteModule('MangaToon', 'https://mangatoon.mobi', cat)

	  cat = 'Indonesian'
      AddWebsiteModule('MangaToonID', 'https://mangatoon.mobi/id', cat)  
	  
	  cat = 'Vietnamese'
      AddWebsiteModule('MangaToonVI', 'https://mangatoon.mobi/vi', cat)
	  
	  cat = 'Spanish'
      AddWebsiteModule('MangaToonSP', 'https://mangatoon.mobi/es', cat)
	  
	  cat = 'Webcomics'
      AddWebsiteModule('MangaToonCN', 'https://mangatoon.mobi/cn', cat)
end

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//h1[1]/span')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="movie-l-img"]/img/@src'))
    mangainfo.authors=x.xpathstring('//dt[contains(., "Author")]/following-sibling::dd')
    mangainfo.genres=x.xpathstringall('//dt[contains(., "Genre")]/following-sibling::dd/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//dt[contains(., "Status")]/following-sibling::dd'))
    mangainfo.summary=x.xpathstring('//div[@id="film-content"]')
	
    local spages = x.xpathstring('//div[@class="general-nav"]/a[last()]/@href')		  
		  spages = string.sub(spages, string.len(spages) - 1, string.len(spages))
		  spages = spages:gsub('/', '')
		  if spages == '' or spages == 'N/A' then
			 spages = 1
		  end
	local pages = tonumber(spages)
	local p = 1
    while p <= pages do
      if p >= 1 then
        if http.get(mangainfo.url..'/'..p) then
			x=TXQuery.Create(http.document)
        else
          break
        end
      end

      if p == pages then
        local spg = x.xpathstring('//div[@class="general-nav"]/a[last()]/@href')
		      spg = string.sub(spg, string.len(spg) - 1, string.len(spg))
		      spg = spg:gsub('/', '')
			  if spg == '' or spg == 'N/A' then
				 spg = 1
			  end
		local pg = tonumber(spg)
		if pg ~= '' then pages = pg end
      end
      local v=x.xpath('//tbody[@id="list"]/tr/td/a')
      for i=1,v.count do
        local v1=v.get(i)
        mangainfo.chapterlinks.add(v1.getAttribute('href'))
        mangainfo.chapternames.add(v1.toString)
      end
      p = p + 1
    end
	
	InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  if http.get(MaybeFillHost(module.rooturl, url .. '/full')) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//img[@class="chapter_img"]/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl .. '/comic-list') then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//div[@class="serie-box"]/ul/li/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'ComicExtra'
  m.rooturl = 'http://www.comicextra.com'
  m.category = 'English'
  m.lastupdated='May 7, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

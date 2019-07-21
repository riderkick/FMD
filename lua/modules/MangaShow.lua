function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if module.website == '11Toon' then
		mangainfo.title=x.xpathstring('//*[@class="title"]')
		mangainfo.coverlink=x.xpathstring('//*[@class="banner"]/@src')
		mangainfo.genres=x.xpathstring('//*[@class="genre-link"]/text()')
		local pages = 1
		local p = 1
		while p <= pages do

		  if p > 1 then
			if http.get(mangainfo.url .. '&sord=&type=&page=' .. tostring(p)) then
			  x=TXQuery.Create(http.document)
			else
			  break
			end
		  end
		  if p == pages then
			local pg = x.XPathString('//a[contains(@class, "pg_end")]/substring-after(@href, "&page=")')
			if pg ~= '' then pages = tonumber(pg) end
		  end
		  local v=x.xpath('//ul[@class="episode-list"]//li/button')
		  for i=1,v.count do
			local v1=v.get(i)
			local link = v1.getAttribute('onclick')
			      link = link:gsub('location.href=', ''):gsub('./', '/'):gsub("'", '')
			mangainfo.chapterlinks.add('/bbs'..link)
			mangainfo.chapternames.add(x.xpathstring('.//*[@class="episode-title ellipsis"]/text()', v1))
		  end
		  p = p + 1
		end
	else
		if mangainfo.title == '' then
		  mangainfo.title=x.xpathstring('css("div.manga-subject")')
		end
		local img = x.xpathstring('css("div.manga-thumbnail")/@style')
		img = GetBetween('url(', ')', img)
		mangainfo.coverlink=MaybeFillHost(module.RootURL, img)
		mangainfo.authors=x.xpathstringall('css("a.author")')
		mangainfo.genres=x.xpathstringall('css("div.manga-tags > a")')
		x.xpathhrefall('css("div.chapter-list > div.slot > a")', mangainfo.chapterlinks, mangainfo.chapternames)
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
  local imghost = 'https://img.ironmancdn.xyz'
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    local s=x.xpathstring('//script[contains(., "img_list")]')
    s = GetBetween('var img_list =', ';', s)
    x.parsehtml(s)
	if module.website == '11Toon' then
		local v = x.xpath('json(*)()')
		for i = 1, v.count do
		  local v1 = v.get(i)
		  local link = v1.ToString:gsub('https://11toon.com', imghost)
		        task.pagelinks.Add(link..'?v=2')
		end
    else
		x.xpathstringall('json(*)()', task.pagelinks)
	end
  else
    return false
  end
  return true
end
    
function getnameandlink()
  if module.website == '11Toon' then
	  if http.GET(module.RootURL .. '/bbs/board.php?bo_table=toon_c&is=&sord=&type=upd&page=' .. IncStr(url)) then
		local x = TXQuery.Create(http.Document)
		local v = x.xpath('//ul[@id="free-genre-list"]//li/a')
		for i = 1, v.count do
		  local v1 = v.get(i)
		  names.Add(v1.getattribute('data-ga-event-label'));
		  links.Add(v1.getattribute('href'));
		end
	  else
		return net_problem
	  end
  else
	  if http.get(module.rooturl .. '/bbs/page.php?hid=manga_list&page=' .. IncStr(url)) then
		local x = TXQuery.Create(http.Document)
		x.XPathHREFAll('css("div.manga-list-gallery div.manga-subject > a")', links, names)
		return no_error
	  else
		return net_problem
	  end
  end
end

function getdirectorypagenumber()
	if module.website == '11Toon' then
		if http.GET(module.RootURL .. '/bbs/board.php?bo_table=toon_c&is=&sord=&type=upd&page=1') then
			local x = TXQuery.Create(http.Document)
			page = tonumber(x.XPathString('//a[contains(@class, "pg_end")]/substring-after(@href, "&page=")'))
			if page == nil then
			  page = 1
			end
			return true
		else
			return false
		end
	else
		if http.GET(module.RootURL .. '/bbs/page.php?hid=manga_list') then
			local x = TXQuery.Create(http.Document)
			page = tonumber(x.XPathString('//ul[@class="pagination"]/li[last()]/a/@href'):match('%((%d+)%)$'))
			if page == nil then
			  page = 1
			end
			return true
		else
			return false
		end
	end
end

function AddWebsiteModule(site, url, cat)
  local m=NewModule()
  m.category=cat
  m.website=site
  m.rooturl=url
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
  return m
end

function Init()
local cat = 'Raw'
	AddWebsiteModule('MangaShow', 'https://mangashow.me', cat)
	AddWebsiteModule('11Toon', 'https://www.11toon.com', cat)
end


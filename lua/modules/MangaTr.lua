local mangatrdirurl = '/manga-list.html?listType=allABC'
local puzzmosdirurl = '/directory?type=text'

function getinfo()
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
	  local imgurl = x.xpathstring('//img[@class="thumbnail"]/@src')
	
	  mangainfo.coverLink = module.rooturl..'/'..imgurl
	  mangainfo.title = x.xpathstring('//title')
	  if mangainfo.title > '' then
		  if (Pos('Manga - Oku', mangainfo.title) > 0) then
			  mangainfo.title = Trim(x.xpathstring('//title/substring-after(substring-before(., " Manga - Oku "), "Read")'))
		  end
		  if (Pos('Mangasını Oku', mangainfo.title) > 0) then
			  mangainfo.title = Trim(x.xpathstring('//title/substring-after(substring-before(., " Mangasını Oku "), "Read")'))
		  end
	  end
	
	  if mangainfo.title == '' then 
	    mangainfo.title = x.xpathstring('//h1')
	  end
    
	  if (Pos('Yazar', x.xpathstring('//table[1]/tbody/tr[1]/td[1]')) > 0) then
      mangainfo.authors = x.xpathstring('//table[1]/tbody/tr[2]/td[1]')
		  mangainfo.genres = Trim(x.xpathstring('//table[1]/tbody/tr[2]/td[3]'))
    else
		  mangainfo.authors = x.xpathstring('//table[2]/tbody/tr[2]/td[1]')
		  mangainfo.genres = Trim(x.xpathstring('//table[2]/tbody/tr[2]/td[3]'))
	  end
	    
	  mangainfo.summary = x.xpathstring('//*[@class="well"]/p')

    local info = x.xpathstring('//*[@slug]/@slug')
	  local pages = 2
	  local p = 1
    while p <= pages do
      if p >= 1 then
        http.reset()
		    http.headers.values['Cache-Control'] = 'no-cache'
		    http.headers.values['content-type'] = 'application/x-www-form-urlencoded; charset=UTF-8'
		    http.Headers.Add('X-Requested-With: XMLHttpRequest')
		    if http.post(module.rooturl .. '/cek/fetch_pages_manga.php?manga_cek='..info, 'page='..p) then
			    x=TXQuery.Create(http.document)
        else
          break
        end
      end

      if p == pages then
        local pg = x.xpathstring('//*[@class="last"]/a/@data-page')
        if pg ~= '' then pages = tonumber(pg) end
      end
      local v=x.xpath('//tr/td[1]/a')
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

function getnameandlink()
  local url = module.rooturl
  if module.website == 'Manga-Tr' then
    url = url..mangatrdirurl
  end
  
  if module.website == 'Puzzmos' then
    url = url..puzzmosdirurl
  end
  
  if http.get(url) then
	  local x=TXQuery.Create(http.Document)
	  local v=x.xpath('//tr/td[1]/a')
		if v.count == 0 then
		   v=x.xpath('//*[@data-toggle="mangapop"]/b/a')
		end
    for i=1,v.count do
      local v1=v.get(i)
		  if v1.toString > '' or v1.toString > 'N/A' then
		    links.Add(v1.getAttribute('href'))
		    names.Add(v1.toString)
		  end
    end
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()  
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    task.pagenumber = x.xpathcount('//div[@class="chapter-content"]/select[2]/option/@value')
  else
    return false
  end
  return true
end

function getimageurl()
  local s = url:gsub('.html', '')..'-page-'..(workid+1)..'.html'
  if http.get(MaybeFillHost(module.rooturl,s)) then
    local x=TXQuery.Create(http.document)
	local imgurl = MaybeFillHost(module.rooturl, x.xpathstring('//div[@class="chapter-content"]//img[@class="chapter-img"]/@src'))
    if imgurl > '' or imgurl > 'N/A' then
		task.pagelinks[workid] = imgurl
	else
	    task.pagelinks[workid] = MaybeFillHost(module.rooturl, x.xpathstring('//div[@class="chapter-content"]//img/@src'))
	end
  	return true
  end
  return false
end

function AddWebsiteModule(site, url)
  local m=NewModule()
  m.category='Turkish'
  m.website=site
  m.rooturl=url
  m.lastupdated = 'May 06, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetimageurl='getimageurl'
  return m
end

function Init()
  AddWebsiteModule('Manga-Tr', 'https://manga-tr.com')
  AddWebsiteModule('Puzzmos', 'http://puzzmos.com')
end
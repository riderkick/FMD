function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
      local x=TXQuery.Create(http.document)
	  mangainfo.title = x.xpathstring('//div[@class="media-body"]//h1')
	  mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="media-left cover-detail"]//img/@src'))
      mangainfo.authors=x.xpathstring('//div[@class="media-body"]/p/span[starts-with(.,"Author(s): ")]')
	  mangainfo.genres=x.xpathstringall('//div[@class="media-body"]/p/span[starts-with(.,"Genre: ")]')
	  mangainfo.summary=x.xpathstring('//div[@class="manga-content"]/p')
	  mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//*[@class="description-update"]//span[starts-with(.,"Status")]'))
	  local v=x.xpath('//div[@class="chapter-list"]/ul/li[@class="row"]//a')
	  for i=1,v.count do
		local v1=v.get(i)
		mangainfo.chapterlinks.add(v1.getattribute('href'))
		mangainfo.chapternames.add(v1.getattribute('title'))
      end
	  InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
	  return no_error
  else
    return net_problem
  end
end

function getpagenumber()
	task.pagelinks.clear()
	if http.get(MaybeFillHost(module.rooturl,url .. '/0')) then
	   TXQuery.Create(http.Document).xpathstringall('//div[@class="each-page"]//img/@src', task.pagelinks)
	   return true
	end
	return false
end

function getnameandlink()  
  if http.get(module.rooturl..'/popular-manga?page='.. IncStr(url)) then
    local x=TXQuery.Create(http.Document)
	  local v = x.xpath('//*[@class="cate-manga"]//*[@class="media-body"]/a')
	  for i = 1, v.count do
      local v1 = v.get(i)
      	local title = v1.getAttribute('title')
      		  title = string.gsub(title, 'Manga', '')
	    names.Add(title);
      	links.Add(v1.getAttribute('href'));
    end	
    --p = tonumber(500)
    p = tonumber(x.xpathstring('//div[@class="pagination"]/ul/li[last()]/substring-after(@href, "?page=")'))
    if p ~= nil then
      updatelist.CurrentDirectoryPageNumber = p
    end
	  return no_error
  else
    return net_problem
  end
end


function Init()
  local m = NewModule()
  m.website = 'MangaFull'
  m.rooturl = 'https://mangafull.org'
  m.category = 'English'
  m.lastupdated='May 14, 2019'
  m.sortedlist = true
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

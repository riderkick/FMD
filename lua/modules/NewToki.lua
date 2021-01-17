function getinfo()
	mangainfo.url=MaybeFillHost(module.RootURL, url)
  	if http.get(mangainfo.url) then
    	x=TXQuery.Create(http.document)
    	mangainfo.title     = x.xpathstring('//title'):gsub('미리보기', '')
    	mangainfo.coverlink = MaybeFillHost(module.RootURL, x.xpathstring('//meta[@property="og:image"]/@content'))
    	mangainfo.authors   = x.xpathstring('//meta[@property="og:author"]/@content')
    	mangainfo.summary   = x.xpathstring('//meta[@property="og:description"]/@content')
    	local v = x.xpath('//ul[contains(@class, "list-body")]//li')
	    for i = 1, v.count do
	      local v1 = v.get(i)
	      local name = v1.getAttribute('href')
	      mangainfo.chapternames.Add(Trim(x.xpathstring('.//div[contains(@class, "wr-subject")]/a', v1)))
	      mangainfo.chapterlinks.Add(x.xpathstring('.//div[contains(@class, "wr-subject")]//a/@href', v1));
	    end
    	InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    	return no_error
  	else
    	return net_problem
  end
end

function getpagenumber()
	task.pagenumber=0
	task.pagelinks.clear()
	if http.get(MaybeFillHost(module.rooturl,url)) then
		local x = TXQuery.Create(http.Document)
		x.xpathstringall('//div[contains(@class, "view-content")]//img/@data-original', task.pagelinks)
		return true
	else
		return false
	end
end

function getnameandlink()
	if http.get(module.rooturl..'/webtoon/p'.. IncStr(url)..'?toon=일반웹툰') then
		local x=TXQuery.Create(http.Document)
		  local v = x.xpath('//*[contains(@id, "webtoon-list-all")]//li')
		  for i = 1, v.count do
	      local v1 = v.get(i)
		    names.Add(Trim(x.xpathstring('.//*[contains(@class, "title")]', v1)));
	      	links.Add(x.xpathstring('.//*[contains(@class, "trans-bg-black")]/a/@href', v1));
	    end	
	    p = tonumber(100) --limit 100 FMD freze if up to 500
	    if p ~= nil then
	      updatelist.CurrentDirectoryPageNumber = p
	    end
		return no_error
	else
	    return net_problem
	end
end

function Init()
	local m=NewModule()
  	m.category='Webcomics'
  	m.website='NewToki'
  	m.rooturl='https://newtoki34.com'
  	m.ongetinfo='getinfo'
  	m.ongetpagenumber='getpagenumber'
  	m.ongetnameandlink='getnameandlink'
  	return m
end
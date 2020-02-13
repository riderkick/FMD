function GetInfo()
	mangainfo.url=MaybeFillHost(module.rooturl,url)
	if http.get(mangainfo.url) then
		x=TXQuery.Create(http.Document)
		mangainfo.title=x.XPathString('//h1[@itemprop="name"]')
		mangainfo.coverlink=x.XPathString('//div[@class="imgdesc"]/img/@src')
		mangainfo.authors=x.XPathString('//div[@class="listinfo"]//li[contains(.,"Author")]/substring-after(.,":")');
		mangainfo.summary=x.XPathString('//div[@id="noidungm"]');
		mangainfo.genres=x.XPathStringAll('//div[@class="listinfo"]//li[contains(., "Genres")]/a')
		mangainfo.status=MangaInfoStatusIfPos((x.XPathString('//div[@class="listinfo"]//li[contains(.,"Status")]')))
		local chapters=x.XPath('//div[@class="cl"]//li/span')
		for i=1,chapters.count do
			mangainfo.chapterlinks.add(x.XPathString('a/@href',chapters.get(i)))
			mangainfo.chapternames.add(x.XPathString('.',chapters.get(i)))
		end		
		InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)    
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if http.get(MaybeFillHost(module.rooturl,url)) then
		task.pagelinks.commatext=TXQuery.Create(http.Document).XPathString('//p[@id="arraydata"]')
		return true
	else
		return false
	end
end

function Init()
	m=NewModule()
	-- m.category='English'
	m.website='MangaDexToday'
	m.rooturl='http://mangadex.today'
	m.ongetinfo='GetInfo'
	m.ongetpagenumber='GetPageNumber'
end

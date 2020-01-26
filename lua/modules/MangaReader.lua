function GetInfo()
	mangainfo.url=MaybeFillHost(module.rooturl,url)
	if http.get(mangainfo.url) then
		x=TXQuery.Create(http.Document)
		mangainfo.coverLink = x.XPathString('//*[@id="mangaimg"]/img/@src')
		mangainfo.title = x.XPathString('//*[@id="mangaproperties"]//h2')
		mangainfo.authors = x.XPathString('//*[@id="mangaproperties"]//td[contains(text(),"Author:")]/following-sibling::td')
		mangainfo.artists = x.XPathString('//*[@id="mangaproperties"]//td[contains(text(),"Artist:")]/following-sibling::td')
		mangainfo.genres = x.XPathStringAll('//*[@id="mangaproperties"]//td[contains(text(),"Genre:")]/following-sibling::td/a')
		mangainfo.status = MangaInfoStatusIfPos((x.XPathString('//*[@id="mangaproperties"]//td[contains(text(),"Status:")]/following-sibling::td')))
		summary = x.XPathString('//*[@id="readmangasum"]/p')
		local chapters=x.XPath('//table[@id="listing"]//tr/td[1]')
		for i=1,chapters.count do
			mangainfo.chapterlinks.add(x.xpathstring('a/@href',chapters.get(i)))
			mangainfo.chapternames.add(x.xpathstring('.',chapters.get(i)))
		end
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if http.get(MaybeFillHost(module.rooturl,url)) then
		task.PageNumber=TXQuery.Create(http.Document).XPath('//select[@id="pageMenu"]/option').Count
		return true
	else
		return false
	end
end

function GetImageURL()
	if http.get(MaybeFillHost(module.rooturl,url)..'/'..tostring(workid+1)) then
		task.pagelinks[workid]=TXQuery.Create(http.Document).XPathString('//img[@id="img"]/@src')
		return true
	else
		return false
	end
end

function GetNameAndLink()
	if http.get(module.rooturl..'/alphabetical') then
		TXQuery.Create(http.Document).XPathHREFAll('//ul[@class="series_alpha"]/li/a', links, names)
		return no_error
	else
		return net_problem
	end
end

function Init()
	AddWebsiteModule('MangaReader', 'http://www.mangareader.net', 'English')
	AddWebsiteModule('MangaPanda', 'http://www.mangapanda.com', 'English')
end

function AddWebsiteModule(name, url, category)
	local m = NewModule()
	m.Website				= name
	m.RootURL				= url
	m.Category				= category
	m.OnGetInfo				= 'GetInfo'
	m.OnGetPageNumber		= 'GetPageNumber'
	m.OnGetImageURL			= 'GetImageURL'
	m.OnGetNameAndLink		= 'GetNameAndLink'
end

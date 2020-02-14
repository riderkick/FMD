local dirurl = '/manga-list'

function GetDirectoryPageNumber()
	if http.get(module.rooturl..dirurl) then
		local s = TXQuery.create(http.document).xpathstring('(//ul[@class="pagination"]//a)[last()-1]/@href')
		page = tonumber(s:match('=(%d+)$') or 1)
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if http.get(module.rooturl..dirurl..'?page='..IncStr(url)) then
	    TXQuery.create(http.document).xpathhrefall('//*[@class="row"]//a[@class="chart-title"]', links, names)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	mangainfo.url = MaybeFillHost(module.rooturl, url)
	if http.get(mangainfo.url) then
		local x = TXQuery.create(http.document)
		
		mangainfo.coverlink = x.xpathstring('//meta[@itemprop="photo"]/@content')
		mangainfo.title     = x.xpathstring('//div[@class="container"]/div[@class="row"]/div/h2')
		mangainfo.summary   = x.xpathstring('//h5[text()="Summary"]/following-sibling::*')
		
		x.XPathHREFAll('//ul[@class="chapters"]/li/h3/a', mangainfo.chapterlinks, mangainfo.chapternames)
		InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if http.get(RemoveURLDelim(MaybeFillHost(module.rooturl, url))) then
		TXQuery.Create(http.document).xpathstringAll('//div[@id="all"]/img/@data-src', task.pagelinks)
		return true
	else
		return false
	end
end

function AddWebsiteModule(website, rooturl, category)
	m=NewModule()
	m.Website                    = website
	m.RootURL                    = rooturl
	m.Category                   = category
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function Init()
	AddWebsiteModule('MangaZuki', 'https://mangazuki.co', 'English-Scanlation')
	AddWebsiteModule('MangaZukiRaws', 'https://raws.mangazuki.co', 'Raw')
end

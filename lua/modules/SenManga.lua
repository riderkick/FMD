function Init()
	m=NewModule()
	m.Website                    = 'SenManga'
	m.RootURL                    = 'https://www.senmanga.com'
	m.Category                   = 'English'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.MaxTaskLimit               = 1
	m.MaxConnectionLimit         = 4
end

function GetDirectoryPageNumber()
	if http.get(module.rooturl .. '/directory') then
		page = tonumber(TXQuery.create(http.document).x.xpathstring('//ul[@class="pagination"]/li[./a/@rel="next"]/preceding-sibling::li[1]')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if http.get(module.rooturl .. '/directory?page=' .. IncStr(url)) then
		TXQuery.create(http.document).xpathhrefall('//div[@class="details"]/p[@class="title"]/a', links, names)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	mangainfo.url=MaybeFillHost(module.rooturl,url):gsub('/*$','')
	if http.get(mangainfo.url) then
		x=TXQuery.Create(http.Document)
		mangainfo.title     = x.xpathstring('//h1[@class="title"]')
		mangainfo.coverlink = x.xpathstring('//div[@class="thumbnail"]/img/@src')
		mangainfo.authors   = Trim(x.xpathstringall('//ul[@class="series-info"]/li[contains(.,"Author:")]/substring-after(text(),":")'))
		mangainfo.artists   = Trim(x.xpathstringall('//ul[@class="series-info"]/li[contains(.,"Artist:")]/substring-after(text(),":")'))
		mangainfo.summary   = x.xpathstring('//*[@itemprop="description"]');
		mangainfo.genres    = x.xpathstringall('//ul[@class="series-info"]/li[contains(., "Categories:")]/a')
		mangainfo.status    = MangaInfoStatusIfPos((x.xpathstring('//ul[@class="series-info"]/li[contains(.,"Status:")]/substring-after(text(),":")')))
		x.xpathhrefall('//div[@class="title" and contains(., "Chapters")]/following-sibling::div/div/a', mangainfo.chapterlinks, mangainfo.chapternames)
		InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	http.cookies.values['viewer'] = '1'
	if http.get(MaybeFillHost(module.rooturl, url)) then
		local x = TXQuery.Create(http.Document)
		local s = x.xpathstring('//script[contains(., "var imglist")]')
		s = GetBetween('var imglist = ', ';', s):gsub('%s', '')
		x.parsehtml(s)
		x.xpathstringall('json(*)().url', task.pagelinks)
		return true
	else
		return false
	end
end

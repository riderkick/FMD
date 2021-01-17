local dirurl = '/explore?search[sort]=date'

function GetDirectoryPageNumber()
	if http.get(module.rooturl .. dirurl) then
		page = tonumber(TXQuery.create(http.document).xpathstring('//ul[@class="pagination"]/li[last()-1]/a')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if http.get(module.rooturl .. dirurl .. '&page=' .. IncStr(url)) then
		TXQuery.create(http.document).xpathhrefall('//a[contains(@class,"comic-grid-name")]', links, names)	    
	else
		return net_problem
	end
end

function GetInfo()
	mangainfo.url = MaybeFillHost(module.rooturl, url)
	if http.get(mangainfo.url) then
		local x = TXQuery.create(http.document)
		
		mangainfo.coverlink = MaybeFillHost(module.url, x.xpathstring('//img[@class="cover-detail-img"]/@src'))
		mangainfo.title     = x.xpathstring('//h1')
		mangainfo.authors   = x.xpathstring('//a[@itemprop="author"]')
		mangainfo.genres    = x.xpathstring('string-join(//span[@itemprop="about"]//svg/a, ", ")')
		mangainfo.summary   = x.xpathstring('//div[@itemprop="description"]')

		x.xpathhrefall('//div[@class="card-header" and .="Содержание"]/following-sibling::div//a[contains(@class,"text-truncate")]', mangainfo.chapterlinks, mangainfo.chapternames)
		InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if http.get(MaybeFillHost(module.rooturl, url)) then
		local x = TXQuery.create(http.document)
		local s = x.xpathstring('//div[@data-js-scans]/@data-js-scans')
		if s ~= '' then
			x.parsehtml(s)
			for _, v in ipairs(x.xpathi('json(*)()("src")')) do
				task.pagelinks.add(MaybeFillHost(module.rooturl, v.tostring:gsub('^//', 'https://')))
			end
		end
		return true
	else
		return false
	end
end

function Init()
	m=NewModule()
	m.Website                    = 'MangaHubRU'
	m.RootURL                    = 'https://mangahub.ru'
	m.Category                   = 'Russian'
	m.SortedList                 = true
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

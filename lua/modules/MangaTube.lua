function Init()
	m=NewModule()
	m.Website                    = 'MangaTube'
	m.RootURL                    = 'https://manga-tube.me'
	m.Category                   = 'German'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetDirectoryPageNumber()
	if http.get(module.rooturl .. '/series/?filter=alphabetic') then
		page = tonumber(TXQuery.create(http.document).x.xpathstring('//div[@id="series_list"]/@data-series-pages')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	local data = 'action=load_series_list_entries&parameter%5Bpage%5D=' .. IncStr(url) .. '&parameter%5Bletter%5D=&parameter%5Bsortby%5D=alphabetic&parameter%5Border%5D=asc'
	if http.post(module.rooturl .. '/ajax', data) then
		local x = TXQuery.create(http.document)
		x.xpathstringall('json(*).success().manga_title', names)
		local v for _,v in ipairs(x.xpathi('json(*).success().manga_slug')) do
			links.add(module.rooturl .. '/series/' .. v.tostring)
		end
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	mangainfo.url = MaybeFillHost(module.rooturl, url)
	if http.get(mangainfo.url) then
		local x = TXQuery.create(http.document)
		
		mangainfo.coverlink = MaybeFillHost(module.url, x.xpathstring('//div[contains(@class, "cover")]//img/@data-original'))
		mangainfo.title     = x.xpathstring('//h1[@class="series-title"]')
		mangainfo.authors   = x.xpathstringall('//ul[contains(@class, "series-details")]/li[contains(., "Autor")]/a')
		mangainfo.artists   = x.xpathstringall('//ul[contains(@class, "series-details")]/li[contains(., "Artist")]/a')
		mangainfo.genres    = x.xpathstringall('//ul[contains(@class, "genre-list")]/li/a')
		mangainfo.genres    = SeparateRight(x.xpathstring('//*[@class="row"][starts-with(.,"Genre")]'),':')
		mangainfo.summary   = x.xpathstring('//h4[text()="Beschreibung"]/following-sibling::text()[1]')
		mangainfo.status    = MangaInfoStatusIfPos(x.xpathstring('//ul[contains(@class, "series-details")]/li[contains(., "Status (Offiziell):")]/text()'), 'laufend', 'abgeschlossen')
		
		local v for _,v in ipairs(x.xpathi('//ul[contains(@class, "chapter-list")]/li/a[contains(@href, "read/")]')) do
			mangainfo.chapterlinks.add(v.getattribute('href'))
			mangainfo.chapternames.add(x.xpathstring('concat(b, " ", span[1])',v))
		end
		InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if http.get(MaybeFillHost(module.rooturl, url)) then
		local x = TXQuery.create(http.document)
		local s = x.xpathstring('//script[contains(., "img_path: ")]')
		local img_path = s:match("img_path:%s*'(.-)'")
		s = s:match('pages:%s*(%[.-%])')
		if img_path and s then
			x.parsehtml(s)
			local v for _,v in ipairs(x.xpathi('json(*)().file_name')) do
				task.pagelinks.add(img_path .. v.tostring)
			end
		end
		return true
	else
		return false
	end
end

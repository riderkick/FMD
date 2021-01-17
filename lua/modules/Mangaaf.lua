local dirurl = '/api/v1/explore/state/'
local dirstates = {'stopped', 'ongoing', 'completed'}

function GetNameAndLink()
	local s, p = dirstates[module.CurrentDirectoryIndex + 1], IncStr(url)
	if http.get(module.rooturl .. dirurl .. s .. '?page=' .. p) then
	    local x = TXQuery.create(http.document)
		if url == '0' then
			s = x.xpathstring('json(*).last_page')
			updatelist.CurrentDirectoryPageNumber = x.xpathstring('json(*).last_page') or 0
		end
		local v
		for _, v in ipairs(x.xpathi('json(*).data()')) do
			links.add(module.rooturl .. '/m/' .. v.getproperty('slug').tostring())
			names.add(v.getproperty('name').tostring())
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
		local v = x.xpath('//div[@id="content"]/noscript/div[@class="container"]')
		mangainfo.coverlink = x.xpathstring('dl[dt="الكاتب"]/dd', v)
		mangainfo.title     = x.xpathstring('h2', v)
		mangainfo.authors   = x.xpathstring('dl[dt="الكاتب"]/dd', v)
		mangainfo.artists   = x.xpathstring('dl[dt="الراسم"]/dd', v)
		mangainfo.genres    = x.xpathstring('string-join(dl[dt="التصنيفات"]/dd/a,", ")')
		mangainfo.summary   = x.xpathstring('dl[dt="القصة"]/dd', v)
		mangainfo.status    = MangaInfoStatusIfPos(x.xpathstring('dl[dt="الحالة"]/dd', v),
			'مستمرة',
			'مكتملة')
		
		x.xpathhrefall('div[@class="Chapters"]/ul/li/a', mangainfo.chapterlinks, mangainfo.chapternames, v)
		InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if http.get(MaybeFillHost(module.rooturl, url)) then
		TXQuery.create(http.document).xpathstringall('//noscript/div[@class="container"]/ul/li/img/@src', task.pagelinks)
		return true
	else
		return false
	end
end

function Init()
	m=NewModule()
	m.Website                    = 'Mangaf'
	m.RootURL                    = 'https://mangaforall.com'
	m.Category                   = 'Arabic'
	m.TotalDirectory             = 3
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

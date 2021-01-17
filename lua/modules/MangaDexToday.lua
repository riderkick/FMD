function Init()
	m=NewModule()
	m.Website                    = 'MangaDexToday'
	m.RootURL                    = 'https://mangadex.today'
	m.Category                   = 'English'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

local dirurl = '/popular-manga'

function GetDirectoryPageNumber()
	if http.get(module.rooturl .. dirurl) then
		page = tonumber(TXQuery.create(http.document).x.xpathstring('//ul[@class="pagination"]/li[./a/@rel="next"]/preceding-sibling::li[1]/a')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	local data = 'action=load_series_list_entries&parameter%5Bpage%5D=' .. IncStr(url) .. '&parameter%5Bletter%5D=&parameter%5Bsortby%5D=alphabetic&parameter%5Border%5D=asc'
	local s = module.rooturl .. dirurl
	if url ~= '0' then
	  s = s .. '?page=' .. IncStr(url)
	end
	if http.get(s) then
		local x = TXQuery.create(http.document)
		local v for _,v in ipairs(x.xpathi('//li/div/div[@class="left"]/a')) do
			links.add(v.getAttribute('href'))
			names.add(x.xpathstring('./h2', v))
		end
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	mangainfo.url=MaybeFillHost(module.rooturl,url)
	if http.get(mangainfo.url) then
		x=TXQuery.Create(http.Document)
		mangainfo.title     = x.xpathstring('//h1[@itemprop="name"]')
		mangainfo.coverlink = x.xpathstring('//div[@class="imgdesc"]/img/@src')
		mangainfo.authors   = x.xpathstring('//div[@class="listinfo"]//li[contains(.,"Author")]/substring-after(.,":")')
		mangainfo.summary   = x.xpathstring('//div[@id="noidungm"]');
		mangainfo.genres    = x.xpathstringall('//div[@class="listinfo"]//li[contains(., "Genres")]/a')
		mangainfo.status    = MangaInfoStatusIfPos((x.xpathstring('//div[@class="listinfo"]//li[contains(.,"Status")]')))
		local chapters=x.XPath('//div[@class="cl"]//li/span')
		for i=1,chapters.count do
			mangainfo.chapterlinks.add(x.xpathstring('a/@href',chapters.get(i)))
			mangainfo.chapternames.add(x.xpathstring('.',chapters.get(i)))
		end		
		InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)    
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if http.get(MaybeFillHost(module.rooturl,url .. '/0')) then
		TXQuery.Create(http.Document).xpathstringall('//div[@id="readerarea"]/img/@src', task.pagelinks)
		return true
	else
		return false
	end
end

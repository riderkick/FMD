local dirurl = '/c/-/albums/frontpage/0/t/manga/sorted/new/page/'

function GetDirectoryPageNumber()
	if http.get(module.rooturl .. dirurl .. '1/') then
		page = tonumber(TXQuery.create(http.document).xpathstring('//*[@class="pagination"]/*[@class="last"]/a/@href'):match('/(%d+)/*$')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if http.get(module.rooturl .. dirurl .. IncStr(url) .. '/') then
		TXQuery.create(http.document).xpathhreftitleall('//*[@id="albums_wrapper"]//*[@class="item_cover"]/a', links, names)	    
	else
		return net_problem
	end
end

function GetInfo()
	mangainfo.url = MaybeFillHost(module.rooturl, url)
	if http.get(mangainfo.url) then
		local x = TXQuery.create(http.document)
		
		mangainfo.coverlink = MaybeFillHost(module.url, x.xpathstring('//*[@class="album_cover_item"]//img/@src'))
		mangainfo.title     = x.xpathstring('//*[@class="album_cover"]/h2')
		mangainfo.artists   = x.xpathstring('//*[@id="tag_section"]/ol/li/a[starts-with(.,"Artist")]/text()[last()]')
		mangainfo.genres    = x.xpathstring('//*[@id="tag_section"]/ol/string-join(li/a/text()[last()],", ")')
		
		mangainfo.chapterlinks.add(url)
		mangainfo.chapternames.add(mangainfo.title)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if http.get(MaybeFillHost(module.rooturl, url)) then
		TXQuery.create(http.document).xpathstringall('//*[@class="picture_page"]//img/@data-src', task.pagelinks)
		return true
	else
		return false
	end
end

function Init()
	m=NewModule()
	m.Website                    = 'Luscious'
	m.RootURL                    = 'https://www.luscious.net'
	m.Category                   = 'H-Sites'
	m.SortedList                 = true
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

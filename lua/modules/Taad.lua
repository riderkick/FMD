local alphalist = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ'

function GetNameAndLink()
	local s, i, j, x, v
	if module.CurrentDirectoryIndex == 0 then
		s = '0-9'
	else
		i = module.CurrentDirectoryIndex + 1
		s = alphalist:sub(i, i)
	end
	if http.get(module.rooturl .. '/category/' .. s .. '_views_' .. IncStr(url) .. '.html') then
		i = 1
		x = TXQuery.create(http.document)
		for _, v in ipairs(x.xpathi('//*[@class="clistChr"]//span[@class="pagetor"]//text()')) do
			j = tonumber(v.tostring) or 1
			if j > i then i = j end
		end
		updatelist.CurrentDirectoryPageNumber = i
		x.xpathhreftitleall('//*[@class="clistChr"]/ul/li/div/h2/a', links, names)
	else
		return net_problem
	end
end

function GetInfo()
	mangainfo.url = MaybeFillHost(module.rooturl, url)
	local s = mangainfo.url
	if not(s:find('waring=1')) then s = s .. '?waring=1' end
	if http.get(s) then
		local x = TXQuery.create(http.document)
		
		mangainfo.coverlink = MaybeFillHost(module.url, x.xpathstring('//table//td/a/img/@src'))
		mangainfo.title     = x.xpathstring('//title/substring-before(.," - Read ")')
		mangainfo.authors   = x.xpathstring('//table//table//td[starts-with(.,"Author:")]/string-join(./a,", ")')
		mangainfo.genres    = x.xpathstring('//table//table//td[starts-with(.,"Categories:")]/string-join(./a,", ")')
		mangainfo.summary   = x.xpathstring('//table//table//td[contains(.," Manga Summary ")]/substring-after(.,"Manga Summary ")')
		mangainfo.status    = MangaInfoStatusIfPos(x.xpathstring('//table//table//td[starts-with(.,"Status:")]/a'), 'Updated', 'Completed')
		
		x.xpathhrefall('//*[@class="chapter_list"]/table//tr/td[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
		InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if http.get(MaybeFillHost(module.rooturl, url)) then
		task.pagenumber = tonumber(TXQuery.create(http.document).xpathstring('//select[@id="page"]/count(./option)')) or 0
		return true
	else
		return false
	end
end

function GetImageURL()
	if http.get(AppendURLDelim(MaybeFillHost(module.rooturl, url)) .. 'page-' .. IncStr(workid)) then
		task.pagelinks[workid] = TXQuery.create(http.document).xpathstring('//img[@id="comicpic"]/@src')
		return true
	else
		return false
	end
end

function Init()
	m=NewModule()
	m.Website                    = 'Taadd'
	m.RootURL                    = 'http://www.taadd.com'
	m.Category                   = 'English'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.OnGetImageURL              = 'GetImageURL'
	m.TotalDirectory             = alphalist:len()
end

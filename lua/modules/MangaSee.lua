function Init()
	m=NewModule()
	m.Website                    = 'MangaSee'
	m.RootURL                    = 'https://mangaseeonline.us'
	m.Category                   = 'English'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetNameAndLink()
	if http.get(module.rooturl .. '/directory/') then
		TXQuery.create(http.document).x.xpathhrefall('//*[@id="content"]//a', links, names)
	else
		return net_problem
	end
end

function GetInfo()
	mangainfo.url = MaybeFillHost(module.rooturl, url)
	if http.get(mangainfo.url) then
		local x = TXQuery.create(http.document)
		
		mangainfo.coverlink = MaybeFillHost(module.url, x.xpathstring('//meta[@property="og:image"]/@content'))
		mangainfo.title     = x.xpathstring('//*[@class="row"]//h1')
		mangainfo.authors   = SeparateRight(x.xpathstring('//*[@class="row"][starts-with(.,"Author")]'),':')
		mangainfo.artists   = SeparateRight(x.xpathstring('//*[@class="row"][starts-with(.,"Artist")]'),':')
		mangainfo.genres    = SeparateRight(x.xpathstring('//*[@class="row"][starts-with(.,"Genre")]'),':')
		mangainfo.summary   = Trim(SeparateRight(x.xpathstring('//*[@class="row"][starts-with(.,"Description")]'),':'))
		mangainfo.status    = MangaInfoStatusIfPos(x.xpathstring('//*[@class="row"][starts-with(.,"Status")]'))
		
		local s,v for _,v in ipairs(x.xpathi('//div[@class="list chapter-list"]//a')) do
			s = v.getAttribute('href')
			if s:find('%-page%-1') then
			  s = s:gsub('%-page%-1','')
			end
			mangainfo.chapterlinks.add(s)
			mangainfo.chapternames.add(x.xpathstring('span[@class="chapterLabel"]',v))
		end
		InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if http.get(MaybeFillHost(module.rooturl, url)) then
		TXQuery.create(http.document).xpathstringall('//*[contains(@class,"image-container")]//img/@src', task.pagelinks)
		return true
	else
		return false
	end
end

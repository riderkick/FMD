function GetInfo()
	local u = MaybeFillHost(module.RootURL, url)

	if not http.get(u) then return net_problem end

	local x = TXQuery.Create(http.Document)
	mangainfo.Title  = x.XPathString("//h1[@class='entry-title']")
	mangainfo.Genres = x.XPathString('//header[@class="entry-header"]/string-join(./p[position()>1]//a,", ")')
	mangainfo.Status = MangaInfoStatusIfPos(x.XPathString('//*[@class="entry-terms" and contains(., "Status")]/a'))

	mangainfo.ChapterLinks.Add(mangainfo.url)
	mangainfo.ChapterNames.Add(mangainfo.title)

	local v = x.XPath('//*[contains(@class,"entry-pagination")]/a')
	for i = 1, v.Count do
		local v1 = v.Get(i)
		if string.match(v1.toString, '^Next') == nil then
			mangainfo.ChapterLinks.Add(v1.getAttribute('href'));
			mangainfo.ChapterNames.Add(mangainfo.Title .. ' - ' .. v1.toString)
		end
	end
	if mangainfo.ChapterNames.Count > 1 then
		mangainfo.ChapterNames[0] = mangainfo.ChapterNames[0]
	end
end

function GetPageNumber()
	task.PageLinks.Clear()
	if http.get(MaybeFillHost(module.RootURL, url)) then
		local x = TXQuery.Create(http.Document)
		x.XPathStringAll('//*[contains(@class,"entry-content")]//img/@data-src', task.PageLinks)
		if task.PageLinks.Count == 0 then
			x.XPathStringAll('//div[@class="separator" and @style]//img/@data-src', task.PageLinks)
		end
	else
		return false
	end
	return true
end

function GetDirectoryPageNumber()
	if http.get(module.RootURL) then
		local x = TXQuery.Create(http.Document)
		page = tonumber(x.XPathString('//*[contains(@class,"archive-pagination")]/ul/li[last()-1]'))
		if page == nil then page = 1 end
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if http.get(module.rooturl .. '/page/' .. IncStr(url) .. '/') then
		local x = TXQuery.Create(http.Document)
		x.XPathHREFAll('//h2[@class="entry-title"]/a', links, names)
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewModule()
	m.Website = 'MyReadingManga'
	m.RootURL = 'https://myreadingmanga.info'
	m.Category = 'H-Sites'
	m.OnGetInfo='GetInfo'
	m.OnGetPageNumber='GetPageNumber'
	m.OnGetNameAndLink='GetNameAndLink'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.SortedList = true
end

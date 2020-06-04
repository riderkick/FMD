function GetInfo()
	mangainfo.URL = MaybeFillHost(module.RootURL, url)
	mangainfo.Website = "MyReadingManga"
	
	if not http.get(mangainfo.URL) then return net_problem end
	
	local x = TXQuery.Create(http.Document)
	
	mangainfo.Title = x.XPathString("//h1[@class='entry-title']")
	mangainfo.Genres = x.XPathString('//header[@class="entry-header"]/string-join(./p[position()>1]//a,", ")')
	
	mangainfo.ChapterLinks.Add(mangainfo.url)
	mangainfo.ChapterNames.Add("1")
	
	local v = x.XPath('//*[contains(@class,"entry-pagination")]/a')
	
	for i = 1, v.Count do
		local v1 = v.Get(i)
		if string.match(v1.toString, '^Next') == nil then
			mangainfo.ChapterLinks.Add(v1.getAttribute('href'));
			mangainfo.ChapterNames.Add(mangainfo.Title .. ' - ' .. v1.toString);
		end
	end
	if mangainfo.ChapterNames.count > 1 then
		mangainfo.ChapterNames[0] = mangainfo.ChapterNames[0]
	end
end

function GetPageNumber()
	if http.get(MaybeFillHost(module.rootURL, url)) then
		local x = TXQuery.Create(http.Document)
		
		local t = x.XPath('//*[contains(@class, "entry-content")]//img/@data-lazy-src')
		
		if t == 0 then
			t = x.XPath('//*[contains(@class,"separator")]//img/@data-lazy-src')
		end
		
		for i = 1, t.Count do
			local t1 = t.Get(i).ToString
			task.PageLinks.Add(t1)
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
	m.LastUpdated='June 04, 2020'
	m.OnGetInfo='GetInfo'
	m.OnGetPageNumber='GetPageNumber'
	m.OnGetNameAndLink='GetNameAndLink'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.SortedList = true
end

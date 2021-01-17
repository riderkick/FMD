local dirurl='/manga'

function GetDirectoryPageNumber()
	page=1
	if http.get(module.rooturl..dirurl) then
		page=TXQuery.Create(http.document).XPathCount('//div[@class="pagination"]/a')
		return no_error
	else
		return net_error
	end
end

function GetNameAndLink()
	if http.get(module.rooturl..dirurl..'/page:'..IncStr(url)) then
	    TXQuery.Create(http.Document).XPathHREFAll('//div[@id="mangadirectory"]/div[@class="mangacontainer"]/a[2]', links, names)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	mangainfo.url=MaybeFillHost(module.rooturl,url)
	if http.get(mangainfo.url) then
		x=TXQuery.Create(http.Document)
		mangainfo.title=x.XPathString('//h1[@class="EnglishName"]'):match('^%((.*)%)$')
		mangainfo.coverlink=x.XPathString('//img[@class="manga-cover"]/resolve-uri(@src)')
		mangainfo.authors=x.XPathString('//div[@class="manga-details-author"]/h4[1]')
		mangainfo.genres=x.XPathString('//div[@class="manga-details-extended"]/ul/string-join(./li/a,", ")')
		mangainfo.status=MangaInfoStatusIfPos(x.XPathString('//div[@class="manga-details-extended"]/h4[2]'),
          'مستمرة',
          'مكتملة')
		x.XPathHREFAll('//ul[@class="new-manga-chapters"]/li/a', mangainfo.chapterLinks, mangainfo.chapterNames)
		InvertStrings(mangainfo.chapterLinks, mangainfo.chapternames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	local u=module.rooturl..url:gsub('/*$','')
	if u:match('(%d+)$')=='1' then u=u:gsub('/1$','') end
	u=u..'/0/full'
	if http.get(u) then
		TXQuery.Create(http.document).XPathStringAll('//*[@id="showchaptercontainer"]//img/resolve-uri(@src)', task.PageLinks)		
		return true
	else
		return false
	end
end

function Init()
	m=NewModule()
	m.Website='MangaAe'
	m.RootURL='https://manga.ae'
	m.Category='Arabic'
	m.OnGetDirectoryPageNumber='GetDirectoryPageNumber'
	m.OnGetNameAndLink='GetNameAndLink'
	m.OnGetInfo='GetInfo'
	m.OnGetPageNumber='GetPageNumber'
end

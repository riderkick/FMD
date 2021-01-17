function GetNameAndLink()
	if http.get(module.rooturl..'/comics/') then
	    TXQuery.Create(http.Document).XPathHREFAll('//ul[@class="manga-list__list"]/li/h4/a', links, names)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	mangainfo.url=AppendURLDelim(MaybeFillHost(module.rooturl,url))
	if http.get(mangainfo.url) then
		x=TXQuery.Create(http.Document)
		mangainfo.title=x.XPathString('//strong')
		mangainfo.coverlink=x.XPathString('//*[@class="fancybox a-alpha"]/img/@src')
		mangainfo.summary=x.XPathString('//*[@class="single-story"]/p')
		-- there is no chapter list?
		-- assuming the first chapter link in manga info is always the last chapters
	    x.XPathHREFAll('//a[@class="single"]', mangainfo.chapterlinks, mangainfo.chapternames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if http.get(FillHost('http://viewer.tonarinoyj.jp',url)) then
		TXQuery.Create(http.document).XPathStringAll('//img[@class="js-page-image"]/@src', task.PageLinks)
		return true
	else
		return false
	end
end

function Init()
	m=NewModule()
	m.Category='Raw'
	m.Website='YoungAceUp'
	m.RootURL='https://web-ace.jp/youngaceup'
	m.OnGetNameAndLink='GetNameAndLink'
	m.OnGetInfo='GetInfo'
	m.OnGetPageNumber='GetPageNumber'
end

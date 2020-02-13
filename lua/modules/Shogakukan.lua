function GetInfo()
	mangainfo.url=MaybeFillHost(module.rooturl,url)
	if http.get(mangainfo.url) then
		x=TXQuery.Create(http.Document)
		mangainfo.title=x.XPathString('//title/substring-before(.," | ")')
		mangainfo.coverlink=x.XPathString('//meta[@property="og:image"]/@content')
		
		mangainfo.chapterlinks.add(url)
		mangainfo.chapternames.add(mangainfo.title)
		return no_error
	else
		return net_problem
	end
end

function TaskStart()
	task.PageLinks.Clear()
	task.PageContainerLinks.Clear()
	task.PageNumber=0
	return true
end;

function GetPageNumber()
	if http.get(MaybeFillHost(module.rooturl,url)) then
		x=TXQuery.Create(http.document)
		x.XPathStringAll('//*[@id="book_data_area"]/input[@data-key="imageCodes"]/@value', task.PageLinks)
        task.PageContainerLinks.Add(url:gsub('^/',''))
        -- task.PageContainerLinks.Add(x.XPathString('//*[@id="book_data_area"]/input[@data-key="isbn"]/@value'))
        task.PageContainerLinks.Add(x.XPathString('//*[@id="book_data_area"]/input[@data-key="vsid"]/@value'))
		return true
	else
		return false
	end
end

function DownloadImage()
    if workid == 0 then
		http.Headers.Values['Referer']=' '..module.RootURL..'/'..task.PageContainerLinks[1]
    else
		http.Headers.Values['Referer']=' '..module.RootURL..'/'..task.PageContainerLinks[1]..'?page='..tostring(WorkId)
	end
    if http.POST(module.RootURL..'/imgDeliver?jan_cd='..task.PageContainerLinks[0],'base64=1&vsid='..task.PageContainerLinks[1]..'&trgCode='..task.PageLinks[workid]) then
		return Base64Decode(http.document)
	else
		return false
	end
end

function Init()
	m=NewModule()
	-- m.Category='Raw'
	m.Website='Shogakukan'
	m.RootURL='https://shogakukan.tameshiyo.me'
	m.OnGetInfo='GetInfo'
	m.OnTaskStart='TaskStart'
	m.OnGetPageNumber='GetPageNumber'
	m.OnDownloadImage='DownloadImage'
end

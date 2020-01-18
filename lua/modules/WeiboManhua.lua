-- Filled in to get url for comic info
local infoURL = 'http://apiwap.vcomic.com/wbcomic/comic/comic_show?comic_id=%s&_request_from=pc'
local coverURL = 'https://img.manhua.weibo.com/%s'
local chapterURL = 'http://apiwap.vcomic.com/wbcomic/comic/comic_play?chapter_id=%s&_request_from=pc'

function GetInfo()
	mangainfo.url = MaybeFillHost(module.rooturl,url)
	local id = url:match('c/(%d+)')

	if http.get(string.format(infoURL,id)) then
		local x = TXQuery.Create(http.Document)
		mangainfo.title = x.XPathString('json(*)/data/comic/name')
		mangainfo.coverLink = string.format(coverURL, x.XPathString('json(*)/data/comic/hcover'))
		mangainfo.authors = x.XPathString('json(*)/data/comic/sina_nickname')
		mangainfo.summary = x.XPathString('json(*)/data/comic/description')
		
		-- Populate genres list
		local genres = ''
		local category = x.XPath('json(*).data.comic_cate()')
		
		if category.count > 0 then genres = x.XPathString('cate_name', category.get(1)) end
		for i = 2, category.count do
			local c = x.XPathString('cate_name', category.get(i))
			genres = genres .. ', ' .. c
		end
		mangainfo.genres = genres
		
		local chapter = x.XPath('json(*).data.chapter_list()')
		for i = 1, chapter.count do
			mangainfo.chapterlinks.add(string.format(chapterURL,x.XPathString('chapter_id',chapter.get(i))))
			mangainfo.chapternames.add(x.XPathString('chapter_name',chapter.get(i)))
		end
		
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if http.get(MaybeFillHost('http://apiwap.vcomic.com/',url)) then
		local x = TXQuery.Create(http.Document)
		local pages = x.XPath('json(*).data.json_content.page()')
		for i = 1, pages.count do
			task.pagelinks.add(x.XPathString('mobileImgUrl',pages.get(i)))
		end
		return true
	else
		return false
	end
end

function GetNameAndLink()
	-- based on assumption of <10,000 comics, rows num must be increased if this is exceeded (!2500 at time of writing)
	if(http.get'(https://apiwap.vcomic.com/wbcomic/comic/filter_result?page_num=1&rows_num=10000&cate_id=0&end_status=0&comic_pay_status=0&order=comic_read_num&_request_from=pc') then
		local x = TXQuery.Create(http.Document)
		local list = x.XPath('json(*).data.data()')
		for i = 1, list.count do
			links.add('http://manhua.weibo.com/c/' .. x.XPathString('comic_id',list.get(i)))
			names.add(x.XPathString('comic_name',list.get(i)))
		end
	else
		return net_problem
	end
end


	if(http.get'(https://apiwap.vcomic.com/wbcomic/comic/filter_result?page_num=1&rows_num=10000&cate_id=0&end_status=0&comic_pay_status=0&order=comic_read_num&_request_from=pc') then
		local x = TXQuery.Create(http.Document)
		page = tonumber(x.XPathString('json(*).data.data.rows_total'))
		if page == nil then page = 1 end
		return no_error
	else
		return net_problem
	end
end

function Init()
	m = NewModule()
	m.category = 'Raw'
	m.website = 'WeiboManhua'
	m.rooturl = 'http://manhua.weibo.com'
	m.ongetinfo = 'GetInfo'
	m.ongetpagenumber = 'GetPageNumber'
	m.ongetnameandlink = 'GetNameAndLink'
	m.ongetdirectorypagenumber = 'GetDirectoryPageNumber'
end
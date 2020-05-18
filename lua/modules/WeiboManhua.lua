-- Filled in to get url for comic info
local infoURL = 'http://apiwap.vcomic.com/wbcomic/comic/comic_show?comic_id=%s&_request_from=pc'
-- Filled in to get cover url
local coverURL = 'https://img.manhua.weibo.com/%s'
-- Filled in to get chapter url
local chapterURL = 'http://apiwap.vcomic.com/wbcomic/comic/comic_play?chapter_id=%s&_request_from=pc'
-- Filled in to get series page url
local seriesURL = 'http://manhua.weibo.com/c/%s'

function GetInfo()
	mangainfo.url = MaybeFillHost(module.rooturl,url)
	local id = url:match('c/(%d+)')
	if http.GET(string.format(infoURL,id)) then
		--print(StreamToString(http.document))
		local y = StreamToString(http.document)
		local x = TXQuery.Create(http.Document)
		mangainfo.title = ToUTF8(GetBetween('\"name\":\"','\",',y))
		if(x.XPathString('json(*)/data/comic/hcover') ~= '') then
			mangainfo.coverLink = string.format(coverURL, x.XPathString('json(*)/data/comic/hcover'))
		else
			mangainfo.coverLink = x.XPathString('json(*)/data/comic/cover')
		end
		mangainfo.authors = ToUTF8(GetBetween('\"sina_nickname\":\"','\",',y))
		mangainfo.summary = ToUTF8(GetBetween('\"description\":\"','\",',y))
		
		-- Populate genres list
		local genres = ''
		local category = GetBetween('\"comic_cate\":[',']',y)
		local j,k = string.find(category,'cate_name\":\"')
		
		while j ~= nil do
			local i2,j2 = string.find(category,'\",',k)
			local c = ToUTF8(string.sub(category,k+1,i2-1))
			if genres == '' then
				genres = c
			else
				genres = genres .. ', ' .. c
			end
			j,k = string.find(category,'cate_name\":\"',j2)
		end
		mangainfo.genres = genres
		
		local chapter = x.XPath('json(*).data.chapter_list()')
		local chapterString = GetBetween('\"chapter_list\":\"[',']',y)
		j,k = string.find(chapterString,'chapter_name\":\"')
		for i = 1, chapter.count do
			mangainfo.chapterlinks.add(string.format(chapterURL,x.XPathString('chapter_id',chapter.get(i))))
			local i2,j2 = string.find(chapterString,'\",',k)
			local c = ToUTF8(string.sub(chapterString,k+1,i2-1))
			mangainfo.chapternames.add(c)
			j,k = string.find(chapterString,'chapter_name\":\"',j2)
		end
		
		return no_error
	else
		return net_problem
	end
end

function ToUTF8(s)
	local i = 1
	local out = ''
	while i < string.len(s) do
		if string.byte(s,i) == string.byte('\\') and string.byte(s,i+1) == string.byte('u') then
			out = out .. utf8.char(tonumber(string.sub(s,i+2,i+5),16))
			i =i+ 6
		else
			out = out .. string.sub(s,i,i)
			i = i+1
		end
	end
	return out
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
		local y = StreamToString(http.Document)
		local x = TXQuery.Create(http.Document)
		local list = x.XPath('json(*).data.data()')
		local j,k = string.find(y,'comic_name\":')
		for i = 1, list.count do
			links.add(string.format(seriesURL,x.XPathString('comic_id',list.get(i))))
			local i2,j2 = string.find(y,'\",',k)
			local n = ToUTF8(string.sub(y,k+1,i2-1))
			names.add(n)
			j,k = string.find(y,'comic_name\":',j2)
		end
	else
		return net_problem
	end
end

function GetDirectoryPageNumber()
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

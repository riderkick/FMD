--Module Download manga from Instant View Telegram
--Support Download
--  => https://t.me/nHentaiBot
--Not Support
--  => Get MangaList
--
--need support Download manga from telegram you can tell me

function GetInfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    mangainfo.title     = x.XPathString('//meta[@property="og:title"]/@content')
    mangainfo.coverlink = x.XPathString('//meta[@name="twitter:image"]/@content')
    	mangainfo.Summary   = mangainfo.title .. string.char(10) .. x.XPathString('//meta[@property="article:author"]/@content') .. ' - ' .. x.XPathString('//meta[@property="article:published_time"]/@content')
    mangainfo.ChapterLinks.Add(mangainfo.url)
	mangainfo.ChapterNames.Add(mangainfo.title)
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
	task.pagenumber=0
	task.pagelinks.clear()
	if http.get(MaybeFillHost(module.rooturl,url)) then
		local x = TXQuery.Create(http.Document)
		local v = x.xpath('//img')
		for i = 1, v.count do
		  local v1 = v.get(i)
		  task.PageLinks.Add(MaybeFillHost(module.RootURL, v1.getAttribute('src')))
		end
		return true
	else
		return false
	end
end

function Init()
  AddWebsiteModule('Telegraph', 'https://telegra.ph', 'Webcomics')
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.Website               = name
  m.RootURL               = url
  m.Category              = category
  m.OnGetInfo             = 'GetInfo'
  m.OnGetPageNumber       = 'GetPageNumber'
  return m
end

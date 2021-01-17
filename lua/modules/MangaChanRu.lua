local dirurl = '/manga/new';
local perpage = 20;

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
	  mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//img[@id="cover"]/@src'))
	  mangainfo.title=x.xpathstring('//*[@class="name_row"]/h1')
    mangainfo.authors=x.xpathstring('//*[@class="item" and contains(.,"Автор")]/following-sibling::*[1]')
    mangainfo.genres=x.xpathstring('//*[@class="item" and contains(.,"Тэги")]/following-sibling::*[1]')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//*[@class="item" and contains(.,"Загружено")]/following-sibling::*[1]'), 'продолжается', '')
    mangainfo.summary=x.xpathstringall('//*[@id="description"]/text()')
    if module.website=='MangaChanRU' then x.xpathhrefall('//table[@class="table_cha"]//div[@class="manga2"]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
    if module.website=='YaoiChanRU' then x.xpathhrefall('//table[@class="table_cha"]//div[@class="manga"]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
    if module.website=='HentaiChanRU' then
      local v=x.xpath('//div[@id="manga_images"]/a')
      for i = 1, v.count do
        v1 = v.get(i)
        mangainfo.chapterlinks.add(v1.getattribute('href'))
        mangainfo.chapternames.add(v1.getattribute('title'))
      end
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagenumber=0
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    local x=TXQuery.Create(http.Document)
    local v=string.match(x.xpathstring('//script[contains(., "var data")]/text()'), 'var data =(.-);')
    TXQuery.Create(v).xpathstringall('json(*).fullimg()', task.pagelinks)
    return true
  else
    return false
  end
end

function getnameandlink()
  local s = module.rooturl..dirurl
  if url ~= '0' then s = s..'?offset='..tostring(tonumber(url) * perpage) end
  if http.get(s) then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//*[@class="content_row"]//a[@class="title_link"]',links,names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  page=1
  if http.GET(module.RootURL .. dirurl) then
    x = TXQuery.Create(http.Document)
    local s = tonumber(x.XPathString('//*[@id="pagination"]/a[last()]/substring-after(@href,"offset=")'))
    if s ~= '' then page=s end
    if page > 1 then page=math.floor(page / perpage) + 1 end
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = category
  m.lastupdated='May 6, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
  return m
end

function Init()
  local cat = 'Russian'
  AddWebsiteModule('MangaChanRU', 'http://manga-chan.me', cat)
  
  local cat = 'H-Sites'
  AddWebsiteModule('HentaiChanRU', 'http://h-chan.me', cat)
  AddWebsiteModule('YaoiChanRU', 'http://yaoi-chan.me', cat)
end

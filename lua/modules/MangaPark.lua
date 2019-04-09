function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  http.cookies.values['set'] = 'h=1'
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title=x.xpathstring('//h2'):gsub(' Manga$', '')
    end
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[contains(@class, "cover")]/img/@src'))
    mangainfo.authors=x.xpathstringall('//table[@class="attr"]//tr[contains(th,"Author")]/td/a')
    mangainfo.artists=x.xpathstringall('//table[@class="attr"]//tr[contains(th,"Artist")]/td/a')
    mangainfo.genres=x.xpathstringall('//table[@class="attr"]//tr[contains(th,"Genre")]/td/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//table[@class="attr"]//tr[contains(th,"Status")]/td'))
    mangainfo.summary=x.xpathstring('//p[@class="summary"]')
    local v = x.xpath('//div[@id="list"]/div[contains(@class, "stream")]')
    for i = 1, v.count do
      local v1 = v.get(i)
      local stream = ' [' .. x.xpathstring('div[@id]//a/span', v1) .. ']'
      local w = x.xpath('div/ul[@class="chapter"]/li', v1)
      for j = 1, w.count do
        local w1 = w.get(j)
        local link = x.xpathstring('div/a/@href', w1)
        local title = x.xpathstring('div/a', w1) .. x.xpathstring('div[contains(@class, "txt")]', w1)
        mangainfo.chapterlinks.add(link:gsub('/%d+$', ''))
        mangainfo.chapternames.add(title .. stream)
      end
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('json(//script[contains(.,"var _load_pages")]/substring-after(substring-before(.,";")," = "))()/u', task.pagelinks)
  else
    return false
  end
  return true
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. '/search?orderby=create') then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.xpathstring('(//div[@id="paging-bar"])[2]/ul/li[last()-2]/a/substring-after(@href, "page=")'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  if http.get(module.rooturl .. '/search?orderby=create&page=' .. IncStr(url)) then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//div[@class="manga-list"]//table//h2/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(name, url)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = 'English'
  m.lastupdated = 'April 09, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
  m.sortedlist = true
end

function Init()
  AddWebsiteModule('MangaPark', 'https://mangapark.me')
  AddWebsiteModule('MangaParkNet', 'https://mangapark.net')
  AddWebsiteModule('MangaParkCom', 'https://mangapark.com')
end 
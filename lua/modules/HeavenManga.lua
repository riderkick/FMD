function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//div[@class="comic-info"]/div/h1')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="comic-info"]/div/img/@src'))
    mangainfo.authors=x.xpathstringall('//div[@class="comic-info"]//div[@class="author"]/a')
    mangainfo.genres=x.xpathstringall('//div[@class="comic-info"]//div[@class="genre"]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="comic-info"]//div[@class="update"]/span[last()]'))
    mangainfo.summary=x.xpathstring('//div[@class="comic-description"]/p')
    local pages = tonumber(x.xpathstring('(//div[@class="pagination"]/a[contains(@class, "page-numbers")])[last()]/substring-after(@href, "/page-")'))
    if pages == nil then pages = 1 end
    local p = 1
    while true do
      local v=x.xpath('//div[contains(@class, "chapters-wrapper")]//h2[@class="chap"]/a')
      for i=1,v.count do
        local v1=v.get(i)
        mangainfo.chapterlinks.add(v1.getattribute('href'))
        mangainfo.chapternames.add(x.xpathstring('text()', v1))
      end
      p = p + 1
      if p > pages then break end
      if http.get(mangainfo.url .. '/page-' .. tostring(p)) then
        x=TXQuery.Create(http.document)
      else
        break
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
    x.xpathstringall('//div[@class="chapter-content"]//img/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. '/manga-list/') then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.xpathstring('(//div[@class="pagination"]/a[contains(@class, "page-numbers")])[last()]/substring-after(@href, "/page-")'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  if http.get(module.rooturl .. '/manga-list/page-' .. IncStr(url)) then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//div[@class="comics-grid"]/div/div/h3/a', links, names)
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
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
  return m
end

function Init()
  AddWebsiteModule('HeavenManga', 'https://heavenmanga.ca')
  AddWebsiteModule('HolyManga', 'http://holymanga.ca')
end

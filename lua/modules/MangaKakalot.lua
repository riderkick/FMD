function GetRedirectUrl(document)
  local x = TXQuery.Create(document)
  local s = x.xpathstring('//script[contains(., "window.location.assign")]')
  if (s ~= '') and (s ~= nil) then
    return GetBetween('("', '")', s)
  end
  return ''
end

function getinfo()
  local u = MaybeFillHost(module.rooturl, url)
  if http.get(u) then
    local s = GetRedirectUrl(http.Document)
    if (s ~= '') and (s ~= nil) then
      u = s
      if not http.GET(u) then return false; end
    end
    mangainfo.url=u
    local x=TXQuery.Create(http.document)
    if module.website == 'MangaKakalot' or module.website == 'MangaKakalots' then
      mangainfo.title=x.xpathstring('//ul[@class="manga-info-text"]/li/h1')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="manga-info-pic"]/img/@src'))
      mangainfo.authors=x.xpathstringall('//ul[@class="manga-info-text"]/li[contains(., "Author")]/a')
      mangainfo.genres=x.xpathstringall('//ul[@class="manga-info-text"]/li[contains(., "Genre")]/a')
      mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//ul[@class="manga-info-text"]/li[contains(., "Status")]'))
      mangainfo.summary=x.xpathstringall('//div[@id="noidungm"]/text()', '')
      x.xpathhrefall('//div[@class="chapter-list"]/div[@class="row"]/span/a', mangainfo.chapterlinks, mangainfo.chapternames)
      InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    else
      mangainfo.title=x.xpathstring('//div[@class="story-info-right"]/h1')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//span[@class="info-image"]/img/@src'))
      mangainfo.authors=x.xpathstringall('//td[contains(., "Author(s)")]/following-sibling::td/a')
      mangainfo.genres=x.xpathstringall('//td[contains(., "Genres")]/following-sibling::td/a')
      mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//td[contains(., "Status")]/following-sibling::td'))
      mangainfo.summary=x.xpathstringall('//div[@class="panel-story-info-description"]/text()', '')
      x.xpathhrefall('//ul[@class="row-content-chapter"]/li/a', mangainfo.chapterlinks, mangainfo.chapternames)
      InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    end
    if (Pos('email', mangainfo.title) > 0) and (Pos('protected', mangainfo.title) > 0) then
      mangainfo.title = Trim(x.xpathstring('//title/substring-after(substring-before(., "Manga Online"), "Read")'))
    end
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  function spliturl(u)
    local pos = 0
    for i = 1, 3 do
      local p = string.find(u, '/', pos+1, true)
      if p == nil then break; end
      pos = p
    end
    return string.sub(u, 1, pos-1), string.sub(u, pos)
  end
  task.pagelinks.clear()
  task.pagenumber=0
  local u = MaybeFillHost(module.rooturl, url)
  if http.get(u) then
    local s = GetRedirectUrl(http.document)
    if (s ~= '') and (s ~= nil) then
      local host, _ = spliturl(s)
      local _, path = spliturl(u)
      u = host .. path
      if not http.get(u) then return false; end
    end
    local x=TXQuery.Create(http.Document)
    for _, v in ipairs(x.XPathI('//div[@id="vungdoc"]/img')) do
      if string.find(v.GetAttribute('src'), "log") == nil then
      task.pagelinks.add(v.GetAttribute('src'))
      end
    end
    if task.pagelinks.count == 0 then
      x.xpathstringall('//div[@class="vung_doc"]/img/@src', task.pagelinks)
    end
    if task.pagelinks.count == 0 then
      x.xpathstringall('//div[@class="container-chapter-reader"]/img/@src', task.pagelinks)
    end
    if task.pagelinks.count == 0 then
      x.xpathstringall('//div[@id="vungdoc"]/img/@data-src', task.pagelinks)
    end
    return true
  else
    return false
  end
end

local dirurl = '/manga_list?type=newest&category=all&state=all&page='
local dirs = '/genre-all/'

function getnameandlink()
  if module.website == 'MangaKakalot' or module.website == 'MangaKakalots' then
    if http.get(module.rooturl .. dirurl .. IncStr(url)) then
      local x = TXQuery.Create(http.Document)
      x.XPathHREFAll('//div[@class="truyen-list"]/div[@class="list-truyen-item-wrap"]/h3/a', links, names)
      return no_error
    else
      return net_problem
    end
  else
    if http.get(module.rooturl .. dirs .. IncStr(url)) then
      local x = TXQuery.Create(http.Document)
      x.XPathHREFAll('//div[@class="panel-content-genres"]//div[@class="genres-item-info"]/h3/a', links, names)
      return no_error
    else
      return net_problem
    end
  end
end

function getdirectorypagenumber()
  if module.website == 'MangaKakalot' or module.website == 'MangaKakalots' then
    if http.GET(module.RootURL .. dirurl .. '1') then
      page = tonumber(TXQuery.Create(http.Document).xpathstring('//a[contains(@class, "page_last")]/@href'):match('page=(%d+)'))
      return no_error
    else
      return net_problem
    end
  else
    if http.GET(module.RootURL .. dirs .. '1') then
      page = tonumber(TXQuery.Create(http.Document).xpathstring('//a[contains(@class, "page-last")]/@href'):match('.-//.-/.-/(%d+)'))
      return no_error
    else
      return net_problem
    end
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
  AddWebsiteModule('MangaKakalot', 'https://mangakakalot.com')
  AddWebsiteModule('MangaNelo', 'https://manganelo.com')
  AddWebsiteModule('MangaKakalots', 'https://mangakakalots.com')
end

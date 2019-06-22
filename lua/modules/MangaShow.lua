function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title=x.xpathstring('css("div.manga-subject")')
    end
    local img = x.xpathstring('css("div.manga-thumbnail")/@style')
    img = GetBetween('url(', ')', img)
    mangainfo.coverlink=MaybeFillHost(module.RootURL, img)
    mangainfo.authors=x.xpathstringall('css("a.author")')
    mangainfo.genres=x.xpathstringall('css("div.manga-tags > a")')
    x.xpathhrefall('css("div.chapter-list > div.slot > a")', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    local s=x.xpathstring('//script[contains(., "img_list")]')
    s = GetBetween('var img_list =', ';', s)
    x.parsehtml(s)
    x.xpathstringall('json(*)()', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl .. '/bbs/page.php?hid=manga_list&page=' .. IncStr(url)) then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('css("div.manga-list-gallery div.manga-subject > a")', links, names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. '/bbs/page.php?hid=manga_list') then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('//ul[@class="pagination"]/li[last()]/a/@href'):match('%((%d+)%)$'))
    if page == nil then
      page = 1
    end
    return true
  else
    return false
  end
end

function Init()
  local m = NewModule()
  m.website = 'MangaShow'
  m.rooturl = 'https://mangashow.me'
  m.category = 'Raw'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
end

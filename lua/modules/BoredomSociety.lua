function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('css("div.titlesinfo_top > h2")')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('css("img.titlesinfo_coverimage")/@src'))
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[contains(span, "Status")]/span[@class="titleinfo_infovalue"]')) 
    mangainfo.summary=x.xpathstringall('//div[contains(span, "Description")]/*[@class="titlesinfo_description"]/text()', '')
    x.xpathhrefall('//table[@class="titlesinfo_chaptertable"]//td[3]/a',mangainfo.chapterlinks,mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  http.post(module.rooturl .. '/module/reader/ajax.php', 'readingtype=all')
  http.reset()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//div[@class="reader_mangaimagebox"]/img/@src', task.pagelinks)
    return true
  end
  return false
end

function getnameandlink()
  if http.get(module.rooturl .. '/titles/page/' .. IncStr(url)) then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('css("div.titlelist_name > a")', links, names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. '/titles') then
    x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('//div[@class="titles_pages"]/a[last()-1]'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'BoredomSociety'
  m.rooturl = 'https://www.boredomsociety.xyz'
  m.category = 'English-Scanlation'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
end
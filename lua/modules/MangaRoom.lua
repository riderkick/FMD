function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//div[@class="col-md-12"]/h3')
    end
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('//div[@class="col-md-12"]/div/img[contains(@class, "img")]/@src'))
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//table[contains(@class, "info_list")]//tr[contains(td, "Status")]/td[2]'))
    mangainfo.authors=x.xpathstring('//table[contains(@class, "info_list")]//tr[contains(td, "Author")]/td[2]')
    mangainfo.artists=x.xpathstring('//table[contains(@class, "info_list")]//tr[contains(td, "Artist")]/td[2]')
    mangainfo.genres=x.xpathstringall('//table[contains(@class, "info_list")]//tr[contains(td, "Genres")]/td[2]/a')
    mangainfo.summary=x.xpathstring('//div[contains(@class, "summary")]/p')
    x.xpathhrefall('//table[contains(@class,"chapter-list")]//tr/td[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, '/manga' .. url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//div[@class="page_container"]/img[contains(@class, "manga_page")]/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  local dirurl = string.format('/search?search-words=&status=%d-page-%s', module.CurrentDirectoryIndex, IncStr(url))
  if http.get(module.rooturl..dirurl) then
    local x = TXQuery.Create(http.document)
    if updatelist.CurrentDirectoryPageNumber <= 1 then
      local page = x.xpathstring('//ul[contains(@class,"pagination")]/li[last()-1]/a/@href')
      page = tonumber(page:match('page%-(%d+)'))
      if page == nil then page = 1 end
      updatelist.CurrentDirectoryPageNumber = page
    end
    x.xpathhrefall('//div[@id="content_item"]//div[@class="manga_title"]/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m=NewModule()
  m.category='English'
  m.website='MangaRoom'
  m.rooturl='http://manga-room.com'
  m.lastupdated='April 8, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.totaldirectory=2
end

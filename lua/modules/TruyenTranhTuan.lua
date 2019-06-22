function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//h1[@itemprop="name"]')
    end
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('//*[@class="manga-cover"]/img/@src'))
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//li[contains(@class, "status")]/p[2]'), 'Đang tiến hành', 'Hoàn thành')
    mangainfo.authors=x.xpathstringall('//p[@class="misc-infor" and starts-with(.,"Tác giả")]/a')
    mangainfo.genres=x.xpathstringall('//p[@class="misc-infor" and starts-with(.,"Thể loại")]/a')
    mangainfo.summary=x.xpathstring('//*[@id="manga-summary"]/p')
    x.xpathhrefall('//*[@id="manga-chapter"]//*[@class="chapter-name"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x = TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(.,"var slides_page")]')
    x.parsehtml(GetBetween('slides_page_path = ', ';', s))
    local v = x.xpath('json(*)()')
    if v.count > 0 then
      local tmp = {}
      for i = 1, v.count do
        table.insert(tmp, v.get(i).toString)
      end
      table.sort(tmp)
      for i, link in ipairs(tmp) do
        task.pagelinks.add(link)
      end
    else
      x.parsehtml(GetBetween('slides_page_url_path = ', ';', s))
      local v = x.xpathstringall('json(*)()', task.pagelinks)
    end
  else
    return false
  end
  return true
end

function getnameandlink()
  local s = module.RootURL
  if url ~= '0' then
    s = s .. '/page/' .. IncStr(url)
  end
  if http.get(s) then
    local x = TXQuery.Create(http.Document)
    local p = 1
    local v = x.xpath('//*[@id="page-nav"]//li')
    for i = 1, v.count do
      local tmp = tonumber(v.get(i).toString)
      if tmp == nil then tmp = 1; end
      if tmp > p then p = tmp; end
    end
    updatelist.CurrentDirectoryPageNumber = p
    x.XPathHREFAll('//*[@id="story-list"]/div/span[1]/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m=NewModule()
  m.category='Vietnamese'
  m.website='TruyenTranhTuan'
  m.rooturl='http://truyentranhtuan.com'
  m.lastupdated='June 20, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//h1[@class="tentruyen"]')
    end
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('//div[contains(@class, "wrapper_image")]/img/@src'))
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[contains(@class, "wrapper_info")]/p[contains(., "Trạng thái")]'), 'Đang tiến hành', 'Hoàn thành')
    mangainfo.authors=x.xpathstring('//div[contains(@class, "wrapper_info")]/p[contains(., "Tác giả")]/substring-after(.,":")')
    mangainfo.genres=x.xpathstringall('//div[contains(@class, "wrapper_info")]/p[contains(., "Thể loại")]/a')
    mangainfo.summary=x.xpathstring('//p[@id="tomtattruyen"]')
    x.xpathhrefall('//div[@id="wrapper_listchap"]//section/div/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//div[@id="content_chap"]/img/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  local s = module.RootURL .. string.format('/danhsach/P%s/index.html?sort=1', IncStr(url))
  if http.get(s) then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//a[h5[@class="tentruyen_slide"]]', links, names)
    local maxp = -1
    local v = x.xpath('//ul[@class="pagination"]/li/a/@href')
    for i = 1, v.count do
      local p = tonumber(string.match(v.get(i).toString, '/P(%d+)/'))
      if (p ~= nil) and (p > maxp) then maxp = p; end
    end    
    if maxp > 0 then updatelist.CurrentDirectoryPageNumber = maxp; end    
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m=NewModule()
  m.category='Vietnamese'
  m.website='HamTruyen'
  m.rooturl='https://hamtruyen.com'
  m.lastupdated='June 15, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = Trim(x.XPathString('//div[@class="post-title"]/h3'))
      if string.match(mangainfo.title:upper(), ' RAW$') ~= nil then
        mangainfo.title = mangainfo.title:sub(1, -5)
      end
    end
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('//div[@class="summary_image"]/a/img/@data-src'))
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[contains(div[@class="summary-heading"]/h5, "Status")]/div[@class="summary-content"]'))
    mangainfo.authors=x.xpathstringall('//div[@class="author-content"]/a')
    mangainfo.artists=x.xpathstringall('//div[@class="artist-content"]/a')
    mangainfo.genres=x.xpathstringall('//div[@class="genres-content"]/a')
    mangainfo.summary=x.xpathstring('//div[@class="description-summary"]/div/p')
    x.xpathhrefall('//div[contains(@class, "listing-chapters")]/ul/li/a', mangainfo.chapterlinks, mangainfo.chapternames)
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
    x.xpathstringall('//img[contains(@class, "wp-manga-chapter-img")]/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.RootURL .. '/manga/page/' .. IncStr(url) .. '/?m_orderby=alphabet') then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//div[@class="page-item-detail"]/div/a')
    for i = 1, v.count do
      local v1 = v.get(i)
      local s = v1.getAttribute('title')
      if string.match(s:upper(), ' RAW$') ~= nil then
        s = s:sub(1, -5)
      end
      names.add(s)
      links.add(v1.getAttribute('href'))
    end
    local p = x.xpathstring('//div[contains(@class, "nav-previous")]/a/@href')
    p = tonumber(p:match('page/(%d+)/'))
    if p ~= nil then
      updatelist.CurrentDirectoryPageNumber = p;
    end
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(name, url)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = 'Raw'
  m.lastupdated = 'April 9, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  return m
end 

function Init()
  AddWebsiteModule('MangaAll', 'http://mangaall.com')
  AddWebsiteModule('MangaTrue', 'http://mangatrue.com')
end

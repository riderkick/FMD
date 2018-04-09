function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = Trim(SeparateLeft(x.XPathString('//title'), '- Raw'))
    end
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('//img[@class="thumbnail"]/@src'))
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//ul[@class="manga-info"]/li[contains(., "Status")]//a'))
    mangainfo.authors=x.xpathstring('//ul[@class="manga-info"]/li[contains(., "Author")]//a')
    mangainfo.genres=x.xpathstringall('//ul[@class="manga-info"]/li[contains(., "Genre")]//a')
    mangainfo.summary=x.xpathstring('//h3[text()="Description"]/following-sibling::p')
    x.xpathhrefall('//div[@id="tab-chapper"]//table/tbody/tr/td/a', mangainfo.chapterlinks, mangainfo.chapternames)
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
    x.xpathstringall('//img[@class="chapter-img"]/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.RootURL .. '/manga-list.html?listType=allABC') then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//span[@manga-slug]//a')
    for i = 1, v.count do
      local v1 = v.get(i)
      names.Add(Trim(SeparateLeft(v1.toString, '- Raw')));
      links.Add(v1.getAttribute('href'));
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m=NewModule()
  m.category='Raw'
  m.website='Lhscans'
  m.rooturl='http://rawlh.com'
  m.lastupdated='April 9, 2018'
  m.totaldirectory=1
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

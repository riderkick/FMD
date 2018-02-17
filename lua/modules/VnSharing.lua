function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.Document)
    mangainfo.title=x.XPathString('//p[@class="title"]')
    mangainfo.coverLink=MaybeFillHost(module.rooturl, GetBetween("url('", "')", x.XPathString('//div[contains(@class, "info_ava")]/@style')))
    mangainfo.status = MangaInfoStatusIfPos(x.XPathString('//div[@class="info"]/span[contains(., "Status")]/a'), 'Đang tiến hành', 'Đang phát hành')
    mangainfo.authors = x.XPathStringAll('//div[@id="manga_detail"]/ul/li[contains(b, "Tác giả")]/text()', '')
    mangainfo.artists = x.XPathStringAll('//div[@class="info"]/span[contains(., "Artist")]/a')
    mangainfo.genres = x.XPathStringAll('//div[@id="manga_detail"]/ul/li[contains(b, "Thể loại")]/a')
    mangainfo.summary = x.XPathString('//p[@class="desc"]')
    x.xpathhrefall('//ul[@id="manga-info-list"]/li[not(contains(@style, "none"))]/a[1]', mangainfo.chapterlinks,mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)    
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  -- TODO: dynamic page count
  if http.GET(module.RootURL .. '/index/KhamPha/newest') then
    x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('(//div[@class="pagination_wrap"]/a)[last()-1]'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    x.xpathstringall('//img[@id="manga_page"]/@src', task.pagelinks)
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if http.get(module.rooturl..'/index/KhamPha/newest/'..IncStr(url)) then
    x=TXQuery.Create(http.Document)
    x.xpathhrefall('//ul[@id="browse_result_wrap"]/li[@class="browse_result_item"]/a[@class="title"]', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Vietnamese'
  m.website='VnSharing'
  m.rooturl='http://truyen.vnsharing.site'
  m.sortedlist=true
  m.lastupdated='February 16, 2018'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
end 
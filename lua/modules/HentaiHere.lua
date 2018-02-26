function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.Document)
    mangainfo.title=x.XPathString('//h4/a/text()')
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('//div[@id="cover"]//img/@src'))
    mangainfo.artists = x.XPathString('//div[@id="info"]/div[contains(span, "Artist")]/a')
    mangainfo.genres = x.XPathStringAll('//div[@id="info"]/div[contains(span, "Category")]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@id="info"]/div[contains(span, "Status")]/a'))
    mangainfo.summary=x.XPathStringall('//div[@id="info"]/div[contains(span, "Summary")]/text()', '')
    v=x.xpath('//ul[@class="arf-list"]/li/a')
    for i=1,v.count do
      v1=v.get(i)
      mangainfo.chapterlinks.add(v1.getattribute('href'))
      mangainfo.chapternames.add(x.xpathstring('./span/text()', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    s=x.xpathstring('//script[contains(.,"rff_imageList")]')
    s=GetBetween('rff_imageList =', ';', s)
    x.parsehtml(s)
    v=x.xpath('json(*)()')
    for i=1,v.count do
      v1=v.get(i)
      print(v1.toString)
      task.pagelinks.add('https://hentaicdn.com/hentai' .. v1.toString)
    end
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if http.get(module.rooturl..'/directory/newest?page='..IncStr(url)) then
    x=TXQuery.Create(http.Document)
    v=x.XPathHREFAll('//div[contains(@class, "seriesBlock")]/div/div[2]/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL..'/directory/newest') then
    x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('(//ul[contains(@class,"pagination")]/li/a)[last()-1]'))
    if page == nil then page = 1 end
    return true
  else
    return false
  end
end

function Init()
  m=NewModule()
  m.category='H-Sites'
  m.website='HentaiHere'
  m.rooturl='https://hentaihere.com'
  m.sortedlist=true
  m.lastupdated='February 15, 2018'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
end 
function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.Document)
    mangainfo.title=x.XPathString('//*[@class="gallery-info"]//*[@class="title"]')
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('//*[@class="cover"]//img/@src'))
    mangainfo.artists = x.XPathString('//*[@class="gallery-info"]//*[@class="table"]//tr/td[starts-with(.,"Artist")]/following-sibling::td[1]/string-join(.//a,", ")')
    mangainfo.genres = x.XPathString('//*[@class="gallery-info"]//*[@class="table"]/string-join(.//tr/td//a,", ")')
    mangainfo.chapterlinks.add(x.XPathString('//a[@class="read-more"]/@href'))
    mangainfo.chapternames.add(mangainfo.title)
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    x.XPathStringAll('json(//script[contains(.,"var chapters")]/substring-before(substring-after(.,"= "),";"))//image', task.pagelinks)
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if http.get(module.rooturl..'/browse/newest?page='..IncStr(url)) then
    x=TXQuery.Create(http.Document)
    v=x.xpath('//*[@class="gallery-listing"]/*[@class="row"]/a')
    for i=1,v.count do
      v1 = v.get(i)
      links.add(v1.getAttribute('href'))
      names.add(x.xpathstring('.//*[@class="title"]/text()[1]', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL) then
    x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('//*[@class="pagination"]/li[last()-1]'))
    if page == nil then page = 1 end
    return true
  else
    return false
  end
end

function Init()
  m=NewModule()
  m.category='H-Sites'
  m.website='Pururin'
  m.rooturl='http://pururin.io'
  m.sortedlist=true
  m.lastupdated='February 14, 2018'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
end 
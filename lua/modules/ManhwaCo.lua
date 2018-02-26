function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.Document)
    mangainfo.title=x.XPathString('//h4[@class="card-title"]')
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('//img[contains(@class, "card-img-top")]/@src'))
    mangainfo.summary = x.XPathString('//p[@class="card-text"]')
    mangainfo.genres = x.XPathStringAll('//div[@class="chip"]')
    v=x.xpath('//div[contains(@class, "list-group")]/a')
    for i=1,v.count do
      v1=v.get(i)
      mangainfo.chapterlinks.add(v1.getAttribute('href'))
      mangainfo.chapternames.add(x.xpathstring('./text()', v1))
    end
    InvertStrings(mangainfo.chapterLinks, mangainfo.chapterNames)
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  task.pagelinks.clear()
  task.pagenumber=0
  if http.get(MaybeFillHost(module.rooturl,url)) then  
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//div[@class="container"]/div/div/img/@src', task.pagelinks)
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if http.get(module.rooturl..'/series') then
    x=TXQuery.Create(http.Document)
    x.xpathstringall('//div[@class="container"]/div[@class="row"]//h5', names)
    x.xpathstringall('//div[@class="container"]//div[contains(@class,"card")]//div/a/@href', links)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  page=1
  return no_error
end

function Init()
  m=NewModule()
  m.category='English-Scanlation'
  m.website='ManhwaCo'
  m.rooturl='https://manhwa.co'
  m.lastupdated='February 15, 2018'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
end 
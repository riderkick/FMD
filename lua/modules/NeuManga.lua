function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.Document)
    mangainfo.title=x.XPathString('//h1[@class="manga1"]/text()')
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('//img[@class="imagemg"]/@src'))
    mangainfo.status = MangaInfoStatusIfPos(x.XPathString('//div[@class="info"]/span[contains(., "Status")]/a'))
    mangainfo.authors = x.XPathStringAll('//div[@class="info"]/span[contains(., "Author")]/a')
    mangainfo.artists = x.XPathStringAll('//div[@class="info"]/span[contains(., "Artist")]/a')
    mangainfo.genres = x.XPathStringAll('//div[@class="info"]/span[contains(., "Genre")]/a')
    mangainfo.summary = x.XPathStringAll('//div[@class="summary"]/text()', '')
    v=x.xpath('//div[@id="scans"]/div[1]/div[@class="item-content"]/a')
    for i=1, v.count do
        v1=v.get(i)
        mangainfo.chapterlinks.add(v1.getAttribute('href'))
        mangainfo.chapternames.add(x.xpathstringall('text()', '', v1))
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)    
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  http.cookies.values['age_confirmed'] = '1'
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    task.pagenumber=x.xpath('(//select[@class="page"])[1]/option').count
    return true
  else
    return false
  end
end

function GetImageURL()
  s=url:gsub('%?.+$','')..'/'..tostring(workid+1)
  http.cookies.values['age_confirmed'] = '1'
  if http.get(MaybeFillHost(module.rooturl,s)) then
    x=TXQuery.Create(http.Document)
    task.pagelinks[workid]=x.xpathstring('//img[@class="imagechap"]/@src')
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if http.get(module.rooturl..'/manga') then
    x=TXQuery.Create(http.Document)
    x.xpathhrefall('//div[@class="alplist"]/li/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Indonesian'
  m.website='NeuManga'
  m.rooturl='http://neumanga.tv'
  m.lastupdated='February 12, 2018'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetimageurl='GetImageURL'
  m.ongetnameandlink='GetNameAndLink'
end 
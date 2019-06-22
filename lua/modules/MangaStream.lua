function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.Document)
    mangainfo.title=x.XPathString('//h1')
    x.xpathhrefall('//table//td/a',mangainfo.chapterlinks,mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)    
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    task.pagenumber=tonumber(x.xpathstring('//div[contains(@class,"btn-reader-page")]/ul[@class="dropdown-menu"]/li[last()]/substring-before(substring-after(.,"("),")")'))
    return true
  else
    return false
  end
end

function GetImageURL()
  if http.get(MaybeFillHost(module.rooturl,url):gsub('/1$','')..'/'..tostring(workid+1)) then
    x=TXQuery.Create(http.Document)
    task.pagelinks[workid]=x.xpathstring('//img[@id="manga-page"]/@src')
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if http.get(module.rooturl..'/manga') then
    x=TXQuery.Create(http.Document)
    x.xpathhrefall('//table//tr/td[1]//a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='English-Scanlation'
  m.website='MangaStream'
  m.rooturl='https://readms.net'
  m.lastupdated='February 8, 2018'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetimageurl='GetImageURL'
  m.ongetnameandlink='GetNameAndLink'
end

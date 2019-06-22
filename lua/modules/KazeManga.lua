function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.Document)
    mangainfo.title=x.XPathString('//h1[@itemprop="headline"]'):gsub(' Bahasa Indonesia$', '')
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('css("div.animeinfo img")/@src'))
    mangainfo.authors = x.XPathStringAll('css("div.animeinfo > span")//span[contains(b, "Author")]/substring-after(text(),":")','')
    mangainfo.summary = x.XPathString('css("div.sinopx > p")')
    mangainfo.genres = x.XPathStringAll('css("div.animeinfo > span")//span[contains(b, "Genre")]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('css("div.animeinfo > span")//span[contains(b, "Status")]'))
    x.xpathhrefall('css("div.epl > ul > li a")', mangainfo.chapterlinks, mangainfo.chapternames)
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
    x.xpathstringall('css("div.badan img")/@src', task.pagelinks)
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if http.get(module.rooturl..'/komik-list/') then
    local x=TXQuery.Create(http.Document)
    x.xpathhrefall('css(".anime-list2 .a-z > li > a")', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Indonesian'
  m.website='KazeManga'
  m.rooturl='https://kazemanga.xyz'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
end

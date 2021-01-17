function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.Document)
    if mangainfo.title == '' then
      mangainfo.title=x.XPathString('//h1[@itemprop="headline"]')
      mangainfo.title=mangainfo.title:gsub('^Baca Manga','')
      mangainfo.title=mangainfo.title:gsub('Bahasa Indonesia$','')
      mangainfo.title=mangainfo.title:gsub('Manga','')
	    mangainfo.title=mangainfo.title:gsub('^Manga','')
	    mangainfo.title=mangainfo.title:gsub('Manga$','')
    end
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('//img[@class="imagemg"]/@src'))
    mangainfo.status = MangaInfoStatusIfPos(x.XPathString('//span[@class="hentry"]/span[contains(., "Status")]/a'))
    mangainfo.authors = x.XPathStringAll('//span[@class="hentry"]/span[contains(., "Author")]/a')
    mangainfo.artists = x.XPathStringAll('//span[@class="hentry"]/span[contains(., "Artist")]/a')
    mangainfo.genres = x.XPathStringAll('//span[@class="hentry"]/span[contains(., "Genre")]/a')
    mangainfo.summary = x.XPathStringAll('//div[contains(@class,"summary")]/text()', '')
    local v=x.xpath('//div[@id="scans"]//div[@class="item-content"]/a')
    for i=1, v.count do
        local v1=v.get(i)
        mangainfo.chapterlinks.add(v1.getAttribute('href'))
        mangainfo.chapternames.add(x.xpathstring('h3', v1))
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)    
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  http.cookies.values['age_confirmed'] = '1'
  local u = AppendURLDelim(MaybeFillHost(module.rooturl,url)) .. '_/1'
  if http.get(u) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//img[@class="imagechap"]/@data-src', task.pagelinks)
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
  m.rooturl='https://neumanga.tv'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
end 

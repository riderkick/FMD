function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
	mangainfo.title=x.xpathstring('//h1[@itemprop="headline"]')
	mangainfo.title = string.gsub(mangainfo.title, 'Bahasa Indonesia', '')
	mangainfo.title = string.gsub(mangainfo.title, 'Baca', '')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="imgprop"]/img/@src'))
    mangainfo.authors=x.xpathstring('//div[@class="mtb"]//tr[contains(th, "Author")]/td')
    mangainfo.genres=x.xpathstringall('//div[@class="mtb"]//tr[contains(th, "Genres")]/td')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="mtb"]//tr[contains(th, "Status")]/td'), "Ongoing", "Completed")
    mangainfo.summary=x.xpathstringall('//*[@class="desc"]/p/text()', '')
	x.xpathhrefall('//span[@class="lchx"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagenumber=0
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]//img/@src', task.pagelinks)
    return true
  else
    return false
  end
end

function getnameandlink()
  if http.get(module.rooturl .. '/daftar-manga/?list') then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//*[@class="blix"]//a',links,names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m=NewModule()
  m.category='Indonesian'
  m.website='MangaID'
  m.rooturl='https://mangaid.me'
  m.lastupdated='September 3, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

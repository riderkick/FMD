function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
	mangainfo.title=x.xpathstring('//div[@class="list-group"]/span[starts-with(.,"Otros nombres")]/substring-after(., ":")')
	mangainfo.title=string.gsub(mangainfo.title, '(Manga)', '')
	mangainfo.title=string.gsub(mangainfo.title, '()', '')
	mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="list-group"]/div/img/@src'))
    mangainfo.genres=x.xpathstring('//div[@class="list-group"]/span[starts-with(.,"Categorías")]/substring-after(., ":")')
	mangainfo.genres=string.gsub(mangainfo.genres, '  ', '')
	mangainfo.genres=string.gsub(mangainfo.genres, ',', ', ')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="list-group"]/span[starts-with(.,"Estado")]/substring-after(., ":")'), "Ongoing", "Completed")
    mangainfo.summary=x.xpathstringall('//div[@class="list-group"]/span[starts-with(.,"Resumen")]/substring-after(., ":")', '')
	x.xpathhrefall('//div[@class="capitulos-list scroll-fb"]/table/tbody/tr/td/a', mangainfo.chapterlinks, mangainfo.chapternames)
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
    TXQuery.Create(http.Document).xpathstringall('//*[@id="all"]//img/@data-src', task.pagelinks)
    return true
  else
    return false
  end
end

function getnameandlink()
  if http.get(module.rooturl .. '/changeMangaList?type=text') then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//*[@class="manga-list"]//li/a',links,names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m=NewModule()
  m.category='Spanish'
  m.website='LeoManga'
  m.rooturl='https://leomanga.xyz'
  m.lastupdated='May 1, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

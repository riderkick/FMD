function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
	mangainfo.chapterlinks.clear()
    mangainfo.chapternames.clear()
	if module.website == 'LeoManga' then
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
	else
	   mangainfo.title=x.xpathstring('//h2[@class="listmanga-header"]')
	   mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//*[@class="img-responsive"]/@src'))
	   mangainfo.genres=x.xpathstringall('//*[@class="tag-links"]/a')
	   mangainfo.summary=x.xpathstring('//*[@class="manga well"]')
	   mangainfo.summary=string.gsub(mangainfo.summary, 'Summary', '')
	   x.xpathhrefall('//*[@class="chapters"]/li/h5/a', mangainfo.chapterlinks, mangainfo.chapternames)
	   InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
	end
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

function AddWebsiteModule(site, url, cat)
  local m=NewModule()
  m.category=cat
  m.website=site
  m.rooturl=url
  m.lastupdated = 'June 06, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  return m
end

function Init()

local cat = 'Spanish'
    AddWebsiteModule('LeoManga', 'https://leomanga.xyz', cat)

      cat = 'English'
    AddWebsiteModule('ReadComicsOnlineRU', 'https://readcomicsonline.ru', cat)
end

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
	
    mangainfo.title=x.xpathstring('//title/text()')
	  :gsub('Baca', ''):gsub('Online Komik', ''):gsub('Komik -', ''):gsub('Komik', ''):gsub('KOMIK', '')
	  :gsub('komik', ''):gsub('Manga -', ''):gsub('Manga', ''):gsub('Online -', ''):gsub('Terbaru', '')
	  :gsub('Bahasa', ''):gsub('Indonesia', ''):gsub('bahasa', ''):gsub('indonesia', '')
	  :gsub('BAHASA', ''):gsub('INDONESIA', ''):gsub('terbaru', ''):gsub('^%s*[–/]', '')

    mangainfo.coverlink=x.xpathstring('//a[@imageanchor]/img/@src')
    mangainfo.authors=x.xpathstring('//*[contains(., ("Author","Penulis"))]/following-sibling::text()'):gsub("^:", "")
	mangainfo.artists=x.xpathstring('//*[contains(., ("Artist","Seniman"))]/following-sibling::text()'):gsub("^:", "")
    mangainfo.genres=x.xpathstring('//*[contains(., "Genre")]/following-sibling::text()'):gsub("^:", "")
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//*[contains(., ("Episodes","Status"))]/following-sibling::text()'))
	mangainfo.summary = x.xpathstring('//*[contains(., "Sinopsis")]/following-sibling::text()'):gsub("^:", "")
    x.xpathhrefall('//div[contains(@style, "-moz-border-radius")]//a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    x.xpathstringall('//div[@class="separator"]/a/img/@src', task.pagelinks)
    if task.pagelinks.count == 0 then
      x.xpathstringall('//img[./@*[starts-with(name(), "data-original")]]/@src', task.pagelinks)
    end
    if task.pagelinks.count == 0 then
      if http.get(MaybeFillHost('http://mangaku.co',url)) then
        x=TXQuery.Create(http.Document)
        x.xpathstringall('//div[@class="separator"]/a/img/@src', task.pagelinks)
        return true
      else
        return false
      end
    else
      return true
    end
  else
    return false
  end
end

function getnameandlink()
  if http.get(module.rooturl..'/daftar-komik-bahasa-indonesia/') then
    TXQuery.Create(http.document).xpathhrefall('//a[@class="screenshot"]',links,names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Indonesian'
  m.website='MangaKu'
  m.rooturl='http://mangaku.in'
  m.lastupdated='July 10, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end 
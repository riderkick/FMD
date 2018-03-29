function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    if module.website == 'KomikCast' then
      mangainfo.title=x.xpathstring('//div[@class="mangainfo"]/h1')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="topinfo"]/img/@src'))
      mangainfo.authors=x.xpathstring('//div[@class="topinfo"]//tr[contains(th, "Author")]/td')
      mangainfo.genres=x.xpathstringall('//div[@class="topinfo"]//tr[contains(th, "Genres")]/td/a')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="topinfo"]//tr[contains(th, "Status")]/td'))
      mangainfo.summary=x.xpathstringall('//*[@class="sin"]/p/text()', '')
    else
      mangainfo.title=x.xpathstring('//h1[@itemprop="name"]')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="imgdesc"]/img/@src'))
      mangainfo.authors=x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Author")]/substring-after(.,":")')
      mangainfo.genres=x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Genre")]/substring-after(.,":")')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Status")]'))
      mangainfo.summary=x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")')
    end
    x.xpathhrefall('//div[@class="cl"]//li/span[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
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
  local dirurl = '/manga-list/'
  if module.website == 'MangaShiro' then dirurl = '/daftar-manga/'
  elseif module.website == 'KomikStation' then dirurl = '/daftar-komik/'
  elseif module.website == 'KomikCast' then dirurl = '/daftar-komik/?list'
  elseif module.website == 'MangaKid' then dirurl = '/manga-lists/'
  end
  if http.get(module.rooturl..dirurl) then
    if module.website == 'KomikStation' then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="daftarkomik"]//a',links,names)
    else
      TXQuery.Create(http.document).xpathhrefall('//*[@class="soralist"]//a',links,names)
    end
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(site, url)
  local m=NewModule()
  m.category='Indonesian'
  m.website=site
  m.rooturl=url
  m.lastupdated='February 21, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  return m
end

function Init()
  AddWebsiteModule('MangaShiro', 'http://mangashiro.net')
  AddWebsiteModule('Subapics', 'http://subapics.com')
  AddWebsiteModule('MangaKita', 'http://www.mangakita.net')
  AddWebsiteModule('Mangavy', 'https://mangavy.com')
  AddWebsiteModule('KomikStation', 'http://www.komikstation.com')
  AddWebsiteModule('MangaKid', 'http://mangakid.net')
  AddWebsiteModule('KomikCast', 'https://komikcast.com')
end

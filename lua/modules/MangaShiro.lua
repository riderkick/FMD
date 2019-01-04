function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    x.xpathhrefall('//div[@class="cl"]//li/span[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    if module.website == 'KomikCast' then
      mangainfo.title=x.xpathstring('//div[@class="mangainfo"]/h1')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="topinfo"]/img/@src'))
      mangainfo.authors=x.xpathstring('//div[@class="topinfo"]//tr[contains(th, "Author")]/td')
      mangainfo.genres=x.xpathstringall('//div[@class="topinfo"]//tr[contains(th, "Genres")]/td/a')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="topinfo"]//tr[contains(th, "Status")]/td'))
      mangainfo.summary=x.xpathstringall('//*[@class="sin"]/p/text()', '')
    elseif module.website == 'WestManga' then
      mangainfo.title=x.xpathstring('//h1')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="naru"]/img/@src'))
      mangainfo.authors=x.xpathstring('//div[@class="infozin"]//li[starts-with(.,"Author")]/substring-after(.,":")')
      mangainfo.genres=x.xpathstringall('//div[@class="infozin"]//li[starts-with(.,"Genre")]/a')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="infozin"]//li[starts-with(.,"Status")]'), "Publishing", "Finished")
      mangainfo.summary=x.xpathstringall('//*[@class="sinopc"]/p/text()', '')
    elseif module.website == 'MangaShiro' or module.website == 'Kiryuu' then
      mangainfo.title=x.xpathstring('//h1[@itemprop="headline"]')
      local img = x.xpathstring('//div[@itemprop="image"]/img/@data-lazy-src')
      if img == '' then
        img = x.xpathstring('//div[@itemprop="image"]/img/@src')
      end
      mangainfo.coverlink=MaybeFillHost(module.RootURL, img)
      mangainfo.authors=x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Author")]/substring-after(.,":")')
      mangainfo.genres=x.xpathstringall('//div[contains(@class,"animeinfo")]/div[@class="gnr"]/a')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Status")]'))
      mangainfo.summary=x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")')
      mangainfo.chapterlinks.clear()
      mangainfo.chapternames.clear()
      x.xpathhrefall('//div[@class="bxcl"]//li//div[@class="lch"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    else
      mangainfo.title=x.xpathstring('//h1[@itemprop="name"]')
      if mangainfo.title == '' then
        mangainfo.title=x.xpathstring('//h1'):gsub('Bahasa Indonesia$', '')
      end
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="imgdesc"]/img/@src'))
      mangainfo.authors=x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Author")]/substring-after(.,":")')
      mangainfo.genres=x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Genre")]/substring-after(.,":")')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Status")]'))
      mangainfo.summary=x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")')
    end    
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
    if module.website == 'WestManga' then
      TXQuery.Create(http.Document).xpathstringall('//*[@class="lexot"]//img/@src', task.pagelinks)
    else
      TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]//img/@data-lazy-src', task.pagelinks)
      if task.pagelinks.count < 1 then
        TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]//img/@src', task.pagelinks)
      end
    end
    return true
  else
    return false
  end
end

function getnameandlink()
  local dirs = {
    ['MangaShiro'] = '/daftar-manga/?list',
    ['KomikStation'] = '/daftar-komik/',
    ['KomikCast'] = '/daftar-komik/?list',
    ['MangaKid'] = '/manga-lists/',
    ['Kiryuu'] = '/manga-lists/?list',
  }
  local dirurl = '/manga-list/'
  if dirs[module.website] ~= nil then
    dirurl = dirs[module.website]
  end
  if http.get(module.rooturl..dirurl) then
    if module.website == 'KomikStation' then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="daftarkomik"]//a',links,names)
    elseif module.website == 'WestManga' then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="jdlbar"]//a',links,names)
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
  AddWebsiteModule('MangaShiro', 'https://mangashiro.net')
  AddWebsiteModule('MangaKita', 'http://www.mangakita.net')
  AddWebsiteModule('KomikStation', 'https://www.komikstation.com')
  AddWebsiteModule('MangaKid', 'http://mgku.net')
  AddWebsiteModule('KomikCast', 'https://komikcast.com')
  AddWebsiteModule('WestManga', 'https://westmanga.info')
  AddWebsiteModule('Kiryuu', 'https://kiryuu.co')
  AddWebsiteModule('KomikOtaku', 'https://komikotaku.net')
end

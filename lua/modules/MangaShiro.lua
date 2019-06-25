function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    x.xpathhrefall('//div[@class="cl"]//li/span[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    if module.website == 'KomikCast' then
      mangainfo.title=x.xpathstring('//div[@class="infox"]/h1')
      mangainfo.title = string.gsub(mangainfo.title, 'Bahasa Indonesia', '')
      mangainfo.title = string.gsub(mangainfo.title, 'Baca', '')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="thumb"]/img/@src'))
      mangainfo.authors=x.xpathstringall('//div[@class="spe"]//span[starts-with(.,"Author:")]/substring-after(.,":")')
      mangainfo.genres=x.xpathstringall('//div[@class="spe"]//span[starts-with(.,"Genres:")]/substring-after(.,":")')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="spe"]//span[starts-with(.,"Status:")]/substring-after(.,":")'))
      mangainfo.summary=x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")')
      mangainfo.summary = string.gsub(mangainfo.summary, 'Sinopsis', '')
    elseif module.website == 'MangaShiro'
      or module.website == 'Kiryuu'
      or module.website == 'MangaIndoNet'
      or module.website == 'KomikIndo'
	  or module.website == 'Maid'
    then
      mangainfo.title=x.xpathstring('//h1[@itemprop="headline"]')
      local img = x.xpathstring('//div[@itemprop="image"]/img/@data-lazy-src')
      if img == '' then
        img = x.xpathstring('//div[@itemprop="image"]/img/@src')
      end
      mangainfo.coverlink=MaybeFillHost(module.RootURL, img)
      mangainfo.authors=x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Author")]/substring-after(.,":")')
	  if mangainfo.authors == '' then
		mangainfo.authors=x.xpathstring('//*[@class="anf"]//li[starts-with(.,"Author")]/substring-after(.,":")')
	  end
      mangainfo.genres=x.xpathstringall('//div[contains(@class,"animeinfo")]/div[@class="gnr"]/a')
	  if mangainfo.genres == '' then
		mangainfo.genres=x.xpathstringall('//div[@class="gnr"]/a')
	  end
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Status")]'))
	  if mangainfo.status == '' then
		mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//*[@class="anf"]//li[starts-with(.,"Status")]/substring-after(.,":")'))
	  end
      mangainfo.summary=x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")')
	  if mangainfo.summary == '' then
		mangainfo.summary=x.xpathstringall('//*[@class="sinopsis"]/string-join(.//text(),"")')
	  end
      mangainfo.chapterlinks.clear()
      mangainfo.chapternames.clear()
      if module.website == 'Kiryuu' then
        x.xpathhrefall('//li//span[@class="leftoff"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
	  elseif module.website == 'Maid' then
	    x.xpathhrefall('//div[@class="bxcl"]//li//*[@class="lchx"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
      else
        x.xpathhrefall('//div[@class="bxcl"]//li//div[@class="lch"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
      end
    elseif module.website == 'Mangacan' then
      mangainfo.title=x.xpathstring('//h1/substring-before(.," Indonesia|Baca")')
      mangainfo.chapterlinks.clear()
      mangainfo.chapternames.clear()
      local v = x.xpath('//table[@class="updates"]//td/a')
      for i = 1, v.count do
        local v1 = v.get(i)
        local s = v1.getAttribute('href')
        s = string.gsub(s, '-1.htm', '.htm')
        mangainfo.chapternames.Add(Trim(SeparateLeft(v1.toString, '')));
        mangainfo.chapterlinks.Add(s);
      end
    elseif module.website == 'KomikMama' then
      mangainfo.title=x.xpathstring('//h1[@itemprop="headline"]')
      mangainfo.title=string.gsub(mangainfo.title, 'Manga', '')
      local img = x.xpathstring('//div[@itemprop="image"]/img/@data-lazy-src')
      if img == '' then
        img = x.xpathstring('//div[@itemprop="image"]/img/@src')
      end
      mangainfo.coverlink=MaybeFillHost(module.RootURL, img)
      mangainfo.authors=x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Pengarang")]/substring-after(.," ")')
      mangainfo.genres=x.xpathstringall('//div[contains(@class,"mrgn animeinfo")]/div[@class="gnr"]/a')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Status")]/substring-after(.," ")'))
      mangainfo.summary=x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")')
      mangainfo.chapterlinks.clear()
      mangainfo.chapternames.clear()
      x.xpathhrefall('//div[@class="bxcl nobn"]//li//div[@class="lch"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
      if mangainfo.chapterlinks.count == 0 then
        x.xpathhrefall('//div[@class="bxcl"]//li//div[@class="lch"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
      end
    elseif module.website == 'MangaIndo' then
      mangainfo.title=x.xpathstring('//*[@class="title"]/h2')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@id="m-cover"]/img/@src'))
      mangainfo.authors=x.xpathstring('//span[@id="m-author"]')
      mangainfo.genres=x.xpathstringall('//span[@id="m-genre"]')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//span[@id="m-status"]'))
      mangainfo.summary=x.xpathstring('//*[@id="m-synopsis"]/string-join(.//text(),"")')
      mangainfo.chapterlinks.clear()
      mangainfo.chapternames.clear()
      x.xpathhrefall('//ul[@class="lcp_catlist"]//li/a', mangainfo.chapterlinks, mangainfo.chapternames)
    elseif module.website == 'PecintaKomik' then
      mangainfo.title=x.xpathstring('//h1[@itemprop="headline"]/*')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@itemprop="image"]/img/@src'))
      mangainfo.authors=x.xpathstring('//table[@class="listinfo"]//tr[contains(th, "Penulis")]/td')
      mangainfo.genres=x.xpathstringall('//table[@class="listinfo"]//tr[contains(th, "Genre")]/td/a')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//table[@class="listinfo"]//tr[contains(th, "Status")]/td'))
      mangainfo.summary=x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")')
      mangainfo.chapterlinks.clear()
      mangainfo.chapternames.clear()
      x.xpathhrefall('//div[@class="bxcl"]//li//*[@class="lchx"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    elseif module.website == 'WestManga' then
      mangainfo.title=x.xpathstring('//div[@class="mangainfo"]/h1')
      mangainfo.title = string.gsub(mangainfo.title, 'Bahasa Indonesia', '')
      mangainfo.title = string.gsub(mangainfo.title, 'Baca', '')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="mangainfo"]//div[@class="topinfo"]/img/@src'))
      mangainfo.authors=x.xpathstring('//table[@class="attr"]//tr[contains(th, "Author")]/td')
      mangainfo.genres=x.xpathstringall('//table[@class="attr"]//tr[contains(th, "Genres")]/td/a')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//table[@class="attr"]//tr[contains(th, "Status")]/td'))
      mangainfo.summary = x.xpathstring('//*[@class="sin"]/p')
      mangainfo.summary = string.gsub(mangainfo.summary, 'Sinopsis', '')
      mangainfo.chapterlinks.clear()
      mangainfo.chapternames.clear()
      x.xpathhrefall('//li//span[@class="leftoff"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    elseif module.website == 'Kyuroku' or 
	       module.website == 'BacaManga' or
           module.website == 'MangaCeng' then
      mangainfo.title=x.xpathstring('//div[@class="infox"]/h1')
      mangainfo.title = string.gsub(mangainfo.title, 'Bahasa Indonesia', '')
      mangainfo.title = string.gsub(mangainfo.title, 'Baca', '')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@itemprop="image"]/img/@src'))
      mangainfo.authors=x.xpathstring('//div[@class="spe"]//span[starts-with(.,"Author")]/substring-after(.,":")')
      if module.website == 'BacaManga' then
        mangainfo.genres=x.xpathstringall('//div[@class="spe"]//span[starts-with(.,"Genre")]/a')
      elseif module.website == 'MangaCeng' then
	    mangainfo.genres=x.xpathstringall('//div[@class="spe"]//span[starts-with(.,"Genres")]/a')
	  else
        mangainfo.genres=x.xpathstringall('//div[@class="genrex"]/a')
      end
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="spe"]//span[starts-with(.,"Status")]'))
      mangainfo.summary=x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")')
      mangainfo.chapterlinks.clear()
      mangainfo.chapternames.clear()
      x.xpathhrefall('//div[@class="bixbox bxcl"]//li//*[@class="lchx"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    elseif module.website == 'Komiku'
      or module.website == 'OtakuIndo'
    then
      mangainfo.title=x.xpathstring('//div[@class="info1"]/*')
      mangainfo.title = string.gsub(mangainfo.title, 'Bahasa Indonesia', '')
      mangainfo.title = string.gsub(mangainfo.title, 'Manga', '')
      mangainfo.title = string.gsub(mangainfo.title, 'Komik', '')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="imgseries"]/img/@src'))
      mangainfo.authors=x.xpathstring('//div[@class="preview"]//li[starts-with(.,"Komikus")]/substring-after(.,":")')
      mangainfo.genres=x.xpathstringall('//ul[@class="genre"]/li')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@class="preview"]//li[starts-with(.,"Tanggal Rilis")]/substring-after(.,"-")'))
      mangainfo.summary=x.xpathstring('//div[@class="sinopsis"]/*')
      mangainfo.chapterlinks.clear()
      mangainfo.chapternames.clear()
      x.xpathhrefall('//table[@class="chapter"]//tr//*[@class="judulseries"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    elseif module.website == 'MangaKita' then
      mangainfo.title=x.xpathstring('//h1')
      mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[contains(@class,"leftImage")]/img/@src'))
      mangainfo.authors=x.xpathstring('//span[@class="details"]//div[starts-with(.,"Author")]/substring-after(.,":")')
      mangainfo.genres=x.xpathstringall('//span[@class="details"]//div[starts-with(.,"Genre")]/a')
      mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//span[@class="details"]//div[starts-with(.,"Status")]'))
      mangainfo.summary=x.xpathstringall('//*[@class="description"]/text()', '')
      mangainfo.chapterlinks.clear()
      mangainfo.chapternames.clear()
      x.xpathhrefall('//div[contains(@class, "chapter-list")]//div[@class="list-group-item"]/span[contains(., "|")]/a', mangainfo.chapterlinks, mangainfo.chapternames)
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
      if module.website == 'KazeManga' then
        mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="img"]/img[@itemprop="image"]/@src'))
        x.xpathhrefall('//li//span[@class="leftoff"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
      end
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
    if module.website == 'PecintaKomik' then TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]/div//img/@src', task.pagelinks) end
    
    if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]//a/@href', task.pagelinks) end
    if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]//img/@src', task.pagelinks) end
    if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@id="readerareaimg"]//img/@src', task.pagelinks) end
    if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@id="imgholder"]//img/@src', task.pagelinks) end
    if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@class="entry-content"]//img/@src', task.pagelinks) end
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
    ['Komiku'] = '/daftar-komik/',
    ['OtakuIndo'] = '/daftar-komik/',
    ['WestManga'] = '/manga-list/?list',
    ['Kyuroku'] = '/manga-list/?list',
    ['BacaManga'] = '/manga/?list',
    ['PecintaKomik'] = '/daftar-manga/?list',
    ['MangaIndoNet'] = '/manga-list/?list',
    ['KomikIndo'] = '/manga-list/?list',
    ['KomikMama'] = '/manga-list/?list',
    ['KomikIndoWebId'] = '/daftar-manga/?list',
    ['KazeManga'] = '/komik-list/',
    ['Mangacan'] =  '/daftar-komik-manga-bahasa-indonesia.html',
    ['MangaIndo'] = '/manga-list-201902-v052/',
	['MangaCeng'] = '/manga/?list',
	['Maid'] = '/manga-list/?list',
  }
  local dirurl = '/manga-list/'
  if dirs[module.website] ~= nil then
    dirurl = dirs[module.website]
  end
  if http.get(module.rooturl..dirurl) then
    if module.website == 'KomikStation' then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="daftarkomik"]//a',links,names)
    elseif module.website == 'WestManga' or module.website == 'MangaKita' or module.website == 'Kyuroku' or module.website == 'KazeManga' then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="jdlbar"]//a',links,names)
    elseif module.website == 'KomikCast' or module.website == 'BacaManga' or module.website == 'Maid' then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="soralist"]//a',links,names)
    elseif module.website == 'Komiku' or module.website == 'OtakuIndo' then
      TXQuery.Create(http.document).xpathhrefall('//*[@id="a-z"]//h4/a',links,names)
    elseif module.website == 'Mangacan' or module.website == 'PecintaKomik' or module.website == 'KomikMama' or module.website == 'MangaCeng' then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="blix"]//a',links,names)
    elseif module.website == 'MangaIndo' then
      TXQuery.Create(http.document).xpathhrefall('//*[@class="manga-list"]/a',links,names)
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
  m.lastupdated = 'May 05, 2019'
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
  AddWebsiteModule('Kyuroku', 'https://kyuroku.com')
  AddWebsiteModule('BacaManga', 'https://bacamanga.co')
  AddWebsiteModule('PecintaKomik', 'https://www.pecintakomik.net')
  AddWebsiteModule('MangaIndoNet', 'https://mangaindo.net')
  AddWebsiteModule('KomikIndo', 'https://komikindo.co')
  AddWebsiteModule('KomikIndoWebId', 'https://www.komikindo.web.id')
  AddWebsiteModule('Komiku', 'https://komiku.co')
  AddWebsiteModule('OtakuIndo', 'https://otakuindo.co')
  AddWebsiteModule('KazeManga', 'https://kazemanga.xyz')
  AddWebsiteModule('Mangacan', 'http://www.mangacanblog.com')
  AddWebsiteModule('MangaIndo', 'https://mangaindo.web.id')
  AddWebsiteModule('KomikMama', 'https://komikmama.net')
  AddWebsiteModule('MangaCeng', 'https://mangaceng.com')
  AddWebsiteModule('Maid', 'https://www.maid.my.id')
end

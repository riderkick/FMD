function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    mangainfo.title     = getTitle(x)
    mangainfo.coverlink = MaybeFillHost(module.RootURL, getCover(x))
    mangainfo.authors   = getAuthors(x)
    mangainfo.genres    = getGenres(x)
    mangainfo.status    = MangaInfoStatusIfPos(getStatus(x))
    mangainfo.summary   = getSummary(x)
    getMangas(x)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getTitle(x)
	local title = ''
	if title == '' then title = x.xpathstring('//div[@class="infox"]/h1') end
	if title == '' then title = x.xpathstring('//h1[@itemprop="headline"]') end
	if title == '' then title = x.xpathstring('//h1[@itemprop="name"]') end
	if title == '' then title = x.xpathstring('//div[@class="info1"]/*') end
	if title == '' then title = x.xpathstring('//div[@class="mangainfo"]/h1') end
	if title == '' then title = x.xpathstring('//h2[@class="entry-title"]') end
	if title == '' then title = x.xpathstring('//h1') end
	if title == '' then title = x.xpathstring('//h2') end
	title = title:gsub('Bahasa Indonesia$', ''):gsub(' Indonesia|Baca"', ''):gsub('Bahasa Indonesia', ''):gsub('Komik', '')
	title = title:gsub('Manga', ''):gsub('Indonesia', ''):gsub('Baca', ''):gsub('bahasa', ''):gsub('indonesia', ''):gsub('can', ''):gsub('|', '')
	title = title:gsub(string.gsub(module.website, 'https://', ''), '')
	return title
end

function getCover(x)
	local img = ''
	if img == '' then img = x.xpathstring('//div[@class="thumb"]/img/@src') end
	if img == '' then img = x.xpathstring('//div[@class="imgdesc"]/img/@src') end
	if img == '' then img = x.xpathstring('//div[contains(@class,"leftImage")]/img/@src') end
	if img == '' then img = x.xpathstring('//div[@class="imgseries"]/img/@src') end
	if img == '' then img = x.xpathstring('//div[@itemprop="image"]/img/@src') end
	if img == '' then img = x.xpathstring('//div[@class="mangainfo"]//div[@class="topinfo"]/img/@src') end
	if img == '' then img = x.xpathstring('//div[@id="m-cover"]/img/@src') end
	if img == '' then img = x.xpathstring('//div[@itemprop="image"]/img/@data-lazy-src') end
	if img == '' then img = x.xpathstring('//div[@class="img"]/img[@itemprop="image"]/@src') end
	return img
end

function getAuthors(x)
	local authors = ''
	if authors == '' then authors = x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Author")]/substring-after(.,":")') end
	if authors == '' then authors = x.xpathstring('//span[@class="details"]//div[starts-with(.,"Author")]/substring-after(.,":")') end
	if authors == '' then authors = x.xpathstring('//div[@class="preview"]//li[starts-with(.,"Komikus")]/substring-after(.,":")') end
	if authors == '' then authors = x.xpathstring('//div[@class="spe"]//span[starts-with(.,"Author")]/substring-after(.,":")') end
	if authors == '' then authors = x.xpathstring('//table[@class="attr"]//tr[contains(th, "Author")]/td') end
	if authors == '' then authors = x.xpathstring('//table[@class="listinfo"]//tr[contains(th, "Penulis")]/td') end
	if authors == '' then authors = x.xpathstring('//*[@class="anf"]//li[starts-with(.,"Author")]/substring-after(.,":")') end
	if authors == '' then authors = x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Pengarang")]/substring-after(.," ")') end
	if authors == '' then authors = x.xpathstring('//span[@id="m-author"]') end
	if authors == '' then authors = x.xpathstring('//ul[@class="baru"]/li[2][starts-with(.,"Mangaka")]/substring-after(.,":")') end
	return authors
end

function getGenres(x)
	local genre = ''
	if genre == '' then genre = x.xpathstringall('//div[@class="spe"]//span[starts-with(.,"Genres:")]/substring-after(.,":")') end
	if genre == '' then genre = x.xpathstringall('//div[contains(@class,"animeinfo")]/div[@class="gnr"]/a') end
	if genre == '' then genre = x.xpathstringall('//div[@class="gnr"]/a') end
	if genre == '' then genre = x.xpathstringall('//div[contains(@class,"mrgn animeinfo")]/div[@class="gnr"]/a') end
	if genre == '' then genre = x.xpathstringall('//span[@id="m-genre"]') end
	if genre == '' then genre = x.xpathstringall('//table[@class="listinfo"]//tr[contains(th, "Genre")]/td/a') end
	if genre == '' then genre = x.xpathstringall('//table[@class="attr"]//tr[contains(th, "Genres")]/td/a') end
	if genre == '' then genre = x.xpathstringall('//div[@class="spe"]//span[starts-with(.,"Genre")]/a') end
	if genre == '' then genre = x.xpathstringall('//div[@class="spe"]//span[starts-with(.,"Genres")]/a') end
	if genre == '' then genre = x.xpathstringall('//div[@class="genrex"]/a') end
	if genre == '' then genre = x.xpathstringall('//ul[@class="genre"]/li') end
	if genre == '' then genre = x.xpathstringall('//span[@class="details"]//div[starts-with(.,"Genre")]/a') end
	if genre == '' then genre = x.xpathstringall('//div[@class="listinfo"]//li[starts-with(.,"Genre")]/substring-after(.,":")') end
	return genre
end

function getStatus(x)
	local status = ''
	if status == '' then status = x.xpathstring('//div[@class="spe"]//span[starts-with(.,"Status:")]/substring-after(.,":")') end
	if status == '' then status = x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Status")]/substring-after(.," ")') end
	if status == '' then status = x.xpathstring('//*[@class="anf"]//li[starts-with(.,"Status")]/substring-after(.,":")') end
	if status == '' then status = x.xpathstring('//span[@id="m-status"]') end
	if status == '' then status = x.xpathstring('//table[@class="listinfo"]//tr[contains(th, "Status")]/td') end
	if status == '' then status = x.xpathstring('//table[@class="attr"]//tr[contains(th, "Status")]/td') end
	if status == '' then status = x.xpathstring('//div[@class="preview"]//li[starts-with(.,"Tanggal Rilis")]/substring-after(.,"-")') end
	if status == '' then status = x.xpathstring('//span[@class="details"]//div[starts-with(.,"Status")]') end
	if status == '' then status = x.xpathstring('//ul[@class="baru"]/li[3]') end
	status = status:gsub('Finished', 'Completed')
	return status
end

function getSummary(x)
	local summary = ''
	if summary == '' then summary = x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.xpathstring('//*[@class="sinopsis"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.xpathstring('//*[@id="m-synopsis"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.xpathstring('//*[@class="sin"]/p') end
	if summary == '' then summary = x.xpathstring('//*[@class="description"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.xpathstring('//div[contains(@class,"animeinfo")]/div[@class="rm"]/span/string-join(.//text(),"")') end
	summary = summary:gsub('.fb_iframe_widget_fluid_desktop iframe', ''):gsub('width: 100%% !important;', ''):gsub('{', ''):gsub('}', '')
	return summary
end

function getMangas(x)
	if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//li//span[@class="leftoff"]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
	if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//div[@class="bxcl"]//li//*[@class="lchx"]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
	if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//div[@class="bxcl"]//li//div[@class="lch"]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
	if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//div[@class="bxcl nobn"]//li//div[@class="lch"]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
	if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//ul[@class="lcp_catlist"]//li/a', mangainfo.chapterlinks, mangainfo.chapternames) end
  	if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//div[contains(@class, "bxcl")]//li//*[contains(@class,"lchx")]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
  	if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//div[contains(@class, "lchx")]//li//*[contains(@class,"bxcl")]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
  	if mangainfo.chapterlinks.count < 1 or module.website == 'Mangacan' then
    local v = x.xpath('//table[@class="updates"]//td/a')
      for i = 1, v.count do
        local v1 = v.get(i)
        local s = v1.getAttribute('href')
        s = string.gsub(s, '-1.htm', '.htm')
        mangainfo.chapternames.Add(Trim(SeparateLeft(v1.toString, '')));
        mangainfo.chapterlinks.Add(s);
      end
  	end
  	if mangainfo.chapterlinks.count < 1 or module.website == 'Komiku' or module.website == 'OtakuIndo' then
    local v = x.xpath('//table[@class="chapter"]//td/a')
      for i = 1, v.count do
        local v1 = v.get(i)
        mangainfo.chapternames.Add(v1.toString);
        mangainfo.chapterlinks.Add(v1.getAttribute('href'));
      end
  	end
  	if mangainfo.chapterlinks.count < 1 or module.website == 'MangaKita' then
    local v = x.xpath('//div[@class="list chapter-list"]//div/span/a')
      for i = 1, v.count do
        local v1 = v.get(i)
        local s = v1.toString
        local l = v1.getAttribute('href')
        local title = l
        	if s < 'Download PDF' then
        		title = title:gsub('mangakita.net', ''):gsub('https:', '')
        		title = title:gsub('/', ''):gsub('-', ' ')
        		mangainfo.chapternames.Add(title);
        		mangainfo.chapterlinks.Add(l);
        	end
      end
  	end
end

function getpagenumber()
  task.pagenumber=0
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]/div//img/@src', task.pagelinks) end    
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
	['KazeManga'] = '/manga-list/?list',
    ['KomikIndoWebId'] = '/daftar-manga/?list',
    ['Mangacan'] =  '/daftar-komik-manga-bahasa-indonesia.html',
    ['MangaIndo'] = '/manga-list-201902-v052/',
	['MangaCeng'] = '/manga/?list',
	['MaidMangaID'] = '/manga-list/?list',
	['KOMIKAV'] = '/manga/?list',
  }
  local dirurl = '/manga-list/'
  if dirs[module.website] ~= nil then
    dirurl = dirs[module.website]
  end
  if http.get(module.rooturl..dirurl) then
    local x=TXQuery.Create(http.document) 
    if links.count < 1 then x.xpathhrefall('//*[@class="daftarkomik"]//a',links,names) end
   	if links.count < 1 then x.xpathhrefall('//*[@class="jdlbar"]//a',links,names) end
   	if links.count < 1 then x.xpathhrefall('//*[@class="soralist"]//a',links,names) end
    if links.count < 1 then x.xpathhrefall('//*[@id="a-z"]//h4/a',links,names) end
    if links.count < 1 then x.xpathhrefall('//*[@class="blix"]//a',links,names) end
    if links.count < 1 then x.xpathhrefall('//*[@class="manga-list"]/a',links,names) end
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
  m.lastupdated='July 19, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  return m
end

function Init()
local cat = 'Indonesian'
	AddWebsiteModule('MangaShiro', 'https://mangashiro.net', cat)
	AddWebsiteModule('MangaKita', 'https://mangakita.net', cat)
	AddWebsiteModule('KomikStation', 'https://www.komikstation.com', cat)
	AddWebsiteModule('MangaKid', 'https://mgku.me', cat)
	AddWebsiteModule('KomikCast', 'https://komikcast.com', cat)
	AddWebsiteModule('WestManga', 'https://westmanga.info', cat)
	AddWebsiteModule('Kiryuu', 'https://kiryuu.co', cat)
	AddWebsiteModule('Kyuroku', 'https://kyuroku.com', cat)
	AddWebsiteModule('BacaManga', 'https://bacamanga.co', cat)
	AddWebsiteModule('PecintaKomik', 'https://www.pecintakomik.net', cat)
	AddWebsiteModule('MangaIndoNet', 'https://mangaindo.net', cat)
	AddWebsiteModule('KomikIndo', 'https://komikindo.co', cat)
	AddWebsiteModule('KomikIndoWebId', 'https://www.komikindo.web.id', cat)
	AddWebsiteModule('Komiku', 'https://komiku.co', cat)
	AddWebsiteModule('OtakuIndo', 'https://otakuindo.co', cat)
	AddWebsiteModule('KazeManga', 'https://kazemanga.web.id', cat)
	AddWebsiteModule('Mangacan', 'http://www.mangacanblog.com', cat)
	AddWebsiteModule('MangaIndo', 'https://mangaindo.web.id', cat)
	AddWebsiteModule('KomikMama', 'https://komikmama.net', cat)
	AddWebsiteModule('MangaCeng', 'https://mangaceng.com', cat)
	AddWebsiteModule('MaidMangaID', 'https://www.maid.my.id', cat)
	AddWebsiteModule('KOMIKAV', 'https://komikav.com/', cat)
end
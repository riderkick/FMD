function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    mangainfo.title     = getTitle(x)
    mangainfo.coverlink = MaybeFillHost(module.RootURL, getCover(x))
    mangainfo.authors   = getAuthors(x)
    mangainfo.artists   = getArtists(x)
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
  if title == '' then title = x.xpathstring('//*[@id="judul"]/h1') end
  if title == '' then title = x.xpathstring('//*[@id="judul_komik"]/h1') end
  if title == '' then title = x.xpathstring('//div[@class="infox"]/h1') end
  if title == '' then title = x.xpathstring('//h1[@itemprop="headline"]') end
  if title == '' then title = x.xpathstring('//h1[@itemprop="name"]') end
  if title == '' then title = x.xpathstring('//div[@class="info1"]/*') end
  if title == '' then title = x.xpathstring('//div[@class="mangainfo"]/h1') end
  if title == '' then title = x.xpathstring('//h2[@class="entry-title"]') end
  if title == '' then title = x.xpathstring('//h1') end
  if title == '' then title = x.xpathstring('//h2') end
  title = title:gsub('Bahasa Indonesia$', ''):gsub(' Indonesia|Baca"', ''):gsub('Bahasa Indonesia', ''):gsub('Komik', ''):gsub(' Raw', '')
  title = title:gsub('Manga', ''):gsub('Indonesia', ''):gsub('Baca', ''):gsub('bahasa', ''):gsub('indonesia', ''):gsub('can', ''):gsub('|', '')
  title = title:gsub(string.gsub(module.website, 'https://', ''), '')
  return title
end

function getCover(x)
  local img = ''
  if img == '' then img = x.xpathstring('//div[@class="thumb"]/img/@data-src') end
  if img == '' then img = x.xpathstring('//div[@class="thumb"]/img/@src') end
  if img == '' then img = x.xpathstring('//div[@class="imgdesc"]/img/@src') end
  if img == '' then img = x.xpathstring('//div[contains(@class,"leftImage")]/img/@src') end
  if img == '' then img = x.xpathstring('//div[@class="imgseries"]/img/@src') end
  if img == '' then img = x.xpathstring('//div[@itemprop="image"]/img/@src') end
  if img == '' then img = x.xpathstring('//div[@class="mangainfo"]//div[@class="topinfo"]/img/@src') end
  if img == '' then img = x.xpathstring('//div[@id="m-cover"]/img/@src') end
  if img == '' then img = x.xpathstring('//div[@itemprop="image"]/img/@data-lazy-src') end
  if img == '' then img = x.xpathstring('//div[@class="img"]/img[@itemprop="image"]/@src') end
  if img == '' then img = x.xpathstring('//div[@class="ims"]/img/@src') end
  return img
end

function getAuthors(x)
  local authors = ''
  if authors == '' then authors = x.xpathstring('//div[@class="spe"]//span[starts-with(.,"المؤلف")]/substring-after(.,":")') end
  if authors == '' then authors = x.xpathstring('//li[starts-with(.,"Komikus")]/b') end
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
  if authors == '' then authors = x.xpathstring('//table[@class="listinfo"]//tr[contains(th, "Author")]/following-sibling::td') end
  if authors == '' then authors = x.xpathstring('//tr[contains(td, "Komikus")]//following-sibling::td') end
  return authors
end

function getArtists(x)
  local artists = ''
  if artists == '' then artists = x.xpathstringall('//div[@class="spe"]//span[starts-with(.,"Artist")]/substring-after(.,":")') end
  return artists
end

function getGenres(x)
  local genre = ''
  if genre == '' then genre = x.xpathstringall('//div[@class="spe"]//span[contains(.,"التصنيفات")]/a') end
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
  if status == '' then status = x.xpathstring('//div[@class="spe"]//span[starts-with(.,"الحالة")]/substring-after(.,":")') end
  if status == '' then status = x.xpathstring('//div[@class="spe"]//span[starts-with(.,"Status:")]/substring-after(.,":")') end
  if status == '' then status = x.xpathstring('//div[@class="listinfo"]//li[starts-with(.,"Status")]/substring-after(.," ")') end
  if status == '' then status = x.xpathstring('//*[@class="anf"]//li[starts-with(.,"Status")]/substring-after(.,":")') end
  if status == '' then status = x.xpathstring('//span[@id="m-status"]') end
  if status == '' then status = x.xpathstring('//table[@class="listinfo"]//tr[contains(th, "Status")]/td') end
  if status == '' then status = x.xpathstring('//table[@class="attr"]//tr[contains(th, "Status")]/td') end
  if status == '' then status = x.xpathstring('//div[@class="preview"]//li[starts-with(.,"Tanggal Rilis")]/substring-after(.,"-")') end
  if status == '' then status = x.xpathstring('//span[@class="details"]//div[starts-with(.,"Status")]') end
  if status == '' then status = x.xpathstring('//ul[@class="baru"]/li[3]') end
  if status == '' then status = x.xpathstring('//tr[contains(td, "Status")]//following-sibling::td') end
  status = status:gsub('Finished', 'Completed')
  return status
end

function getSummary(x)
  local summary = ''
  if summary == '' then summary = x.xpathstring('//div[@class="sinopsis"]/p') end
  if summary == '' then summary = x.xpathstring('//*[@class="desc"]/string-join(.//text(),"")') end
  if summary == '' then summary = x.xpathstring('//*[@class="sinopsis"]/string-join(.//text(),"")') end
  if summary == '' then summary = x.xpathstring('//*[@id="m-synopsis"]/string-join(.//text(),"")') end
  if summary == '' then summary = x.xpathstring('//*[@class="sin"]/p') end
  if summary == '' then summary = x.xpathstring('//*[@class="description"]/string-join(.//text(),"")') end
  if summary == '' then summary = x.xpathstring('//div[contains(@class,"animeinfo")]/div[@class="rm"]/span/string-join(.//text(),"")') end
  if summary == '' then summary = x.xpathstring('//*[@class="jds"]/p') end
  summary = summary:gsub('.fb_iframe_widget_fluid_desktop iframe', ''):gsub('width: 100%% !important;', ''):gsub('{', ''):gsub('}', '')
  return summary
end

function getMangas(x)
  if module.website == 'Ngomik' then
    local v = x.xpath('//div[contains(@class, "bxcl")]//li//*[contains(@class,"lch")]/a')
    for i = 1, v.count do
      local v1 = v.get(i)
      local name = v1.getAttribute('href')
      mangainfo.chapternames.Add(name:gsub(module.rooturl..'/',''))
      mangainfo.chapterlinks.Add(v1.getAttribute('href'));
    end
  else
    if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//li//span[@class="leftoff"]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
    if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//div[@class="bxcl"]//li//*[@class="lchx"]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
    if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//div[@class="bxcl"]//li//div[@class="lch"]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
    if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//div[@class="bxcl nobn"]//li//div[@class="lch"]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
    if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//ul[@class="lcp_catlist"]//li/a', mangainfo.chapterlinks, mangainfo.chapternames) end
    if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//div[contains(@class, "bxcl")]//li//*[contains(@class,"lchx")]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
    if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//div[contains(@class, "lchx")]//li//*[contains(@class,"bxcl")]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
    if mangainfo.chapterlinks.count < 1 then x.xpathhrefall('//span[@class="lchx"]/a', mangainfo.chapterlinks, mangainfo.chapternames) end
    
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
    
    if mangainfo.chapterlinks.count < 1 or module.website == 'Komiku' then
      local v = x.xpath('//table[@class="chapter"]//td[1]/a')
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
end

function getpagenumber()
  task.pagenumber=0
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    if module.website == 'BacaManga' then
      local x = TXQuery.Create(http.Document)
      local s = x.xpathstring('*')
            x.parsehtml(DecodeBase64(GetBetween('](atob(', ')),', s)))
            x.xpathstringall('json(*)()', task.pagelinks)
    elseif module.website == 'Kiryuu' then
      local x = TXQuery.Create(http.Document)
      local v=x.xpath('//*[@id="readerarea"]//img')
        for i=1,v.count do
            local v1=v.get(i)
            if string.find(v1.getAttribute('src'), ".filerun.") == nil and
               string.find(v1.getAttribute('src'), ",0.jpg") == nil and
               string.find(v1.getAttribute('src'), ",5.jpg") == nil and
               string.find(v1.getAttribute('src'), ".5.jpg") == nil and
               string.find(v1.getAttribute('src'), "00.jpg") == nil and
               string.find(v1.getAttribute('src'), "z10.jpg") == nil and
               string.find(v1.getAttribute('src'), "Komeng.jpg") == nil and
               string.find(v1.getAttribute('src'), "ZZ.jpg") == nil then
                task.pagelinks.add(v1.getAttribute('src'))
            end
        end
    elseif module.website == 'MangaSWAT' then TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]/p/img/@data-src', task.pagelinks)
    else
      if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]/p/img/@src', task.pagelinks) end
      if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]/p//img/@src', task.pagelinks) end
      if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]/div//img/@src', task.pagelinks) end    
      if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]//a/@href', task.pagelinks) end
      if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@id="readerarea"]//img/@src', task.pagelinks) end
      if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@id="readerareaimg"]//img/@src', task.pagelinks) end
      if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@id="imgholder"]//img/@src', task.pagelinks) end
      if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@class="entry-content"]//img/@src', task.pagelinks) end
      if task.pagelinks.count < 1 then TXQuery.Create(http.Document).xpathstringall('//*[@class="bc"]/img/@src', task.pagelinks) end
      if task.pagelinks.count < 1 or module.website == 'KoMBatch' then 
        local link = MaybeFillHost(module.rooturl,url)
        link = link:gsub('/read', '/api/chapter')
        if http.get(link) then
          x=TXQuery.Create(http.document)
          x.parsehtml(http.document)
		  for _, v in ipairs(x.xpathi('json(*).chapter.images()("text")')) do
            task.pagelinks.add(v.tostring:gsub('^//', 'https://'))
          end
        else
          return false
        end
      end
    end
    return true
  else
    return false
  end
end

function getnameandlink()
  local dirs = {
    ['MangaShiro'] = '/manga/?list',
    ['KomikStation'] = '/manga/?list',
    ['MangaKid'] = '/manga-lists/',
    ['KomikCast'] = '/daftar-komik/?list',
    ['WestManga'] = '/manga-list/?list',
    ['Kiryuu'] = '/manga-lists/?list',
    ['Kyuroku'] = '/manga/?list',
    ['BacaManga'] = '/manga/?list',
    ['PecintaKomik'] = '/daftar-manga/?list',
    ['MangaIndoNet'] = '/manga-list/?list',
    ['KomikIndo'] = '/manga-list/?list',
    ['KomikIndoWebId'] = '/daftar-komik/',
    ['Komiku'] = '/daftar-komik/',
    ['KazeManga'] = '/manga-list/?list',
    ['Mangacan'] =  '/daftar-komik-manga-bahasa-indonesia.html',
    ['MangaIndo'] = '/manga-list-201902-v052/',
    ['KomikMama'] = '/manga-list/?list',
    ['MangaCeng'] = '/manga/?list',
    ['MaidMangaID'] = '/manga-list/?list',
    ['KomikAV'] = '/manga/?list',
    ['Ngomik'] = '/daftar-komik/?list',
    ['MangaPus'] = '/manga-list/?list',
    ['Mangaseno'] = '/manga-list/?list',
    ['SekteKomik'] = '/manga/?list',
    ['BaekjinScans'] = '/manga/?list',
    ['Mangakyo'] = '/daftar-manga/?list',
    ['MataKomik'] = '/manga/?list',
    ['Rawkuma'] = '/manga/?list',
    ['KomikGoCoID'] = '/manga/?list',
    ['MangaSWAT'] = '/manga/?list',
    ['MangaTsuki'] = '/manga/?list'
  }
  local dirurl = '/manga-list/'
  if dirs[module.website] ~= nil then
    dirurl = dirs[module.website]
  end
  if http.get(module.rooturl..dirurl) then
    local x=TXQuery.Create(http.document) 
    if links.count < 1 then x.xpathhrefall('//*[@class="daftarkomik"]//a',links,names) end
    if links.count < 1 then x.xpathhrefall('//*[@class="jdlbar"]//a',links,names) end  
    if links.count < 1 then x.xpathhrefall('//*[@class="blix"]//a',links,names) end
    if links.count < 1 then x.xpathhrefall('//*[@class="soralist"]//a',links,names) end
    if links.count < 1 then x.xpathhrefall('//*[@id="a-z"]//h4/a',links,names) end
    if links.count < 1 then x.xpathhrefall('//*[@class="manga-list"]/a',links,names) end
    
    if links.count < 1 or module.website == 'KoMBatch' then
      local pages = 1
      local p = 1
      while p <= pages do
        if p > 1 then
          if http.get(module.rooturl..dirurl..'?page=' .. tostring(p)) then
            x=TXQuery.Create(http.document)
          else
            break
          end
        end
        if p == pages then
          local pg = x.XPathString('//*[contains(@class, "pagination")]//li[last()-1]/a/substring-after(@href, "?page=")')
          if pg ~= '' then pages = tonumber(pg) end
        end
        local v=x.xpath('//*[contains(@class, "trending")]//*[contains(@class, "box_trending")]')
        for i=1,v.count do
          local v1=v.get(i)
          local title = x.xpathstring('.//*[contains(@class, "_2dU-m")]/text()', v1)
          local link = x.xpathstring('.//*[contains(@class, "_2dU-m")]/@href', v1)
          names.add(title)
          links.add(link)
        end
        p = p + 1
      end
    end
    if links.count < 1 or module.website == 'Mangacan' then x.xpathhrefall('//*[@class="blix"]/ul//a',links,names) end
    return no_error
  else
    return net_problem
  end
end

function BeforeDownloadImage()
  http.Headers.Values['referer'] = module.RootURL
  http.Headers.Values['Accept'] = 'image/webp,image/apng,image/*,*/*'   
  return true
end

function AddWebsiteModule(site, url, cat)
  local m=NewModule()
  m.category=cat
  m.website=site
  m.rooturl=url
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  if site == 'MangaShiro' then m.OnBeforeDownloadImage = 'BeforeDownloadImage' end
  return m
end

function Init()
local cat = 'Indonesian'
  AddWebsiteModule('MangaShiro', 'https://mangashiro.co', cat)
  AddWebsiteModule('MangaKita', 'https://mangakita.net', cat)
  AddWebsiteModule('KomikStation', 'https://www.komikstation.com', cat)
  AddWebsiteModule('MangaKid', 'https://mangakid.club', cat)
  AddWebsiteModule('KomikCast', 'https://komikcast.com', cat)
  AddWebsiteModule('WestManga', 'https://westmanga.info', cat)
  AddWebsiteModule('Kiryuu', 'https://kiryuu.co', cat)
  AddWebsiteModule('Kyuroku', 'https://kyuroku.com', cat)
  AddWebsiteModule('BacaManga', 'https://bacamanga.co', cat)
  AddWebsiteModule('PecintaKomik', 'https://www.pecintakomik.net', cat)
  AddWebsiteModule('MangaIndoNet', 'https://mangaindo.net', cat)
  AddWebsiteModule('KomikIndo', 'https://komikindo.co', cat)
  AddWebsiteModule('KomikIndoWebId', 'https://www.komikindo.web.id', cat)
  AddWebsiteModule('Komiku', 'https://komiku.co.id', cat)
  AddWebsiteModule('KazeManga', 'https://kazemanga.web.id', cat)
  AddWebsiteModule('Mangacan', 'http://www.mangacanblog.com', cat)
  AddWebsiteModule('MangaIndo', 'https://mangaindo.web.id', cat)
  AddWebsiteModule('KomikMama', 'https://komikmama.net', cat)
  AddWebsiteModule('MangaXin', 'https://mangaxin.com', cat)
  AddWebsiteModule('MaidMangaID', 'https://www.maid.my.id', cat)
  AddWebsiteModule('KomikAV', 'https://komikav.com', cat)
  AddWebsiteModule('KoMBatch', 'https://kombatch.com', cat)
  AddWebsiteModule('Ngomik', 'https://ngomik.in', cat)
  AddWebsiteModule('MangaPus', 'https://mangapus.com', cat)
  AddWebsiteModule('Mangaseno', 'https://mangaseno.com', cat)
  AddWebsiteModule('Mangakyo', 'https://www.mangakyo.me', cat)
  AddWebsiteModule('MataKomik', 'https://matakomik.com', cat)
  AddWebsiteModule('KomikGoCoID', 'https://www.komikgo.co.id', cat)
  AddWebsiteModule('MangaTsuki', 'https://mangatsuki.web.id', cat)
  
  cat = 'Webcomics'
  AddWebsiteModule('SekteKomik', 'http://sektekomik.com', cat)
  AddWebsiteModule('BaekjinScans', 'https://baekjinscans.xyz', cat)
  
  cat = 'Raw'
  AddWebsiteModule('Rawkuma', 'https://rawkuma.com', cat)
  
  cat = 'Arabic'
  AddWebsiteModule('MangaSWAT', 'https://mangaswat.com', cat)
end

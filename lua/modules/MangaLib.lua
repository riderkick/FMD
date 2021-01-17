function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//meta[@itemprop="alternativeHeadline"]/@content')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//img[@class="manga__cover"]/@src'))
    mangainfo.authors=x.xpathstring('//div[@class="info-list__row"][starts-with(.,"Автор")]')
    mangainfo.authors=string.gsub(mangainfo.authors, 'Автор', '')
    mangainfo.authors=string.gsub(mangainfo.authors, '  ', '')
    mangainfo.artists=x.xpathstring('//div[@class="info-list__row"][starts-with(.,"Художник")]')
    mangainfo.artists=string.gsub(mangainfo.artists, 'Художник', '')
    mangainfo.artists=string.gsub(mangainfo.artists, '  ', '')
    mangainfo.genres=x.xpathstring('//div[@class="info-list__row"][starts-with(.,"Жанры")]')
    mangainfo.genres=string.gsub(mangainfo.genres, 'Жанры', '')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="info-list__row"][starts-with(.,"Перевод")]'), 'продолжается', 'завершен')
    mangainfo.summary=x.xpathstringall('//div[contains(@class, "info-desc__content")]/text()', '')
    x.xpathhrefall('//div[@class="chapters-list"]/div/div[@class="chapter-item__name"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
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
    local x=TXQuery.Create(http.Document)
    local imgbaseurl=buildimageurl(TXQuery.Create(string.match(x.xpathstring('//script[contains(., "window.__info")]/text()'), 'window.__info =(.*);')))
    local p=TXQuery.Create(DecodeBase64(StreamToString(http.document):match('<span class="pp"><!%-%-(.-)%-%-></span>'):gsub('^%s*(.-)%s*$', '%1')))
    local v=p.xpath('json(*)()')
    for i=1,v.count do
      local v1=v.get(i)
      task.pagelinks.add(imgbaseurl..x.xpathstring('u', v1))
    end
    return true
  else
    return false
  end
end

function buildimageurl(json)
  local result = string.gsub(module.RootURL, 'https://', '')
  if json.xpathstring('json(*).imgServer') == 'primary' then
    result = 'img1.'..result
  else
    result = 'img2.'..result
  end
  result = 'https://' .. result .. json.xpathstring('json(*).imgUrl')
  
  return result
end

function getnameandlink()
  if tonumber(url) <= 0 then url = 200 end
  if http.get(module.rooturl .. '/filterlist?page='..IncStr(url)..'&cat=&alpha=&sortBy=name&asc=true&author=&artist=') then
    local x = TXQuery.Create(http.Document)
    local page = x.xpathstring('//div[@class="paginator paginator_full paginator_border-top"]//ul[@class="pagination"]/li[last()]/a/substring-after(@href, "?page=")')
    if x.xpathstring('//div/p[contains(., "Ничего не найдено")]') == '' then
      local v = x.xpath('//*[@class="manga-list-item"]/a[@class="manga-list-item__content"]')
      for i=1,v.count do
        local v1=v.get(i)
        links.add(v1.getattribute('href'))
        names.add(v1.getattribute('title'))
      end
      updatelist.CurrentDirectoryPageNumber = page
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'MangaLib'
  m.rooturl = 'https://mangalib.me'
  m.category = 'Russian'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

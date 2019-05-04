function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//*[@class="manga-bg__subtitle"]')
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
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    task.pagenumber = x.xpathcount('//select[@id="reader-pages"]/option/@value')
  else
    return false
  end
  return true
end

function getimageurl()
  local s = AppendURLDelim(url)..(workid+1)
  if http.get(MaybeFillHost(module.rooturl,s)) then
    local x=TXQuery.Create(http.document)
	local link = x.xpathstring('//meta[@property="og:image"]/@content')
		  link = string.sub(link, 1, string.len(link) - 6)
	local page = string.sub('00'..(workid+1), 2, 4)
		  if  string.len(page) == 3 then
			  page = (workid+1)
		  end
	task.pagelinks[workid]=link..page..'.png'
  	return true
  end
  return false
end

function getnameandlink()
  if tonumber(url) < 0 then return no_error end
  if http.get(module.rooturl .. '/filterlist?page='..IncStr(url)..'&cat=&alpha=&sortBy=name&asc=true&author=&artist=') then
    local x = TXQuery.Create(http.Document)
    if x.xpathstring('//div/p[contains(., "Ничего не найдено")]') == '' then
      local v = x.xpath('//ul[contains(@class, "manga-list")]/li/div/div[@class="heading"]/a[@class="ttl"]')
      for i=1,v.count do
        local v1=v.get(i)
        links.add(v1.getattribute('href'))
        names.add(x.xpathstringall('h2/text()', '', v1))
      end
      updatelist.CurrentDirectoryPageNumber = updatelist.CurrentDirectoryPageNumber + 1
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
  m.lastupdated='May 4, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetimageurl='getimageurl'
end

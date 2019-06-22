function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//*[@class="manga__title"]/h2')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//img[@class="manga__cover"]/@src'))
    mangainfo.authors=x.xpathstringall('//div[@class="manga-info"]//p[contains(*, "Автор")]/a')
    mangainfo.artists=x.xpathstringall('//div[@class="manga-info"]//p[contains(*, "Художник")]/a')
    mangainfo.genres=x.xpathstringall('//div[@class="manga-info"]//p[contains(*, "Жанры")]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="manga-info"]//p[contains(*, "Перевод")]/span'), 'продолжается', 'завершен')
    mangainfo.summary=x.xpathstringall('//div[contains(@class, "manga__desc")]/blockquote/text()', '')
    x.xpathhrefall('//div[@class="chapters-list"]/div/div[@class="chapter-item__name"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber=0
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(., "var pages")]')
    local base = s:match('%.scan%-page.+src\'%s*,%s*\'([^\']+)\'')
    s = GetBetween('var pages =', ';', s)
    x.parsehtml(s)
    local v = x.xpath('json(*)().page_image')
    for i = 1,v.count do
      local v1=v.get(i)
      task.pagelinks.add(base .. '/' .. v1.tostring)
    end
  else
    return false
  end
  return true
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
  m.lastupdated='March 3, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end
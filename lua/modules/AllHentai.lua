local dirurl = '/list?type=&sortType=DATE_CREATE'
local perpage = 60

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    local rname = x.xpathstring('//meta[@itemprop="name"]/@content')
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//meta[@itemprop="alternativeHeadline"]/@content')
    end
    if mangainfo.title == '' then
      mangainfo.title = rname
    end
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//img[@itemprop="image"]/@src'))
    mangainfo.authors=x.xpathstringall('//p[@class="elementList" and contains(b, "Автор")]/a')
    mangainfo.genres=x.xpathstringall('//p[@class="elementList" and (contains(b, "Жанры") or contains(b, "Категории"))]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//p[contains(b, "Перевод")]'), 'продолжается', 'завершен')
    mangainfo.summary=x.xpathstring('//div[@class="mangaDescription"]/div[@itemprop="description"]')
    -- TODO: remove manga name from chapter name
    x.xpathhrefall('//div[@class="expandable"]/table[@class="cTable"]/tbody/tr/td/a',mangainfo.chapterlinks,mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function beforedownloadimage()
  http.headers.values['Referer'] = module.rooturl
  return true
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(., "var pictures")]')
    s = GetBetween('pictures =', ';', s)
    x.parsehtml(s)
    x.xpathstringall('json(*)().url', task.pagelinks)
  else
    return false
  end
  return true
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. dirurl) then
    local x = TXQuery.Create(http.Document)
    local s = x.xpathstring('//*[@class="pagination"]/a[@class="step"][last()]/@href')
    page = tonumber(s:match('offset=(%d+)'))
    if page == nil then page = 1 end
    if page > 1 then page = math.ceil(page / perpage) + 1; end
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  local s = module.rooturl .. dirurl
  if url ~= '0' then s = s .. '&offset=' .. (tonumber(url) * perpage) .. '&max=' .. perpage; end
  if http.get(s) then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//table[@class="cTable"]//tr/td/a[not(@class)]')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(v1.getattribute('href'))
      names.add(x.xpathstring('./text()', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'AllHentai'
  m.rooturl = 'http://allhentai.ru'
  m.category = 'Russian'
  m.lastupdated='April 26, 2018'
  m.sortedlist = true
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
  m.onbeforedownloadimage = 'beforedownloadimage'
end

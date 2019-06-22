function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//h1')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="item"]/div[@class="image"]/img/@src'))
    mangainfo.genres=x.xpathstringall('//div[@class="extra"]/a')
    mangainfo.summary=x.xpathstring('//div[@id="description"]')
    local s = x.xpathstring('//script[contains(., "mangaChapterCollection")]')
    local name = GetBetween('View.Manga(', ');', s)
    name = name:match('mangaName:%s*[\'"]([^\'"]+)[\'"]')
    s = GetBetween('MangaChapter(', ');', s)
    x.parsehtml(s)
    local v = x.xpath('json(*)()')
    for i = 1, v.count do
      local v1 = v.get(i)
      s = string.format('/%s/%s/%s/1/', name, x.xpathstring('volume', v1), x.xpathstring('number', v1))
      mangainfo.chapterlinks.add(s)
      mangainfo.chapternames.add(x.xpathstring('title', v1))
    end
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
    local s = x.xpathstring('//script[contains(., "chapterRouter")]')
    s = GetBetween('Chapter(', ');', s)
    x.parsehtml(s)
    local base = x.xpathstring('json(*).srcBaseUrl')
    local v = x.xpath('json(*).pages/*/src')
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
  if http.get(module.rooturl .. '/genre/all/page/'..IncStr(url)) then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//div[@class="genres"]/a[@class="genre"]')
    for i=1,v.count do
      local v1=v.get(i)
      links.add(v1.getattribute('href'))
      names.add(x.xpathstring('div/h2', v1))
    end
    local page = tonumber(x.xpathstring('(//a[@class="ui button"])[last()]'))
    if page == nil then page = 1; end
    updatelist.CurrentDirectoryPageNumber = page
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'MangaOnlineBiz'
  m.rooturl = 'https://manga-online.biz'
  m.category = 'Russian'
  m.lastupdated='March 3, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//h1/span[@class="name"]')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//img[@itemprop="image"]/@src'))
    mangainfo.genres=x.xpathstringall('//ul[@class="tagList"]/li/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//span[contains(@class, "status_tag")]'), 'выходит', 'издано')
    mangainfo.summary=x.xpathstring('//div[@itemprop="description"]')
    x.xpathhrefall('//ul[@class="chlist"]/li/h4/a', mangainfo.chapterlinks, mangainfo.chapternames)
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
    local s = x.xpathstring('//script[contains(., "Reader.init")]')
    s = GetBetween('.init(', ');', s)
    x.parsehtml(s)
    local dir = x.xpathstring('json(*).dir')
    local v = x.xpath('json(*).images()()')
    for i = 1,v.count,3 do
      local v1=v.get(i)
      task.pagelinks.add(dir .. v1.tostring)
    end
  else
    return false
  end
  return true
end

local dirurl = '/manga/?order_by=name&page='
function getnameandlink()
  if http.get(module.rooturl .. dirurl .. IncStr(url)) then
    local x = TXQuery.Create(http.Document)
    x.xpathhrefall('//div[@class="animeList"]/ol/li/div/h3/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. dirurl .. '1') then
    x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('//div[@class="PageNav"]/nav/a[last()-1]'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'DesuMe'
  m.rooturl = 'https://desu.me'
  m.category = 'Russian'
  m.lastupdated='March 3, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
end
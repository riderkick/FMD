function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    local s = x.xpathstring('//script[contains(., "var description")]')
    mangainfo.title = GetBetween('title="', '";', s)
    mangainfo.coverlink = MaybeFillHost(module.rooturl, GetBetween('imageUrl="', '";', s))
    mangainfo.summary = GetBetween('description="', '";', s)
    s = GetBetween('chapString="', '";', s)
    x.parsehtml(s)
    local v = x.xpath('//a')
    for i = 1, v.count do
      local v1 = v.get(i)
      mangainfo.chapterlinks.add(mangainfo.url .. '/' .. v1.getattribute('href'))
      mangainfo.chapternames.add(v1.toString)
    end
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(., "imageUrls")]')
    x.parsehtml(GetBetween('imageUrls =', ';', s))
    local v = x.xpath('json(*)()')
    for i = 1, v.count do
      task.pagelinks.add(MaybeFillHost(module.rooturl, v.get(i).toString))
    end
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl) then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//div[@class="index-mangas"]/div/a')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(v1.getattribute('href'))
      names.add(x.xpathstring('.//div[@class="index-manga-title"]', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'SiberOwl'
  m.rooturl = 'http://siberowl.com'
  m.category = 'English-Scanlation'
  m.lastupdated='April 10, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

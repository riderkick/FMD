function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//h1')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//meta[@property="og:image"]/@content'))
    mangainfo.summary=x.xpathstring('//div/div[h1]/following-sibling::div[@class="panel-body"]')
    local id = x.xpathstring('//input[@id="Identification"]/@value')
    local base = x.xpathstring('//script[contains(., "var chapterUrl")]')
    base = GetBetween("'", "'", base)
    if http.get(module.RootURL .. '/chapter/getall?mangaIdentification=' .. id) then
      x.parsehtml(http.document)
      local root = x.xpath('json(json(*).data)')
      local v = x.xpath('jn:members(result)', root)
      local t = {}
      for i = 1, v.count do
        table.insert(t, tonumber(x.xpathstring('Number', v.get(i))))
      end
      table.sort(t)
      for _, k in ipairs(t) do
        local chid = x.xpathstring('jn:members(result)[Number='..k..']/Identification', root)
        local chnum = x.xpathstring('jn:members(result)[Number='..k..']/FriendlyChapterNumber', root)
        mangainfo.chapterlinks.add(module.rooturl .. '/chapter/chapterIndexControls?identification=' .. chid)
        mangainfo.chapternames.add('Capítulo: ' .. chnum)
      end
    end
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    local v=x.xpath('//div[contains(@class,"PagesContainer")]/a/img/@id')
    for i = 1, v.count do
      local id = v.get(i).toString
      task.pagelinks.add(MaybeFillHost(module.rooturl, '/page/getPageImage/?identification='..id));
    end
  else
    return false
  end
  return true
end

function getnameandlink()
  local data = 'filter%5Bgeneres%5D%5B%5D=-1&filter%5BqueryString%5D=&filter%5Bskip%5D=0&filter%5Btake%5D=10000&filter%5Bsortby%5D=5&filter%5BbroadcastStatus%5D=0'
  if http.post(module.rooturl .. '/manga/getMangasConsultResult', data) then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//a[contains(@class,"manga-result")]')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(v1.getattribute('href'))
      names.add(x.xpathstring('.//h4', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'InManga'
  m.rooturl = 'https://inmanga.com'
  m.category = 'Spanish'
  m.lastupdated='April 16, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

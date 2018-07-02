function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//p[@class="title"]')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="episode_bnr"]/img/@src'))
    mangainfo.genres=x.xpathstringall('//p[@class="category"]/span'):gsub('#', '')
    mangainfo.summary=x.xpathstring('//p[@class="synop"]')
    local v = x.xpath('//table[@class="episode_list"]//tr[contains(@class, "episode_tr") and contains(@class, "not_need_pay")]')
    for i = 1, v.count do
      local v1 = v.get(i)
      local s = MaybeFillHost(module.RootURL, GetBetween('=\'', '\'', v1.getAttribute('onclick')))
      mangainfo.chapterlinks.add(s)
      s = x.xpathstring('td[@class="toon_title"]/div[@class="episode_subtitle"]', v1)
      mangainfo.chapternames.add(s)
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  http.headers.values['Referer'] = module.rooturl .. task.link
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(., "toon_img")]')
    x.parsehtml(DecodeBase64(GetBetween("toon_img = \"", "\";", s)))
    local v = x.xpath('//img/@src')
    for i = 1, v.count do
      task.pagelinks.add(MaybeFillHost(module.RootURL, v.get(i).toString))
    end
    task.pagecontainerlinks.values[0] = http.cookies.text
  else
    return false
  end
  return true
end

local dirurls = {
  '/manga', '/complete'
}

function getnameandlink()
  local lurl = dirurls[module.CurrentDirectoryIndex+1]
  if http.get(module.rooturl .. lurl) then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//ul[@id="comic_item_list"]/li/a')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(v1.getAttribute('href'))
      names.add(x.xpathstring('div/div/span[@class="title"]', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function beforedownloadimage()
  http.reset()
  http.cookies.text = task.pagecontainerlinks.values[0]
  http.headers.values['Referer'] = module.rooturl
  return true
end

function Init()
  local m = NewModule()
  m.category = 'Raw'
  m.Website = 'HoduComics'
  m.RootURL = 'https://hoducomics.com'
  m.lastupdated='June 24, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.onbeforedownloadimage='beforedownloadimage'
  m.totaldirectory = #dirurls
end

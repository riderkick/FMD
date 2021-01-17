function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//meta[@name="title"]/@content')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//*[@class="bt_thumb"]/a/img/@src'))
    mangainfo.authors=x.xpathstring('//meta[@name="author"]/@content')
    mangainfo.summary=x.xpathstring('//meta[@name="description"]/@content')
    local v = x.xpath('//table[@class="web_list"]/tbody//tr/td[@class="content__title"]')
    for i = 1, v.count do
      local v1 = v.get(i)
      mangainfo.chapterlinks.add(v1.getAttribute('data-role'))
      mangainfo.chapternames.add(v1.toString)
    end
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
    local s = x.xpathstring('//script[contains(., "toon_img")]')
    x.parsehtml(DecodeBase64(GetBetween("toon_img = '", "';", s)))
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
  '/%EC%9B%B9%ED%88%B0'
}

function getnameandlink()
  local lurl = dirurls[module.CurrentDirectoryIndex+1]
  if http.get(module.rooturl .. lurl) then
    local x = TXQuery.Create(http.Document)
    x.xpathhrefall('//ul[contains(@class, "homelist")]/li/div//a[@id="title"]', links, names)
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
  m.Website = 'Toonkor'
  m.RootURL = 'https://tkor.pro'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.onbeforedownloadimage='beforedownloadimage'
  m.totaldirectory = #dirurls
end

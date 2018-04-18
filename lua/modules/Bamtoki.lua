function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//div[@class="title-section-inner"]/div/h1')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="title-section-inner"]/div/img/@src'))
    mangainfo.genres=x.xpathstringall('//div[@class="title-section-inner"]//div[@class="toon-section"]/a')
    mangainfo.summary=x.xpathstring('//div[@class="title-section-inner"]//div[@class="toon-section"]/following-sibling::div')
    x.xpathhrefall('//ul[@class="list-container"]/li/div[@class="list-item"]/div[contains(@class, "list-details")]//a', mangainfo.chapterlinks,mangainfo.chapternames)
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
    local s = DecodeBase64(x.xpathstring('//*[@id="tooncontentdata"]'))
    x.parsehtml(s)
    x.xpathstringall('//img/@src', task.pagelinks)
  else
    return false
  end
  return true
end

local dirurls = {
  '/%EC%9B%B9%ED%88%B0/%EC%9E%91%ED%92%88?sort=%EC%A0%9C%EB%AA%A9',
  '/%EB%A7%9D%EA%B0%80/%EC%9E%91%ED%92%88?sort=%EC%A0%9C%EB%AA%A9'
}

function getnameandlink()
  local lurl = dirurls[module.CurrentDirectoryIndex+1]
  if http.get(module.rooturl .. lurl) then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//div[@class="list-container"]/div/div')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(x.xpathstring('./a/@href', v1))
      names.add(x.xpathstring('./div/h3', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'Bamtoki'
  m.rooturl = 'https://webtoon.bamtoki.com'
  m.category = 'Raw'
  m.lastupdated='April 11, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.totaldirectory = #dirurls
end

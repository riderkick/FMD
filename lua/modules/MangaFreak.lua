function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.XPathString('//div[@class="manga_series_data"]/h5')
    end
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('//div[@class="manga_series_image"]/img/@src'))
    mangainfo.genres=x.xpathstringall('//div[@class="series_sub_genre_list"]/a')
    mangainfo.summary=x.xpathstring('//div[@class="manga_series_description"]'):gsub('Manga Description', '')
    x.xpathhrefall('//div[@class="manga_series_list"]/table/tbody/tr/td[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.XPathStringAll('//img[@id="gohere"]/@src', task.PageLinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.RootURL .. '/Mangalist/All/'..IncStr(url)) then
    local x = TXQuery.Create(http.Document)
    local v=x.xpath('//*[contains(@class, "list_item_content")]//div[contains(@class, "list_item")]')
    for i=1,v.count do
      local v1=v.get(i)
      local title = x.xpathstring('.//*[contains(@class, "list_item_info")]/h3', v1)
      local link = x.xpathstring('.//*[contains(@class, "list_item_info")]/h3/a/@href', v1)
      names.add(title)
      links.add(link)
    end
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. '/Mangalist') then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.xpathstring('//div[contains(@class,"pagination")]/a[contains(@class,"last_p")]/@href/substring-after(.,"All/")'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m=NewModule()
  m.category='English'
  m.website='MangaFreak'
  m.rooturl='https://w11.mangafreak.net'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
end

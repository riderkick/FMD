function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.XPathString('//div[@class="manga_series_data"]/h5')
    end
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('//div[@class="manga_series_image"]/img/@src'))
    --mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//ul[@class="manga-info"]/li[contains(., "Status")]//a'))
    --mangainfo.authors=x.xpathstring('//ul[@class="manga-info"]/li[contains(., "Author")]//a')
    mangainfo.genres=x.xpathstringall('//div[@class="series_sub_genre_list"]/a')
    --mangainfo.summary=x.xpathstring('//h3[text()="Description"]/following-sibling::p')
    x.xpathhrefall('//div[@class="manga_series_list"]/table/tbody/tr/td[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getimageurl()
  local lurl=MaybeFillHost(module.rooturl,url)
  if workid~=0 then lurl=lurl..'_'..workid+1 end
  if http.get(lurl) then
    x=TXQuery.Create(http.Document)
    task.pagelinks[workid]=x.xpathstring('//div[@class="read_image"]//img/@src')
    return true
  else
    return false
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    task.pagenumber = x.xpathcount('//div[@class="read_selector"]/select/option')
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.RootURL .. '/Mangalist') then
    local x = TXQuery.Create(http.Document)
    x.xpathhrefall('//div[@class="manga_list"]/div/div[@class="manga_item"]/div/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m=NewModule()
  m.category='English'
  m.website='MangaFreak'
  m.rooturl='http://www.mangafreak.net'
  m.lastupdated='April 9, 2018'
  m.totaldirectory=1
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetimageurl='getimageurl'
end

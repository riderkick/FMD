local dirurl = {
  ['TruyenChon'] = '/the-loai?status=-1&sort=15&page=%s',
  ['NetTruyen'] = '/tim-truyen?status=-1&sort=15&page=%s'
}

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//h1[@class="title-detail"]')
    end
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('//div[contains(@class, "col-image")]/img/@src'))
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//li[contains(@class, "status")]/p[2]'), 'Đang tiến hành', 'Hoàn thành')
    mangainfo.authors=x.xpathstring('//li[contains(@class, "author")]/p[2]')
    mangainfo.artists=x.xpathstring('//h4[starts-with(./label,"Artista")]/substring-after(.,":")')
    mangainfo.genres=x.xpathstringall('//li[contains(@class, "kind")]/p[2]/a')
    mangainfo.summary=x.xpathstring('//div[@class="detail-content"]/p')
    x.xpathhrefall('//div[@class="list-chapter"]//ul/li/div[contains(@class, "chapter")]/a', mangainfo.chapterlinks, mangainfo.chapternames)
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
    x.xpathstringall('//div[@class="page-chapter"]/img/@data-original', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.RootURL .. dirurl[module.website]:format(IncStr(url))) then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//div[@class="item"]//h3/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. dirurl[module.website]:format('1')) then
    local x = TXQuery.Create(http.Document)
    local s = x.xpathstring('//ul[@class="pagination"]/li[last()]/a/@href')
    page = tonumber(s:match('&page=(%d+)'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(name, url)
  local m=NewModule()
  m.category='Vietnamese'
  m.website=name
  m.rooturl=url
  m.lastupdated='June 15, 2018'
  m.sortedlist = true
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
  return m
end

function Init()
  AddWebsiteModule('TruyenChon', 'http://truyenchon.com')
  AddWebsiteModule('NetTruyen', 'http://www.nettruyen.com')
end

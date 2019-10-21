function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)	
    mangainfo.title=x.xpathstring('//h1')
    if module.website == 'NHentai' then
       mangainfo.coverlink=x.xpathstring('//*[@id="cover"]//img/@data-src')
       mangainfo.artists=x.xpathstringall('//*[@id="tags"]//a[contains(@href,"/artist/")]/text()')
    else
       mangainfo.coverlink=x.xpathstring('//*[@id="cover"]//img/@src')
       mangainfo.artists=x.xpathstringall('//*[@id="tags"]//a[contains(@href,"/artists/")]/text()')
    end
    mangainfo.genres=x.xpathstringall('//*[@id="tags"]//a/text()')
    mangainfo.summary=x.xpathstring('//div[contains(@class, "drop-discription")]/p/text()')
    mangainfo.chapterlinks.add(url)
    mangainfo.chapternames.add(mangainfo.title)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    TXQuery.Create(http.Document).xpathstringall('//*[@class="thumb-container"]//img/@data-src/replace(.,"//t\\.(.+\\d+)t\\.","//i.$1.")',task.pagelinks)
    return true
  else
    return false
  end
  return true
end

function getdirectorypagenumber()
  if http.get(module.rooturl) then
    page=tonumber(TXQuery.Create(http.document).xpathstring('//a[@class="last"]/@href/substring-after(.,"=")'))
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  if http.get(module.rooturl..'/?page='..IncStr(url)) then
    TXQuery.Create(http.document).xpathhrefall('//*[@id="content"]/div/div[@class="gallery"]/a',links,names)
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(name, url, cat)
  local m=NewModule()
  m.category=cat
  m.website=name
  m.rooturl=url
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
  m.ongetnameandlink='getnameandlink'
  m.sortedlist=true
  return m
end

function Init()
local cat = 'H-Sites'
  AddWebsiteModule('NHentai', 'https://nhentai.net', cat)
  AddWebsiteModule('HentaiHand', 'https://hentaihand.com', cat)
end

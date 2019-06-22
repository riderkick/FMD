function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title=x.xpathstring('//h1[@class="page-title"]')
    end
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//img[@class="series"]/@src'))
    mangainfo.authors=x.xpathstringall('//li[@class="info" and contains(*, "Author")]/text()', '')
    mangainfo.artists = mangainfo.authors
    mangainfo.genres=x.xpathstringall('//li[@class="info" and contains(*, "Genre")]/text()', '')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstringall('//li[@class="info" and contains(*, "Status")]/text()', ''))
    mangainfo.summary=x.xpathstringall('//li[@class="summary"]/text()', '')
    x.xpathhrefall('//div[@id="chapterlist"]/li/a',mangainfo.chapterlinks,mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    http.reset()
    http.headers.values['Referer'] = mangainfo.url
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    task.pagenumber = x.xpathcount('(//select[@name="page"])[1]/option')
  else
    return false
  end
  return true
end

function getimageurl()
  local lurl=MaybeFillHost(module.rooturl,url)
  if workid~=0 then lurl=lurl..'/'..(workid+1) end
  if http.get(lurl) then
    local x=TXQuery.Create(http.Document)
    local base = x.xpathstring("//base/@href")
    task.pagelinks[workid]=MaybeFillHost(base, x.xpathstring('//img[@class="picture"]/@src'))
    return true
  else
    return false
  end
end

function beforedownloadimage()
  http.headers.values['Referer'] = module.rooturl
  return true
end

function getnameandlink()
  if http.get(module.rooturl .. '/comics-list') then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//div[@id="content-wrap"]//table//tr/td/div/span/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'ReadComicBooksOnline'
  m.rooturl = 'https://readcomicbooksonline.org'
  m.category = 'English'
  m.lastupdated='April 26, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetimageurl='getimageurl'
  m.onbeforedownloadimage = 'beforedownloadimage'
end

local ALPHA_LIST_UP = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ'

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if Pos('waring', url) == 0 then mangainfo.url = mangainfo.url .. '?waring=1' end
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//div[@class="book-info"]/h1/substring-before(., " Manga")')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="book-info"]//img/@src'))
    mangainfo.authors=x.xpathstringall('//dd[@class="about-book"]/p[contains(span, "Author")]/a')
    mangainfo.genres=x.xpathstringall('//div[@class="book-info"]/ul/li[position()>1]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//dd[@class="about-book"]/p[contains(span, "Status")]/a'))
    mangainfo.summary=x.xpathstring('//dd[@class="short-info"]/p/span')
    local v=x.xpath('//ul[@class="chapter-box"]/li/div[@class="chapter-name long"]/a')
    for i=1,v.count do
      local v1=v.get(i)
      mangainfo.chapterlinks.add(v1.getattribute('href'))
      mangainfo.chapternames.add(x.xpathstring('text()', v1))
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber=0
  if http.get(MaybeFillHost(module.rooturl, url)) then
    task.pagenumber=TXQuery.Create(http.Document).xpathcount('(//select[@class="sl-page"])[1]/option')
    module.storage.text = http.cookies.text
  else
    return false
  end
  return true
end

function BeforeDownloadImage()
  http.headers.values['Referer'] = module.rooturl
  return true
end

function getimageurl()
  http.headers.values['Referer'] = module.rooturl
  http.cookies.text = module.storage.text
  if http.get(MaybeFillHost(module.rooturl,url):gsub('/?$','-'..(workid+1)..'.html')) then
    task.pagelinks[workid]=TXQuery.create(http.document).xpathstring('//img[contains(@class, "manga_pic")]/@src')
    return true
  end
  return false
end

function getnameandlink()
  local s = '0-9'
  if module.CurrentDirectoryIndex ~= 0 then
    s = ALPHA_LIST_UP:sub(module.CurrentDirectoryIndex+1,module.CurrentDirectoryIndex+1)
  end
  if http.get(module.rooturl .. '/category/' .. s .. '_views_' .. IncStr(url) .. '.html') then
    local x = TXQuery.Create(http.Document)
    local q = '//ul[@id="list_container"]/li/dl/dt/a'
    x.XPathHREFtitleAll(q, links, names)
    if x.xpathcount(q) > 0 then
      updatelist.CurrentDirectoryPageNumber = updatelist.CurrentDirectoryPageNumber + 1
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'TenManga'
  m.rooturl = 'http://www.tenmanga.com'
  m.category = 'English'
  m.lastupdated='February 26, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetimageurl='getimageurl'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
  m.totaldirectory = ALPHA_LIST_UP:len()
end

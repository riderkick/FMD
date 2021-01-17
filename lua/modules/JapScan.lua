local ALPHA_LIST_UP = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ'

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//h1')
      mangainfo.title = string.gsub(mangainfo.title, '^Manga ', '')
      mangainfo.title = string.gsub(mangainfo.title, '^Manhua ', '')
      mangainfo.title = string.gsub(mangainfo.title, '^Manhwa ', '')
      mangainfo.title = string.gsub(mangainfo.title, ' VF$', '')
    end
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@id="main"]//img/@src'))
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstringall('//div[@id="main"]//p[contains(span, "Statut")]/text()', ''), 'En Cours', 'Termine')
    mangainfo.authors=x.xpathstringall('//div[@id="main"]//p[contains(span, "Auteur")]/text()', '')
    mangainfo.artists=x.xpathstringall('//div[@id="main"]//p[contains(span, "Artiste")]/text()', '')
    mangainfo.genres=x.xpathstringall('//div[@id="main"]//p[contains(span, "Type(s)")]/text()', '')
    mangainfo.summary=x.xpathstring('//div[@id="main"]//div[contains(text(), "Synopsis")]/following-sibling::*')
    x.xpathhrefall('css("div#chapters_list div.chapters_list > a")', mangainfo.chapterlinks, mangainfo.chapternames)
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
    task.pagenumber = x.xpathcount('//select[@id="pages"]/option/@value')
  else
    return false
  end
  return true
end

function getimageurl()
  local s = AppendURLDelim(url)..(workid+1)..'.html'
  if http.get(MaybeFillHost(module.rooturl,s)) then
    task.pagelinks[workid]=TXQuery.create(http.document).xpathstring('//div[@id="image"]/@data-src')
    return true
  end
  return false
end

function getnameandlink()
  local s = '0-9'
  if module.CurrentDirectoryIndex ~= 0 then
    s = ALPHA_LIST_UP:sub(module.CurrentDirectoryIndex+1,module.CurrentDirectoryIndex+1)
  end
  if http.get(MaybeFillHost(module.rooturl, '/mangas/' .. s .. '/' .. IncStr(url))) then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//div[@id="main"]//p/a[contains(@href, "/manga/")]', links, names)
    local page = tonumber(x.XPathString('//ul[contains(@class, "pagination")]/li[last()]/a'))
    if page == nil then
      page = 1
    end
    updatelist.CurrentDirectoryPageNumber = page
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m=NewModule()
  m.category='French'
  m.website='Japscan'
  m.rooturl='http://www.japscan.co'
  m.lastupdated='April 6, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetimageurl='getimageurl'
  m.totaldirectory=ALPHA_LIST_UP:len()
end

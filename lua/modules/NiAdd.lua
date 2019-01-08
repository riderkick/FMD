function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title=x.xpathstring('css("h1.manga-title")')
    end
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('css("img.detail-cover")/@src'))
    mangainfo.authors=x.xpathstringall('//*[@itemprop="author" and contains(span, "Author")]/a')
    mangainfo.artists=x.xpathstringall('//*[@itemprop="author" and contains(span, "Artist")]/a')
    mangainfo.genres=x.xpathstringall('//*[contains(span, "Genres")]/a')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('css(".status-tag")'))
    mangainfo.summary=x.xpathstring('css(".summary-box")/span[@itemprop]')
    x.xpathhreftitleall('css("ul.detail-chlist > a")', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
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
    task.pagenumber = x.xpathcount('(//div[contains(@class, "mangaread-pagenav")])[1]/select[@class="sl-page"]/option')
  else
    return false
  end
  return true
end

function getnameandlink()
  local s = string.format("/category/index_%s.html?sort=name", IncStr(url))
  if http.get(module.rooturl .. s) then
    local x = TXQuery.Create(http.Document)
    local p=x.xpathstring('//div[@class="page-nav"]/a[last()-1]')
    p = tonumber(p)
    if p ~= nil then
      updatelist.CurrentDirectoryPageNumber = p
    end
    x.XPathHREFAll('css("p.title > a")', links, names)
    return no_error
  else
    return net_problem
  end
end

function getimageurl()
  local s = MaybeFillHost(module.RootURL, url)
  s = string.format('%s-%s.html', s:gsub('/$', ''), IncStr(workid))
  if http.GET(s) then
    local x = TXQuery.Create(http.Document)        
    task.pagelinks[workid] = x.xpathstring('css("img.manga_pic")/@src')
    return true
  else
    return false
  end
end

function AddWebsiteModule(name, url, cat)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = cat
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetimageurl = 'getimageurl'
end

function Init()
  AddWebsiteModule('NiAdd', 'https://www.niadd.com', 'English')
end 
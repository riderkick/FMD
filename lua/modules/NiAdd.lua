function getinfo()
  if url:find('/chapters') then
    mangainfo.url=MaybeFillHost(module.RootURL, url)
  else
    mangainfo.url=MaybeFillHost(module.RootURL, url..'/chapters'):gsub('.html', '')
  end
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title=x.xpathstring('//h1')
    end
    mangainfo.coverlink=x.xpathstring('//img[@itemprop="image"]/@src')
    mangainfo.authors=x.xpathstringall('//td[@class="bookside-general-type"]//div[@itemprop="author" and contains(span, "Author")]/a/span')
    mangainfo.artists=x.xpathstringall('//td[@class="bookside-general-type"]//div[@itemprop="author" and contains(span, "Artist")]/a/span')
    mangainfo.genres=x.xpathstringall('//td[@class="bookside-general-type"]//div[contains(span, "Genres")]/a/span')
    local n = x.xpath('//span[@class="chp-title"]')
    local v = x.xpath('//ul[contains(@class, "chapter-list")]/a')
    for i = 1, v.count do
      local v1 = v.get(i)
      mangainfo.chapternames.Add(n.get(i).toString)
      mangainfo.chapterlinks.Add(v.get(i).getAttribute('href'))
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
    x.xpathstringall('(//select[@class="sl-page"])[last()]/option/@value', task.pagecontainerlinks)
    task.pagenumber = task.pagecontainerlinks.count
  else
    return false
  end
  return true
end

function getnameandlink()
  local s = string.format("/category/index_%s.html?sort=name", IncStr(url))
  if http.get(module.rooturl .. s) then
    local x = TXQuery.Create(http.Document)
    local p=x.xpathstring('//div[@class="page-all-num"]/substring-after(.,"All ")')
    p = tonumber(p)
    if p ~= nil then
      updatelist.CurrentDirectoryPageNumber = p
    end
    local v = x.XPath('//div[contains(@class, "manga-list")]//div[@class="manga-item"]//td[2]/a[1]')
    for i = 1, v.Count do
      links.Add(v.Get(i).GetAttribute('href'):gsub('.html', '') .. '/chapters')
      names.Add(x.XPathString('div', v.Get(i)))
    end
    return no_error
  else
    return net_problem
  end
end

function getimageurl()
  local s = MaybeFillHost(module.RootURL, task.pagecontainerlinks[workid])
  if http.GET(s) then      
    task.pagelinks[workid] = TXQuery.Create(http.Document).xpathstring('//img[contains(@class,"manga_pic")]/@src')
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

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = Trim(x.XPathString('//h1[contains(@class, "element-title")]/text()'))
    end
    mangainfo.coverlink = x.xpathstring('//img[contains(@class,"book-thumbnail")]/@src')
    mangainfo.genres=x.xpathstringall('//a[contains(@class, "badge")]')
    mangainfo.authors=Trim(x.xpathstring('//span[@class="list-group-item" and contains(., "Autor")]/a'))
    mangainfo.artists=Trim(x.xpathstring('//span[@class="list-group-item" and contains(., "Artist")]/a'))
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//span[contains(@class, "book-status")]'), 'public', 'final')
    mangainfo.summary = x.xpathstringall('//*[@class="element-description"]/text()', '')
    local v = x.xpath('//div[contains(@class, "chapters")]/ul//li')
    for i = 1, v.count do
      local v1 = v.get(i)
      local name = x.xpathstring('h4', v1)
      local w = x.xpath('div//ul/li/div', v1)
      for j = 1, w.count do
        local w1 = w.get(j)
        local scan = '[' .. x.xpathstring('div[1]', w1) .. ']'
        mangainfo.chapterlinks.add(x.xpathstring('div[contains(@class, "text-right")]/a/@href', w1))
        mangainfo.chapternames.add(name .. ' ' .. scan)
      end
    end
    if mangainfo.chapterlinks.count == 0 then
      local w = x.xpath('//ul[contains(@class, "chapter-list")]/li/div')
      for j = 1, w.count do
        local w1 = w.get(j)
        local scan = '[' .. x.xpathstring('div[1]', w1) .. ']'
        mangainfo.chapterlinks.add(x.xpathstring('div[contains(@class, "text-right")]/a/@href', w1))
        mangainfo.chapternames.add(mangainfo.title .. ' ' .. scan)
      end
    end
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  http.headers.values['Referer'] = module.rooturl:gsub('http://', 'https://')
  if not http.get(MaybeFillHost(module.rooturl, url):gsub('http://', 'https://')) then return false; end
  local x = TXQuery.Create(http.Document)
  local u = x.xpathstring('//meta[@property="og:url"]/@content')
  if string.match(u, 'cascade') then
    u = string.gsub(u, '/cascade', '/paginated')
    print(u)
    if not http.get(MaybeFillHost(module.rooturl, u):gsub('http://', 'https://')) then return false; end
    x = TXQuery.Create(http.Document)
  end
  task.pagenumber = tonumber(x.xpathstring('(//select[@id="viewer-pages-select"])[1]/option[last()]/text()'))
  for i = 1, task.pagenumber do
    task.pagecontainerlinks.add(u..'/'..i)
  end
  return true
end

function getimageurl()
  local s = MaybeFillHost(module.rooturl, task.pagecontainerlinks[workid]):gsub('http://', 'https://')
  http.headers.values['Referer'] = module.rooturl:gsub('http://', 'https://')
  if http.get(s) then
    task.pagelinks[workid] = TXQuery.Create(http.document).xpathstring('//img[@class="viewer-image"]/@src')
    
    return true
  end
  return false
end

function getnameandlink()
  local s = '/library?order_item=alphabetically&order_dir=asc&filter_by=title&page='..IncStr(url)
  if http.GET(module.RootURL .. s) then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//*[@data-identifier]/a')
    local hasTitles = false
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(v1.getAttribute('href'))
      names.add(x.xpathstring('div/div[@class="thumbnail-title"]', v1))
      hasTitles = true
    end
    if hasTitles then
      updatelist.CurrentDirectoryPageNumber = updatelist.CurrentDirectoryPageNumber + 1
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'Tumangaonline'
  m.rooturl = 'http://tmofans.com' -- Don't set to https because TMO set http for Cloudflare!
  m.category = 'Spanish'
  m.maxtasklimit = 1
  m.maxconnectionlimit = 1
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetimageurl='getimageurl'
end

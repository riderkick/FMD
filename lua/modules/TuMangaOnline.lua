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
        local scan = '[' .. x.xpathstring('div[1]//a', w1) .. ']'
        mangainfo.chapterlinks.add(x.xpathstring('div[contains(@class, "text-right")]/a/@href', w1))
        mangainfo.chapternames.add(name .. ' ' .. scan)
      end
    end
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagecontainerlinks.clear()
  http.headers.values['Referer'] = module.rooturl
  if not http.get(MaybeFillHost(module.rooturl, url)) then return false; end
  if http.lasturl:match('paginated$') ~= nil then
    if not http.get(http.lasturl:gsub('paginated$', 'cascade')) then return false; end
  end
  local x=TXQuery.Create(http.Document)
  x.xpathstringall('//img[contains(@class, "viewer-image")]/@data-original', task.pagelinks)
  task.pagecontainerlinks.text = http.lasturl
  return true
end

--[[
function getnameandlink()
  if http.GET(module.RootURL .. '/changeMangaList?type=text') then
    TXQuery.Create(http.Document).XPathHREFAll('//li/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL) then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.xpathstring('//*[@class="pagination"]/a[last()-1]'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end
--]]

function Init()
  local m = NewModule()
  m.website = 'Tumangaonline'
  m.rooturl = 'https://tumangaonline.me'
  m.category = 'Spanish'
  m.lastupdated='June 14, 2018'
  m.maxtasklimit = 1
  m.maxconnectionlimit = 1
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  --m.ongetnameandlink='getnameandlink'
  --m.ongetdirectorypagenumber = 'getdirectorypagenumber'
end

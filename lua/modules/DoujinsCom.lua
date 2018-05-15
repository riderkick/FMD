local dirurl = '/list?type=&sortType=DATE_CREATE'
local perpage = 60

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//div[@class="folder-title"]/a[last()]')
    end
    mangainfo.coverlink = x.xpathstring('(//img[@class="doujin"])[1]/@data-thumb')
    print(x.xpathstring('(//img[@class="doujin"])[1]/@data-thumb'))
    mangainfo.authors=x.xpathstringall('//div[@class="gallery-artist"]/a')
    mangainfo.artists=x.xpathstringall('//div[@class="gallery-artist"]/a')
    mangainfo.genres=x.xpathstringall('//li[@class="tag-area"]/a')
    mangainfo.chapterlinks.add(mangainfo.url)
    mangainfo.chapternames.add(mangainfo.title)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//img[@class="doujin"]/@data-file', task.pagelinks)
  else
    return false
  end
  return true
end

--[[
function getdirectorypagenumber()
  if http.GET(module.RootURL .. dirurl) then
    local x = TXQuery.Create(http.Document)
    local s = x.xpathstring('//*[@class="pagination"]/a[@class="step"][last()]/@href')
    page = tonumber(s:match('offset=(%d+)'))
    if page == nil then page = 1 end
    if page > 1 then page = math.ceil(page / perpage) + 1; end
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  local s = module.rooturl .. dirurl
  if url ~= '0' then s = s .. '&offset=' .. (tonumber(url) * perpage) .. '&max=' .. perpage; end
  if http.get(s) then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//table[@class="cTable"]//tr/td/a[not(@class)]')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(v1.getattribute('href'))
      names.add(x.xpathstring('./text()', v1))
    end
    return no_error
  else
    return net_problem
  end
end
]]--

function Init()
  local m = NewModule()
  m.website = 'DoujinsCom'
  m.rooturl = 'https://doujins.com/'
  m.category = 'H-Sites'
  m.lastupdated='May 14, 2018'
  m.sortedlist = true
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  --m.ongetnameandlink='getnameandlink'
  --m.ongetdirectorypagenumber = 'getdirectorypagenumber'
end
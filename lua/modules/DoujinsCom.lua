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

function today()
  local now = os.date('*t');
  return os.time({year=now.year, month=now.month, day=now.day})
end

local endDate = os.time({year=2007, month=9, day=30})
local step = 30 * 24 * 60 * 60

function getdirectorypagenumber()
  page = math.ceil((today() - endDate) / step)
  return no_error
end

function getnameandlink()
  local to = today() - tonumber(url) * step
  local from = to - step + 1
  if from < endDate then return no_error; end
  if http.xhr(module.rooturl .. string.format('/folders?start=%d&end=%d', from, to)) then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('json(*).folders()')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(x.xpathstring('link', v1))
      names.add(x.xpathstring('name', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'DoujinsCom'
  m.rooturl = 'https://doujins.com'
  m.category = 'H-Sites'
  m.lastupdated='May 14, 2018'
  m.sortedlist = true
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
end
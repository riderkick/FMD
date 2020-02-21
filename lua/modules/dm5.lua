function getinfo()
  mangainfo.url = MaybeFillHost(module.RootURL, url)
  http.cookies.values['isAdult'] = '1'
  if http.get(mangainfo.url) then
    local x = TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title=x.xpathstring('//div[@class="banner_detail_form"]//p[@class="title"]/text()')
    end
    mangainfo.coverlink=MaybeFillHost(module.rooturl,x.xpathstring('//div[@class="cover"]/img/@src'))
    mangainfo.authors=x.xpathstringall('//div[@class="info"]/p[@class="subtitle"]/a')
    mangainfo.genres=x.xpathstringall('//div[@class="info"]/p[@class="tip"]/span[contains(., "题材")]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="info"]/p[@class="tip"]/span[contains(., "状态")]/span'), '连载中', '已完结');
    mangainfo.summary=x.xpathstring('//div[@class="info"]/p[@class="content"]')
    v=x.xpath('//div[@id="chapterlistload"]/ul')
    for i = 1, v.count do
      local v1 = v.get(i)
      local w = x.xpath('.//li/a', v1)
      for j = 1, w.count do
        local w1 = w.get(j)
        mangainfo.chapterlinks.add(module.RootURL .. w1.getAttribute('href'))
        mangainfo.chapternames.add(w1.toString)
      end
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function urlencode(str)
  if (str) then
    str = string.gsub (str, "\n", "\r\n")
    str = string.gsub (str, "([^%w ])",
       function (c) return string.format ("%%%02X", string.byte(c)) end)
    str = string.gsub (str, " ", "+")
  end
  return str    
end

local js = require 'utils.jsunpack'
function gettext(s)
  s = SeparateRight(s, "}('")
  local text = SeparateLeft(s, "',")
  local a = tonumber(GetBetween("',", ",", s))
  s = SeparateRight(s, "',")
  local c = tonumber(GetBetween(",", ",'", s))
  local w = js.splitstr(GetBetween(",'", "'", s), '|')
  return js.unpack36(text, a, c, w)
end

function getpagenumber()
  local u = MaybeFillHost(module.rooturl,url)
  if http.get(u) then
    local x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(., "DM5_MID")]')
    local dm5mid = GetBetween('DM5_MID=', ';', s)
    local dm5cid = GetBetween('DM5_CID=', ';', s)
    local dm5viewsign = GetBetween('DM5_VIEWSIGN="', '";', s)
    local dm5viewsigndt = GetBetween('DM5_VIEWSIGN_DT="', '";', s)
    local dm5key = x.xpathstring('//*[@id="dm5_key"]/@value')
    local dm5page, cnt = 1, 0
    
    local total = tonumber(x.xpathstring('(//div[@id="chapterpager"])[1]/a[last()]'))
    if total == nil then total = 0 end
    
    while (cnt < total) and (dm5page <= total) do
      local xhrurl = string.format('%s/chapterfun.ashx?cid=%s&page=%d&key=%s&language=1&gtk=6&_cid=%s&_mid=%s&_dt=%s&_sign=%s',
        RemoveURLDelim(u), dm5cid, dm5page, dm5key, dm5cid, dm5mid, urlencode(dm5viewsigndt), dm5viewsign)
      if http.terminated then break end
      http.reset()
      http.headers.values['Referer'] = u
      if http.xhr(xhrurl) then
        local s = gettext(StreamToString(http.document))
        local cid = GetBetween('cid=', ';', s)
        local key = GetBetween("key=\\'", "\\';", s)
        local pix = GetBetween('pix="', '";', s)
        local pvalue = '[' .. GetBetween('pvalue=[', '];', s) .. ']'
        x.parsehtml(pvalue)
        local v = x.xpath('json(*)()')
        for i = 1, v.count do
          local v1 = v.get(i)
          task.pagelinks.add(pix .. v1.toString .. string.format('?cid=%s&key=%s', cid, key))
          cnt = cnt + 1
        end
        dm5page = dm5page + v.count
      else
        return false
      end
    end
    task.pagecontainerlinks.text = u
    return true
  else
    return false
  end
end

function BeforeDownloadImage()
  http.headers.values['Referer'] = task.pagecontainerlinks.text
  return true
end

local perpage = 100
function getdirectorypagenumber()
  local u = string.format('%s/dm5.ashx?t=%d', module.rooturl, os.time()*1000)
  local data = 'pagesize=1&pageindex=1&tagid=0&areaid=0&status=0&usergroup=0&pay=-1&char=&sort=18&action=getclasscomics'
  if http.POST(u, data) then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('json(*).Count'))
    if page == nil then
      page = 1
    else
      page = math.ceil(page / perpage)
    end
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  local u = string.format('%s/dm5.ashx?t=%d', module.rooturl, os.time()*1000)
  local data = string.format('pagesize=%d&pageindex=%d&tagid=0&areaid=0&status=0&usergroup=0&pay=-1&char=&sort=18&action=getclasscomics', perpage, IncStr(url))
  if http.post(u, data) then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('json(*).UpdateComicItems()')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(module.rooturl .. '/' .. x.xpathstring('UrlKey', v1) .. '/')
      names.add(x.xpathstring('Title', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Raw'
  m.website='DM5'
  m.rooturl='http://www.dm5.com'
  m.lastupdated='May 23, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
  m.ongetnameandlink='getnameandlink'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
  m.sortedlist = true
end

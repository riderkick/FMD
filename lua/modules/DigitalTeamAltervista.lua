function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.XPathString('//div[contains(@class, "nomob")]')
    end
    mangainfo.coverlink = MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="cover"]/img/@src'))
    mangainfo.genres=x.xpathstring('//div[@class="info"]/ul/li[contains(span[@class="info_name"], "Genere")]/span[@class="info_content"]')
    mangainfo.authors=x.xpathstring('//div[@class="info"]/ul/li[contains(span[@class="info_name"], "Autore")]/span[@class="info_content"]')
    mangainfo.artists=x.xpathstring('//div[@class="info"]/ul/li[contains(span[@class="info_name"], "Artista")]/span[@class="info_content"]')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="info"]/ul/li[contains(span[@class="info_name"], "Status")]/span[@class="info_content"]'), 'In corso', 'Completo')
    mangainfo.summary = x.xpathstringall('//div[@class="plot"]/text()', '')
    x.xpathhrefall('//div[@class="chapter_list"]/ul/li/div[@class="ch_top"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
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

function getpagenumber()
  task.pagelinks.clear()
  task.pagecontainerlinks.clear()
  if not http.get(MaybeFillHost(module.rooturl, url)) then return false; end
  local x=TXQuery.Create(http.Document)
  local s = x.xpathstring('//script[contains(., "a_url")]')
  local a_url = '/reader'
  local m = GetBetween("m='", "'", s)
  local ch = GetBetween("ch='", "'", s)
  local chs = GetBetween("chs='", "'", s)
  local title = urlencode(x.xpathstring('//title'))
  local data = string.format("info%%5Bmanga%%5D=%s&info%%5Bchapter%%5D=%s&info%%5Bch_sub%%5D=%s&info%%5Btitle%%5D=%s", m, ch, chs, title)
  http.reset()
  if not http.post(module.rooturl .. a_url .. '/c_i', data) then return false; end
  s = StreamToString(http.Document):gsub('\\"', '"'):gsub('^"', ''):gsub('"$', '')
  x.parsehtml(s)
  s = x.xpathstring('json(*)()[3]')
  local cnt = x.xpathcount('json(*)()[1]()')
  for i = 1, cnt do
    local name = x.xpathstring('json(*)()[1]()['..i..']/name') .. x.xpathstring('json(*)()[2]()['..i..']')
    local ex = x.xpathstring('json(*)()[1]()['..i..']/ex')
    task.pagelinks.add(module.rooturl .. a_url .. s .. name .. ex)
  end
  return true
end

function getnameandlink()
  if http.GET(module.RootURL .. '/reader/series') then
    local x = TXQuery.Create(http.Document)
    x.xpathhrefall('//div[@class="manga_title"]/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'DigitalTeamAltervista'
  m.rooturl = 'http://digitalteam1.altervista.org'
  m.category = 'Italian-Scanlation'
  m.lastupdated='June 21, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

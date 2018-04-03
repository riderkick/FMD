function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)	
    mangainfo.title=x.xpathstring('//h2[contains(@class, "works-intro-title")]')
    mangainfo.coverlink=MaybeFillHost(module.rooturl,x.xpathstring('//div[contains(@class,"works-cover")]/a/img/@src'))
    mangainfo.authors=x.xpathstring('//p[@class="works-intro-digi"]/span[contains(., "作者")]/em/substring-before(., " ")')
    mangainfo.summary=x.xpathstring('//p[contains(@class,"works-intro-short")]')
    x.xpathhrefall('//div[@id="chapter"]//ol[contains(@class, "chapter-page-all")]/li//a', mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function decode1(str)
  local _keyStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
  str = str:gsub('[^A-Za-z0-9%+/=]', '')
  local a = ''
  local b, d, h, f, g, e = 0, 0, 0, 0, 0, 0
  while e < str:len() do
    e = e + 1; b = string.find(_keyStr, str:sub(e, e)); if b == nil then b = -1 else b = b - 1 end;
    e = e + 1; d = string.find(_keyStr, str:sub(e, e)); if d == nil then d = -1 else d = d - 1 end;
    e = e + 1; f = string.find(_keyStr, str:sub(e, e)); if f == nil then f = -1 else f = f - 1 end;
    e = e + 1; g = string.find(_keyStr, str:sub(e, e)); if g == nil then g = -1 else g = g - 1 end;
    b = b << 2 | d >> 4
    d = (d & 15) << 4 | f >> 2
    h = (f & 3) << 6 | g
    a = a .. string.char(b)
    if f ~= 64 then a = a .. string.char(d); end
    if g ~= 64 then a = a .. string.char(h); end
  end
  function utf8_decode(c)
    local a, b, d, c1, c2 = '', 0, 0, 0, 0
    while b < c:len() do
      d = c:byte(b+1)
      if 128 > d then
        a = a .. string.char(d)
        b = b + 1
      else
        if (191 < d) and (224 > d) then
          c2 = c:byte(b+2)
          a = a .. string.char((d & 31) << 6 | c2 & 63)
          b = b + 2
        else
          c2 = c:byte(b+2)
          c3 = c:byte(b+3)
          a = a .. string.char((d & 15) << 12 | (c2 & 63) << 6 | c3 & 63)
          b = b + 3
        end
      end
    end
    return a
  end
  return utf8_decode(a)
end

function getpagenumber()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(., "var DATA")]')
    s = GetBetween("= '", "',", s)
    s = decode1(s:sub(2))
    x.parsehtml(s)
    x.xpathstringall('json(*).picture().url', task.pagelinks)
    return true
  else
    return false
  end
end

function BeforeDownloadImage()
  http.headers.values['Referer'] = module.rooturl
  return true
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. '/Comic/all/search/hot/page/1') then
    x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('//span[contains(@class,"ret-result-num")]/em'))
    if page == nil then page = 1 end
    page = math.ceil(page / 12)
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  if http.get(module.rooturl..'/Comic/all/search/hot/page/'..IncStr(url)) then
    TXQuery.Create(http.document).XPathHREFAll('//ul[contains(@class, "ret-search-list")]/li//h3/a',links,names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Raw'
  m.website='AcQQCom'
  m.rooturl='http://ac.qq.com'
  m.lastupdated='April 2, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
  m.ongetnameandlink='getnameandlink'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end

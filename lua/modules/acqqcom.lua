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

function getpagenumber()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(., "window") and contains(., "eval")]')
    local nonce = ExecJS('var window={};'..s..';window.nonce;');
    s = x.xpathstring('//script[contains(., "var DATA")]')
    local data = ExecJS(s..';DATA;');
    local script = x.xpathstring('//script[contains(@src, "chapter")]/@src')
    if http.get(script) then
      s = StreamToString(http.document)
      s = '!function(){eval(function(p, a, c, k, e, r)'..GetBetween('eval(function(p, a, c, k, e, r)', '}();', s)..'}();'
      s = 'var W={nonce:"'..nonce..'",DATA:"'..data..'"};'..s..';JSON.stringify(_v);'
      s = ExecJS(s)
      x.parsehtml(s)
      x.xpathstringall('json(*).picture().url', task.pagelinks)
      return true
    else
      return false
    end
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
  m.rooturl='https://ac.qq.com'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
  m.ongetnameandlink='getnameandlink'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end

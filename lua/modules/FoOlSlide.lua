local dirurl = '/directory/'
local dirurlreader = '/reader/directory/'
local dirurlfoolslide = '/foolslide/directory/'
local dirurlslide = '/slide/directory/'
local dirurlslideU = '/Slide/directory/'
local dirurlonline = '/online/directory/'
local dirurlhelvetica = '/r/directory/'
local dirurllector = '/lector/directory/'
local dirurlfsdir = '/fs/directory/'

function getWithCookie(lurl)
  if http.get(lurl) then
    local x = TXQuery.Create(http.document)
    local s = x.xpathstring('//form//input[(@type="hidden") and (@name="adult")]/@value')
    if s:lower() == 'true' then
      http.reset()
      return http.post(lurl, 'adult=true')
    end
    return true
  else
    return false
  end
end

function getdirurl(website)
  local dirs = {
    ['GoManga'] = dirurlreader,
    ['Jaiminisbox'] = dirurlreader,
    ['TripleSevenScan'] = dirurlreader,
    ['DokiFansubs'] = dirurlreader,
    ['AtelierDuNoir'] = dirurlreader,
    ['OneTimeScans'] = dirurlfoolslide,
    ['DejameProbar'] = dirurlslide,
    ['MenudoFansub'] = dirurlslide,
    ['NeoProjectScan'] = dirurlslide,
    ['SolitarioNoFansub'] = dirurlslide,
    ['SantosScan'] = dirurlslideU,
    ['Pzykosis666HFansub'] = dirurlonline,
    ['SeinagiFansub'] = dirurlonline,
    ['HelveticaScans'] = dirurlhelvetica,
    ['RavensScans'] = dirurllector,
    ['NoraNoFansub'] = dirurllector,
    ['HotChocolateScans'] = dirurlfsdir,
    ['AntisenseScans'] = dirurlonline,
    ['MangaichiScan'] = dirurlfsdir
  }  
  if dirs[website] ~= nil then
    return dirs[website]
  else
    return dirurl
  end
end

function getinfo()
  local lurl = MaybeFillHost(module.rooturl, url)
  local result = net_problem
  if getWithCookie(lurl) then
    x = TXQuery.Create(http.document)
    mangainfo.coverlink = x.xpathstring('//div[@class="thumbnail"]/img/@src')
    if mangainfo.title == '' then
      if module.website == 'AtelierDuNoir' then
        mangainfo.title = x.xpathstring('//div[@class="section-headline"]//h3')
      else
        mangainfo.title = x.xpathstring('//h1[@class="title"]')
      end
    end
    if Pos('emailprotected', mangainfo.title) > 0 then
      mangainfo.title = Trim(SeparateLeft(x.xpathstring('//title'), '::'))
    end
    mangainfo.authors = string.gsub(
      x.xpathstring('//div[@class="info"]/*[contains(text(),"Author")]/following-sibling::text()[1]'),
      '^[%s:]*', '')
    mangainfo.artists = string.gsub(
      x.xpathstring('//div[@class="info"]/*[contains(text(),"Artist")]/following-sibling::text()[1]'),
      '^[%s:]*', '')
    mangainfo.summary = string.gsub(
      x.xpathstring('//div[@class="info"]/*[contains(text(),"Synopsis")]/following-sibling::text()[1]'),
      '^[%s:]*', '')
    v = x.xpath('//div[@class="list"]//div[@class="title"]/a')
    for i = 1, v.count do
      v1 = v.get(i)
      mangainfo.chapterlinks.add(v1.getattribute('href'))
      if v1.getattribute('title') ~= '' then
        mangainfo.chapternames.add(v1.getattribute('title'))
      else
        mangainfo.chapternames.add(v1.ToString)
      end
    end
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    result = no_error
  end
  return result
end

function taskstart()
  task.pagelinks.clear()
  task.pagenumber = 0
  return true
end

function getpagenumber()
  local result = false
  if getWithCookie(MaybeFillHost(module.rooturl, url)) then
    x = TXQuery.create(http.document)
    task.pagenumber = x.xpath('//div[@class="topbar_right"]//ul[@class="dropdown"]/li').count
    s = x.xpathstring('//script[contains(.,"var pages")]')
    if s ~= '' then
      s = GetBetween('var pages = ', ';', s)
      if Pos('atob("', s) > 0 then
         s = GetBetween('atob("', '")', s)
         s = DecodeBase64(s)  
      end
      x.parsehtml(s)
      v = x.xpath('json(*)()("url")')
      for i = 1, v.count do
        task.pagelinks.add(v.get(i).ToString)
      end
    end
    result = true
  end
  return result
end

function getimageurl()
  local result = false
  local s = url
  if workid > 0 then
    s = AppendURLDelim(s) .. 'page/' .. (workid + 1)
  end
  if getWithCookie(MaybeFillHost(module.rooturl, s)) then
    x = TXQuery.create(http.document)
    task.pagelinks.set(workid, x.XPathString('//div[@id="page"]//img/@src'))
  end
  return result
end

function getdirectorypagenumber()
  local result = net_problem
  page = 1
  if getWithCookie(module.rooturl .. getdirurl(module.website)) then
    result = no_error
    x = TXQuery.create(http.document)
    v = x.xpath('//*[@class="next"]/a/@href')
    for i = 1, v.count do
      local s = tonumber(string.match(v.get(i).tostring, '/(%d+)/$'))
      if (s ~= nil) and (s > page) then
        page = s
      end
    end
  end
  return result
end

function getnameandlink()
  local result = net_problem
  local s = module.rooturl .. getdirurl(module.website)
  if url ~= '0' then
    s = s .. (tonumber(url) + 1) .. '/'
  end
  if getWithCookie(s) then
    result = no_error
    x = TXQuery.create(http.document)
    if module.website == 'AtelierDuNoir' then
      v = x.xpath('//div[@class="caption"]')
      for i = 1, v.count do
        v1 = v.get(i)
        links.add(x.xpathstring('div/a/@href', v1))
        names.add(x.xpathstring('h4', v1))
      end
    else
      x.XpathHREFAll('//div[@class="list series"]/div/div[@class="title"]/a', links, names)
    end
  end
  return result
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = category
  m.lastupdated = 'february, 6 2018'
  m.ongetinfo = 'getinfo'
  m.OnTaskStart = 'taskstart'
  m.OnGetPageNumber = 'getpagenumber'
  m.OnGetImageURL = 'getimageurl'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
  m.OnGetNameAndLink = 'getnameandlink'
  return m
end

function Init()
  local cat = 'English-Scanlation'
  AddWebsiteModule('PowerManga', 'http://read.powermanga.org', cat)
  AddWebsiteModule('Shoujosense', 'http://reader.shoujosense.com', cat)
  AddWebsiteModule('GoManga', 'http://gomanga.co', 'English')
  AddWebsiteModule('OneTimeScans', 'http://otscans.com', cat)
  AddWebsiteModule('SenseScans', 'http://reader.sensescans.com', cat)
  AddWebsiteModule('Jaiminisbox', 'https://jaiminisbox.com', cat)
  AddWebsiteModule('KireiCake', 'https://reader.kireicake.com', cat)
  AddWebsiteModule('HelveticaScans', 'http://helveticascans.com', cat)
  AddWebsiteModule('WhiteoutScans', 'http://reader.whiteoutscans.com', cat)
  AddWebsiteModule('DokiFansubs', 'https://kobato.hologfx.com', cat)
  AddWebsiteModule('AtelierDuNoir', 'http://atelierdunoir.org', cat)
  AddWebsiteModule('ChampionScans', 'http://reader.championscans.com', cat)
  AddWebsiteModule('WorldThree', 'http://www.slide.world-three.org', cat)
  AddWebsiteModule('S2Scans', 'https://reader.s2smanga.com', cat)
  AddWebsiteModule('HotChocolateScans', 'http://hotchocolatescans.com', cat)
  AddWebsiteModule('LetItGoScans', 'http://reader.letitgo.scans.today', cat)
  AddWebsiteModule('TripleSevenScan', 'http://triplesevenscans.com', cat)
  AddWebsiteModule('SeaOtterScans', 'https://reader.seaotterscans.com', cat)
  AddWebsiteModule('AntisenseScans', 'http://antisensescans.com', cat)
  AddWebsiteModule('TheCatScans', 'https://reader.thecatscans.com', cat)
  AddWebsiteModule('DeathTollScans', 'https://reader.deathtollscans.net', cat)
  AddWebsiteModule('MangaichiScan', 'http://mangaichiscans.mokkori.fr', cat)
  
  -- es-sc
  cat = 'Spanish-Scanlation'
  AddWebsiteModule('DangoOnlineNoFansub', 'http://lector.dangolinenofansub.com', cat)
  AddWebsiteModule('DejameProbar', 'http://dejameprobar.es', cat)
  AddWebsiteModule('HoshinoFansub', 'http://manga.animefrontline.com', cat)
  AddWebsiteModule('MenudoFansub', 'http://www.menudo-fansub.com', cat)
  AddWebsiteModule('NeoProjectScan', 'http://npscan.mangaea.net', cat)
  AddWebsiteModule('Pzykosis666HFansub', 'http://pzykosis666hfansub.com', cat)
  AddWebsiteModule('SantosScan', 'http://santosfansub.com', cat)
  AddWebsiteModule('SeinagiFansub', 'http://seinagi.org', cat)
  AddWebsiteModule('SeinagiAdultoFansub', 'https://adulto.seinagi.org', cat)
  AddWebsiteModule('SolitarioNoFansub', 'http://snf.mangaea.net', cat)
  AddWebsiteModule('RavensScans', 'http://ravens-scans.com', cat)
  AddWebsiteModule('KirishimaFansub', 'http://lector.kirishimafansub.com', cat)
  AddWebsiteModule('NoraNoFansub', 'https://www.noranofansub.com', cat)
  AddWebsiteModule('YamiTenshiNoFansub', 'http://lector.ytnofan.com', cat)
  AddWebsiteModule('XAnimeSeduccion', 'http://xanime-seduccion.com', cat)
  AddWebsiteModule('JokerFansub', 'http://reader.jokerfansub.com', cat)
  AddWebsiteModule('PatyScans', 'http://lector.patyscans.com', cat)
  AddWebsiteModule('IdkScans', 'http://reader.idkscans.com', cat)
end

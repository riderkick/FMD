local dirurl = '/directory/'
local dirurlreader = '/reader/directory/'
local dirurlfoolslide = '/foolslide/directory/'
local dirurlslide = '/slide/directory/'
local dirurlslideU = '/Slide/directory/'
local dirurlonline = '/online/directory/'
local dirurlhelvetica = '/r/directory/'
local dirurllector = '/lector/directory/'
local dirurlfsdir = '/fs/directory/'
local dirurlreaderlist = '/fs/reader/list/'

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
    ['Jaiminisbox'] = dirurlreader,
    ['DokiFansubs'] = dirurlreader,
    ['OneTimeScans'] = dirurl,
    ['DejameProbar'] = dirurlslide,
    ['MenudoFansub'] = dirurlslide,
    ['SolitarioNoFansub'] = dirurlslide,
    ['Pzykosis666HFansub'] = dirurlonline,
    ['SeinagiFansub'] = dirurlonline,
    ['HelveticaScans'] = dirurlhelvetica,
    ['RavensScans'] = dirurllector,
    ['NoraNoFansub'] = dirurllector,
    ['AntisenseScans'] = dirurlonline,
	['SenseScans'] = dirurlreader,
    ['MangaichiScan'] = dirurlfsdir,
    ['Riceballicious'] = dirurlreaderlist,
    ['Yuri-ism'] = dirurlslide,
    ['MangajinNoFansub'] = dirurllector,
    ['BunnysScans'] = '/read/directory/',
    ['CanisMajorScans'] = dirurlreader,
    ['HoshikuzuuScans'] = dirurl,
    ['YaoiIsLife'] = dirurlreader,
    ['FujoshiBitches'] = dirurlreader,
    ['TapTrans'] = dirurlfsdir,
    ['LoliVault'] = dirurlonline,
    ['Shoujohearts'] = dirurlreader
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
    mangainfo.coverlink = x.xpathstring('//div[@class="thumbnail" or contains(@class, "thumb")]/img/@src')
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//h1[@class="title"]')
    end
    if Pos('emailprotected', mangainfo.title) > 0 then
      mangainfo.title = Trim(SeparateLeft(x.xpathstring('//title'), '::'))
    end
    local cls = 'info'
    mangainfo.authors = string.gsub(
      x.xpathstring('//div[@class="'..cls..'"]/*[contains(text(),"Author")]/following-sibling::text()[1]'),
      '^[%s:]*', '')
    mangainfo.artists = string.gsub(
      x.xpathstring('//div[@class="'..cls..'"]/*[contains(text(),"Artist")]/following-sibling::text()[1]'),
      '^[%s:]*', '')
    mangainfo.summary = string.gsub(
      x.xpathstring('//div[@class="'..cls..'"]/*[contains(text(),"Synopsis")]/following-sibling::text()[1]'),
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

function getinfo_ths()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if getWithCookie(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//div[@id="series_right"]/h1')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//img[@class="series_img"]/@src'))
    mangainfo.authors = x.xpathstring('//ul[@class="series_left_data"]/li[contains(span, "Author")]/span[@class="value"]')
    mangainfo.artists = x.xpathstring('//ul[@class="series_left_data"]/li[contains(span, "Artist")]/span[@class="value"]')
    mangainfo.genres=x.xpathstringall('//ul[@class="series_left_data"]/li[contains(span, "Genre")]/span[@class="value"]/text()')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//ul[@class="series_left_data"]/li[contains(span, "Status")]/span[@class="value"]'))
    mangainfo.summary = x.xpathstring('//div[@id="series_des"]')
    x.xpathhreftitleall('//div[@id="staff"]/div/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
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

function getpagenumber_jb()
  local result = false
  if getWithCookie(MaybeFillHost(module.rooturl, url)) then
    x = TXQuery.create(http.document)
    task.pagenumber = x.xpath('//div[@class="topbar_right"]//ul[@class="dropdown"]/li').count
    s = x.xpathstring('//script[contains(.,"[\'fromCharCode\',\'")]')
    if s ~= '' then
      s = GetBetween('[\'fromCharCode\',\'', '\'];', s)
      s = ExecJS("function decrypt(encrypted) {return encrypted.replace(/[a-zA-Z]/g,function (a) {return String.fromCharCode((a <= 'Z' ? 90 : 122) >= (a = a.charCodeAt(0) + 13) ? a : a - 26);});}; decrypt('" .. s .. "');")
      s = DecodeBase64(s)
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
    local x = TXQuery.create(http.document)
    if module.website == 'TwistedHelScans' then
      local v = x.xpath('//div[contains(@class, "series_card")]/a')
      for i = 1, v.count do
        local v1 = v.get(i)
        links.add(v1.getattribute('href'))
        names.add(x.xpathstring('span', v1))
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
  m.ongetinfo = 'getinfo'
  m.OnTaskStart = 'taskstart'
  m.OnGetPageNumber = 'getpagenumber'
  m.OnGetImageURL = 'getimageurl'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
  m.OnGetNameAndLink = 'getnameandlink'
  if name == 'TwistedHelScans' then m.ongetinfo = 'getinfo_ths'; end
  if name == 'Jaiminisbox' then m.OnGetPageNumber = 'getpagenumber_jb'; end
  return m
end

function Init()
  local cat = 'English-Scanlation'
  AddWebsiteModule('AntisenseScans', 'http://antisensescans.com', cat)
  AddWebsiteModule('BunnysScans', 'http://bns.shounen-ai.net', cat)
  AddWebsiteModule('CanisMajorScans', 'http://cm-scans.shounen-ai.net', cat)
  AddWebsiteModule('DeathTollScans', 'https://reader.deathtollscans.net', cat)
  AddWebsiteModule('DokiFansubs', 'https://kobato.hologfx.com', cat)
  AddWebsiteModule('EvilFlowers', 'http://reader.evilflowers.com', cat)
  AddWebsiteModule('ForgottenScans', 'http://reader.fos-scans.com', cat)
  AddWebsiteModule('FujoshiBitches', 'http://fujoshibitches.shounen-ai.net', cat)
  AddWebsiteModule('HelveticaScans', 'http://helveticascans.com', cat)
  AddWebsiteModule('HoshikuzuuScans', 'http://hoshiscans.shounen-ai.net', cat)
  AddWebsiteModule('IlluminatiManga', 'http://reader.manga-download.org', cat)
  AddWebsiteModule('Jaiminisbox', 'https://jaiminisbox.com', cat)
  AddWebsiteModule('KireiCake', 'https://reader.kireicake.com', cat)
  AddWebsiteModule('MangaichiScan', 'http://mangaichiscans.mokkori.fr', cat)
  AddWebsiteModule('OneTimeScans', 'https://reader.otscans.com', cat)
  AddWebsiteModule('PhoenixSerenade', 'https://reader.serenade.moe', cat)
  AddWebsiteModule('PowerManga', 'http://read.powermanga.org', cat)
  AddWebsiteModule('Riceballicious', 'http://riceballicious.info', cat)
  AddWebsiteModule('RoseliaScanlations', 'http://reader.roseliascans.com', cat)
  AddWebsiteModule('S2Scans', 'https://reader.s2smanga.com', cat)
  AddWebsiteModule('SeaOtterScans', 'https://reader.seaotterscans.com', cat)
  AddWebsiteModule('SenseScans', 'http://sensescans.com', cat)
  AddWebsiteModule('Shoujohearts', 'http://shoujohearts.com', cat)
  AddWebsiteModule('Shoujosense', 'http://reader.shoujosense.com', cat)
  AddWebsiteModule('SilentSkyScans', 'http://reader.silentsky-scans.net', cat)
  AddWebsiteModule('TapTrans', 'https://taptaptaptaptap.net', cat)
  AddWebsiteModule('TheCatScans', 'https://reader2.thecatscans.com', cat)
  AddWebsiteModule('TwistedHelScans', 'http://www.twistedhelscans.com', cat)
  AddWebsiteModule('VortexScans', 'https://reader.vortex-scans.com', cat)
  AddWebsiteModule('WorldThree', 'http://www.slide.world-three.org', cat)
  AddWebsiteModule('YaoiIsLife', 'http://yaoislife.shounen-ai.net', cat)
  AddWebsiteModule('Yuri-ism', 'https://www.yuri-ism.net', cat)
  
  -- es-sc
  cat = 'Spanish-Scanlation'
  AddWebsiteModule('KirishimaFansub', 'https://kirishimafansub.net', cat)
  AddWebsiteModule('LoliVault', 'https://lolivault.net', cat)
  AddWebsiteModule('Mangasubes', 'http://mangasubes.patyscans.com', cat)
  AddWebsiteModule('MenudoFansub', 'http://www.menudo-fansub.com', cat)
  AddWebsiteModule('NeoProjectScan', 'http://npscan.mangaea.net', cat)
  AddWebsiteModule('Nightow', 'http://nightow.net', cat)
  AddWebsiteModule('NoraNoFansub', 'https://www.noranofansub.com', cat)
  AddWebsiteModule('PCNet', 'http://pcnet.patyscans.com', cat)
  AddWebsiteModule('PatyScans', 'http://lector.patyscans.com', cat)
  AddWebsiteModule('Pzykosis666HFansub', 'https://pzykosis666hfansub.com', cat)
  AddWebsiteModule('RavensScans', 'http://ravens-scans.com', cat)
  AddWebsiteModule('SeinagiAdultoFansub', 'https://adulto.seinagi.org.es', cat)
  AddWebsiteModule('SeinagiFansub', 'https://seinagi.org.es', cat)
  AddWebsiteModule('SolitarioNoFansub', 'http://snf.mangaea.net', cat)
  AddWebsiteModule('TrueColorsScan', 'https://truecolorsscans.miocio.org', cat)
  AddWebsiteModule('XAnimeSeduccion', 'http://xanime-seduccion.com', cat)
end

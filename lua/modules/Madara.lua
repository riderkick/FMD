Modules = {}

function Modules.Madara()
  local Madara = {}
  
  function Madara:new()
    local obj = {}
    setmetatable(obj, self)
    self.__index = self
    return obj
  end
  
  function Madara:getinfo()
    mangainfo.url=MaybeFillHost(module.RootURL, url)
    if http.get(mangainfo.url) then
      local x=TXQuery.Create(http.document)
      
      if module.website == 'NinjaScans' then
        local fixedHtml = StreamToString(http.document):gsub('a href=(.-/)>', 'a href="%1">')
        x.ParseHTML(fixedHtml)
      end
      
      mangainfo.title=x.xpathstringall('//div[@class="post-title"]/*[self::h1 or self::h2 or self::h3]/text()', '')
      if string.match(mangainfo.title:upper(), ' RAW$') ~= nil then
        mangainfo.title = mangainfo.title:sub(1, -5)
      end
      if module.website == 'ArtemisNF' then
        mangainfo.title=x.xpathstringall('//div[@class="post-title post-sigle-title"]/*[self::h1 or self::h2 or self::h3]/text()', '')
      elseif module.website == 'GetManhwa' then
	    mangainfo.title=x.xpathstringall('//div[@class="post-title-dpage"]/h3')
      elseif module.website == 'WordExcerpt' then
	    mangainfo.title=x.xpathstringall('//div[@class="c-manga-title"]/h1')
      end
      mangainfo.coverlink=x.xpathstring('//div[@class="summary_image"]//img/@data-src')
      if mangainfo.coverlink == '' then
        mangainfo.coverlink=x.xpathstring('//div[@class="summary_image"]//img/@src')
      end
      if module.website == 'GetManhwa' then
        mangainfo.coverlink=x.xpathstring('//div[@class="my_profile-manga"]/@style'):match('background%-image:url%((.-)%)')
      elseif module.website == 'WordExcerpt' then
        mangainfo.coverlink=x.xpathstring('//div[@class="c-manga-thumbnail-bg"]/@style'):match('background%-image: url%((.-)%)')
      end
      mangainfo.authors=x.xpathstringall('//div[@class="author-content"]/a')
      if mangainfo.authors == '' then
        mangainfo.authors=x.xpathstringall('//div[@class="summary-heading-creator"]/a')
      end
      if module.website == 'WordExcerpt' then
        mangainfo.authors=x.xpathstring('//div[@class="post-content_item" and contains(., "Author")]//a')
      end
      mangainfo.artists=x.xpathstringall('//div[@class="artist-content"]/a')
      if module.website == 'WordExcerpt' then
        mangainfo.genres=x.xpathstringall('//div[@class="post-content_item tags"]/a')
      else
        mangainfo.genres=x.xpathstringall('//div[@class="genres-content"]/a')
      end
      if module.website == 'ATMSubs' then
        mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="summary-heading" and contains(h5, "Statut")]/following-sibling::div'), 'En Cours', 'Complete')
      else
        mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="summary-heading" and contains(h5, "Status")]/following-sibling::div'))
      end
      if module.website == 'Mangareceh' then
        mangainfo.summary=x.xpathstring('//div[contains(@class,"description-summary")]//p[2]')
      elseif module.website == 'WordExcerpt' then
        mangainfo.summary=x.xpathstring('//div[@class="content"]/p')
      else
        mangainfo.summary=x.xpathstring('//div[contains(@class,"description-summary")]//p')
      end
      
      if module.website == 'DoujinYosh' or module.website == 'MangaYosh' or module.website == 'KIDzScan' then
        local v = x.xpath('//li[contains(@class, "wp-manga-chapter")]/a')
        for i = 1, v.count do
          local v1 = v.get(i)
          local link = v1.getAttribute('href')
          if module.website == 'MangaYosh' then
            link = string.gsub(link, 'https://yosh.tranivson.me', module.rooturl)
          else
            link = string.gsub(link, 'https://doujinyosh.bloghadi.me', module.rooturl)
          end
          mangainfo.chapternames.Add(v1.toString);
          mangainfo.chapterlinks.Add(link);
        end
      elseif module.website == 'Mangareceh' then
        x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[1]', mangainfo.chapterlinks, mangainfo.chapternames)
      else
        x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a', mangainfo.chapterlinks, mangainfo.chapternames)
      end
	  
      local idmanga = x.xpathstring('//div[@id="manga-chapters-holder"]/@data-id')
      if mangainfo.chapterlinks.count < 1 then
        http.reset()
        http.headers.values['Cache-Control'] = 'no-cache'
        http.headers.values['Content-Type'] = 'application/x-www-form-urlencoded; charset=UTF-8'
        http.headers.Add('X-Requested-With: XMLHttpRequest')
        local q = 'action=manga_get_chapters&manga=' .. idmanga
        if http.post(module.rooturl .. '/wp-admin/admin-ajax.php', q) then
            local x = TXQuery.Create(http.Document)
            x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a', mangainfo.chapterlinks, mangainfo.chapternames)
        end
      end
      
      InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
      return no_error
    end
    return net_problem
  end
  
  function Madara:getpagenumber()
    task.pagelinks.clear()
    local aurl = MaybeFillHost(module.rooturl, url)
    if Pos('style=list', aurl) == 0 then
      aurl = aurl .. '?style=list'
    end
    if http.get(aurl) then
      local x = TXQuery.Create(http.Document)
      if module.website == 'ManhwaHentai' then
        v = x.xpath('//div[contains(@class, "page-break")]/img')
        for i = 1, v.count do
          v1 = v.get(i)
          local src = v1.getattribute('src')
          src = src:gsub('https://cdn.shortpixel.ai/client/q_glossy,ret_img/', '')
          task.pagelinks.add(src)
        end
      else
        x.xpathstringall('//div[contains(@class, "page-break")]/img/@data-src', task.pagelinks)
      end
      if task.pagelinks.count == 0 then
        x.xpathstringall('//div[@class="entry-content"]//picture/img/@src', task.pagelinks)
      end
      if task.pagelinks.count < 1 then
        x.xpathstringall('//div[contains(@class, "page-break")]/img/@src', task.pagelinks)
  	  end
      if task.pagelinks.count < 1 then
        x.xpathstringall('//*[@class="wp-manga-chapter-img webpexpress-processed"]/@src', task.pagelinks)
      end
      if task.pagelinks.count < 1 then
        x.xpathstringall('//div[@class="reading-content"]//img/@src', task.pagelinks)
      end
      return true
    end
    return false
  end
  
  function Madara:getnameandlink()
    local perpage = 100
    local q = 'action=madara_load_more&page='.. url ..'&template=madara-core%2Fcontent%2Fcontent-archive&vars%5Bpost_type%5D=wp-manga&vars%5Berror%5D=&vars%5Bm%5D=&vars%5Bp%5D=0&vars%5Bpost_parent%5D=&vars%5Bsubpost%5D=&vars%5Bsubpost_id%5D=&vars%5Battachment%5D=&vars%5Battachment_id%5D=0&vars%5Bname%5D=&vars%5Bstatic%5D=&vars%5Bpagename%5D=&vars%5Bpage_id%5D=0&vars%5Bsecond%5D=&vars%5Bminute%5D=&vars%5Bhour%5D=&vars%5Bday%5D=0&vars%5Bmonthnum%5D=0&vars%5Byear%5D=0&vars%5Bw%5D=0&vars%5Bcategory_name%5D=&vars%5Btag%5D=&vars%5Bcat%5D=&vars%5Btag_id%5D=&vars%5Bauthor%5D=&vars%5Bauthor_name%5D=&vars%5Bfeed%5D=&vars%5Btb%5D=&vars%5Bpaged%5D=1&vars%5Bmeta_key%5D=&vars%5Bmeta_value%5D=&vars%5Bpreview%5D=&vars%5Bs%5D=&vars%5Bsentence%5D=&vars%5Btitle%5D=&vars%5Bfields%5D=&vars%5Bmenu_order%5D=&vars%5Bembed%5D=&vars%5Bignore_sticky_posts%5D=false&vars%5Bsuppress_filters%5D=false&vars%5Bcache_results%5D=true&vars%5Bupdate_post_term_cache%5D=true&vars%5Blazy_load_term_meta%5D=true&vars%5Bupdate_post_meta_cache%5D=true&vars%5Bposts_per_page%5D='.. tostring(perpage) ..'&vars%5Bnopaging%5D=false&vars%5Bcomments_per_page%5D=50&vars%5Bno_found_rows%5D=false&vars%5Border%5D=ASC&vars%5Borderby%5D=post_title&vars%5Btemplate%5D=archive&vars%5Bsidebar%5D=full&vars%5Bpost_status%5D=publish'
    if http.post(module.rooturl .. '/wp-admin/admin-ajax.php', q) then
      if http.headers.values['Content-Length'] == '0' then return no_error end
      local x = TXQuery.Create(http.Document)
      if x.xpath('//div[contains(@class, "post-title")]/*[self::h5 or self::h3]/a').count == 0 then return no_error end
      x.XPathHREFAll('//div[contains(@class, "post-title")]/*[self::h5 or self::h3]/a', links, names)
      updatelist.CurrentDirectoryPageNumber = updatelist.CurrentDirectoryPageNumber + 1
      return no_error
    else
      return net_problem
    end
  end
  
  return Madara
end

function Modules.ChibiManga()
  local ChibiManga = {}
  setmetatable(ChibiManga, { __index = Modules.Madara() })
  
  function ChibiManga:getpagenumber()
    task.pagelinks.clear()
    if http.get(MaybeFillHost(module.rooturl, url)) then
      local x = TXQuery.Create(http.Document)
      local s = x.xpathstring('//script[contains(., "chapter_preloaded_images")]', task.pagelinks)
      s = "["..GetBetween("[", "]", s).."]"
      x.parsehtml(s)
      x.xpathstringall('json(*)()', task.pagelinks)
      return true
    end
    return false
  end
  
  return ChibiManga
end

function Modules.HentaiRead()
  local HentaiRead = {}
  setmetatable(HentaiRead, { __index = Modules.Madara() })
  
  function HentaiRead:getpagenumber()
    task.pagelinks.clear()
    if http.get(MaybeFillHost(module.rooturl, url)) then
      local x = TXQuery.Create(http.Document)
      local s = x.xpathstring('//script[contains(., "chapter_preloaded_images")]', task.pagelinks)
      s = "["..GetBetween("[", "]", s).."]"
      x.parsehtml(s)
      x.xpathstringall('json(*)()', task.pagelinks)
      return true
    end
    return false
  end
  
  return HentaiRead
end

function Modules.OnManga()
  local OnManga = {}
  setmetatable(OnManga, { __index = Modules.Madara() })
  
  function OnManga:getpagenumber()
    task.pagelinks.clear()
    if http.get(MaybeFillHost(module.rooturl, url)) then
      local x = TXQuery.Create(http.Document)
      local s = x.xpathstring('//script[contains(., "chapter_preloaded_images")]', task.pagelinks)
      s = "{"..GetBetween("{", "}", s).."}"
      x.parsehtml(s)
      x.xpathstringall('let $c := json(*) return for $k in jn:keys($c) return $c($k)', task.pagelinks)
      return true
    end
    return false
  end
  
  return OnManga
end
-------------------------------------------------------------------------------

function createInstance()
  local m = Modules[module.website]
  if m ~= nil then
    return m():new()
  else
    return Modules.Madara():new()
  end
end

------------------------------------------------------------------------------- 

function getinfo()
  return createInstance():getinfo()
end

function getpagenumber()
  return createInstance():getpagenumber()
end

function getnameandlink()
  return createInstance():getnameandlink()
end

function BeforeDownloadImage()
  http.headers.values['referer'] = module.rooturl
  return true
end

-------------------------------------------------------------------------------

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = category
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
  return m
end

function Init()
  local cat = 'Raw'
  AddWebsiteModule('MangazukiClub', 'https://mangazuki.club', cat)
  
  cat = 'English'
  AddWebsiteModule('IsekaiScan', 'https://isekaiscan.com', cat)
  AddWebsiteModule('MangaKomi', 'https://mangakomi.com', cat)
  AddWebsiteModule('MangaZukiWhatStatus', 'https://whatstatus.co', cat)
  AddWebsiteModule('MangaZukiOnline', 'https://www.mangazuki.online', cat)
  AddWebsiteModule('MangaZukiSite', 'https://www.mangazuki.site', cat)
  AddWebsiteModule('MangaZukiMe', 'https://mangazuki.me', cat)
  AddWebsiteModule('YoManga', 'https://yomanga.info', cat)
  AddWebsiteModule('OnManga', 'https://onmanga.com', cat)
  AddWebsiteModule('ReadRawManga', 'https://www.readrawmanga.com', cat)
  AddWebsiteModule('MangaStreamCC', 'https://www.mangastream.cc', cat)
  AddWebsiteModule('TeabeerComics', 'https://teabeercomics.com', cat)
  AddWebsiteModule('MangaSushi', 'https://mangasushi.net', cat)
  AddWebsiteModule('Manhuaus', 'https://manhuaus.com', cat)
  AddWebsiteModule('AstralLibrary', 'https://astrallibrary.net', cat)

  cat = 'English-Scanlation'
  AddWebsiteModule('TrashScanlations', 'https://trashscanlations.com', cat)
  AddWebsiteModule('ChibiManga', 'http://www.cmreader.info', cat)
  AddWebsiteModule('ZinManga', 'https://zinmanga.com', cat)
  AddWebsiteModule('SiXiangScans','http://www.sixiangscans.com', cat)
  AddWebsiteModule('NinjaScans', 'https://ninjascans.com', cat)
  AddWebsiteModule('ReadManhua', 'https://readmanhua.net', cat)
  AddWebsiteModule('MangaDods', 'https://www.mangadods.com', cat)
  AddWebsiteModule('DisasterScans', 'https://disasterscans.com', cat)
  AddWebsiteModule('RaiderScans', 'https://raiderscans.com', cat)
  AddWebsiteModule('WordExcerpt', 'https://wordexcerpt.com', cat)
  AddWebsiteModule('MartialScans', 'https://martialscans.com', cat)
  AddWebsiteModule('AsuraScans', 'https://asurascans.com', cat)
  AddWebsiteModule('HeroManhua', 'https://heromanhua.com', cat)
  AddWebsiteModule('MangaTurf', 'https://mangaturf.com', cat)
  
  cat = 'French'
  AddWebsiteModule('WakaScan', 'https://wakascan.com', cat)
  AddWebsiteModule('ATMSubs', 'https://atm-subs.fr', cat)
  AddWebsiteModule('NovelFrance', 'http://novel-france.fr', cat)
  
  cat = 'Indonesian'
  AddWebsiteModule('MangaYosh', 'https://mangayosh.xyz', cat)
  AddWebsiteModule('KomikGo', 'https://komikgo.com', cat)
  AddWebsiteModule('KlikManga', 'https://klikmanga.com', cat)
  AddWebsiteModule('PojokManga', 'https://pojokmanga.com', cat)
  AddWebsiteModule('Mangareceh', 'https://mangareceh.id', cat)
  
  cat = 'H-Sites'
  AddWebsiteModule('ManhwaHand', 'https://manhwahand.com', cat)
  AddWebsiteModule('DoujinYosh', 'https://doujinyosh.fun', cat)
  AddWebsiteModule('ManhwaHentai', 'https://manhwahentai.me', cat)
  AddWebsiteModule('HentaiRead', 'http://hentairead.com', cat)
  AddWebsiteModule('ManhwaClub', 'https://manhwa.club', cat)
  AddWebsiteModule('NManhwa', 'https://nmanhwa.com', cat)
  AddWebsiteModule('ManyToonCom', 'https://manytoon.com', cat)
  AddWebsiteModule('Hiperdex', 'https://hiperdex.com', cat)
  AddWebsiteModule('ShosetsuManga', 'https://www.shosetsu-manga.org', cat)

  cat = 'Spanish-Scanlation'
  AddWebsiteModule('GodsRealmScan', 'https://godsrealmscan.com', cat)
  AddWebsiteModule('DarkskyProjects', 'https://darkskyprojects.org', cat)
  AddWebsiteModule('PlotTwistNoFansub', 'https://www.plotwistscan.com', cat)
  AddWebsiteModule('KnightNoFansub', 'https://knightnoscanlation.com', cat)
  AddWebsiteModule('CopyPasteScanlation', 'https://copypastescan.xyz', cat)
  AddWebsiteModule('ZManga', 'https://zmanga.org', cat)
  AddWebsiteModule('KIDzScan', 'https://grafimanga.com', cat)
  AddWebsiteModule('HunterFansub', 'https://hunterfansub.com', cat)
  AddWebsiteModule('SDLGFansub', 'https://www.sdlg-fansub.tk', cat)
  AddWebsiteModule('ArtemisNF', 'https://artemisnf.com', cat)
  AddWebsiteModule('LazyBoysScan', 'https://lazyboysscan.com', cat)
  AddWebsiteModule('BDSFansub', 'https://bdsfansub.com', cat)
  AddWebsiteModule('LeerManhua', 'https://leermanhua.com', cat)
  AddWebsiteModule('BakaguyaScan', 'https://bakaguya-scan.tk', cat)
	
  cat = 'Webcomics'
  AddWebsiteModule('ManhwaPool', 'https://manhwapool.com', cat)
  AddWebsiteModule('ManyToon', 'https://manytoon.me', cat)
  AddWebsiteModule('PocketAngelScan', 'https://pocketangelscans.com', cat)
  AddWebsiteModule('Toonily', 'https://toonily.com', cat)
  AddWebsiteModule('ManhuaBox', 'https://manhuabox.net', cat)
  AddWebsiteModule('TopManhua', 'https://topmanhua.com', cat)
  AddWebsiteModule('Wakamics', 'https://wakamics.com', cat)
  AddWebsiteModule('GetManhwa', 'https://getmanhwa.co', cat)
  AddWebsiteModule('Manhuas', 'https://manhuas.net', cat)
  AddWebsiteModule('MixedManga', 'https://mixedmanga.com', cat)
  AddWebsiteModule('MangaTX', 'https://mangatx.com', cat)
  AddWebsiteModule('NightComic', 'https://nightcomic.com', cat)
  AddWebsiteModule('MangaDao', 'https://mangadao.com', cat)
  AddWebsiteModule('365Manga', 'https://365manga.com', cat)
  AddWebsiteModule('MangaBob', 'https://mangabob.com', cat)
  AddWebsiteModule('Manga68', 'https://manga68.com', cat)
  AddWebsiteModule('EarlyManga', 'https://earlymanga.net', cat)
  AddWebsiteModule('Mangakiss', 'https://mangakiss.org', cat)
  AddWebsiteModule('MangaLord', 'https://www.mangalord.com', cat)
  AddWebsiteModule('KissMangaIN', 'https://kissmanga.in', cat)
  AddWebsiteModule('MiracleScans', 'https://miraclescans.com', cat)
  AddWebsiteModule('1stKissManga', 'https://1stkissmanga.com', cat)
  AddWebsiteModule('WuxiaWorld', 'https://wuxiaworld.site', cat)
  AddWebsiteModule('WebNovelLive', 'https://webnovel.live', cat)
  AddWebsiteModule('MangaRockTeam', 'https://mangarockteam.com', cat)
  AddWebsiteModule('PhenomenalNoFansub', 'https://phenomenalnofansub.com', cat)
  AddWebsiteModule('TraduccionesAmistosas', 'https://scantraduccionesamistosas.xyz', cat)
  AddWebsiteModule('WebtoonXYZ', 'https://www.webtoon.xyz', cat)
  AddWebsiteModule('AHStudios', 'https://ahstudios.net', cat)
  AddWebsiteModule('Alianzamarcial', 'https://www.alianzamarcial.xyz', cat)
  AddWebsiteModule('Skymangas', 'https://skymangas.com', cat) 
  AddWebsiteModule('Fenixscan', 'https://fenixscan.com', cat) 
  AddWebsiteModule('Apolltoons', 'https://apolltoons.xyz', cat)
  AddWebsiteModule('Apollcomics', 'https://apollcomics.xyz', cat) 
  AddWebsiteModule('ManhuaPlus', 'https://manhuaplus.com', cat)
  
  cat = 'Arabic-Scanlation'
  AddWebsiteModule('3asqOrg', 'https://3asq.org', cat)
  AddWebsiteModule('AzoraManga', 'https://www.azoramanga.com', cat)
  
  cat = 'Turkish'
  AddWebsiteModule('AdonisFansub', 'https://manga.adonisfansub.com', cat)
  
  cat = 'Arabic'
  AddWebsiteModule('Mangalek', 'https://mangalek.com', cat)
  AddWebsiteModule('MangaAction', 'https://manga-action.com', cat)
  AddWebsiteModule('NijiTranslations', 'https://niji-translations.com', cat)
  AddWebsiteModule('MangaArabTeam', 'https://mangaarabteam.com', cat)
  
  cat = 'Portuguese'
  AddWebsiteModule('IchirinNoHanaYuri', 'https://ichirinnohanayuri.com.br', cat)
  AddWebsiteModule('YaoiToshokan', 'https://www.yaoitoshokan.com.br', cat)
  
  cat = 'Russian'
  AddWebsiteModule('BestManga', 'https://bestmanga.club', cat)
  
  cat = 'Adult'
  AddWebsiteModule('Milftoon', 'https://milftoon.xxx', cat)
  
  cat = 'Portuguese-Scanlation'
  AddWebsiteModule('NeoxScanlator', 'https://neoxscan.com/newsite', cat)
end

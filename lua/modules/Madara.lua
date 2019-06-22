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
      mangainfo.title=x.xpathstringall('//div[@class="post-title"]/*[self::h1 or self::h2 or self::h3]/text()', '')
      if string.match(mangainfo.title:upper(), ' RAW$') ~= nil then
        mangainfo.title = mangainfo.title:sub(1, -5)
      end
      mangainfo.coverlink=x.xpathstring('//div[@class="summary_image"]/a/img/@data-src')
      if mangainfo.coverlink == '' then
        mangainfo.coverlink=x.xpathstring('//div[@class="summary_image"]/a/img/@src')
      end
      mangainfo.authors=x.xpathstringall('//div[@class="author-content"]/a')
      mangainfo.artists=x.xpathstringall('//div[@class="artist-content"]/a')
      mangainfo.genres=x.xpathstringall('//div[@class="genres-content"]/a')
      mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="summary-heading" and contains(h5, "Status")]/following-sibling::div/div/a'))
      mangainfo.summary=x.xpathstring('//div[contains(@class,"summary__content")]/*')
      x.XPathHREFAll('//li[@class="wp-manga-chapter"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
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
      x.xpathstringall('//div[contains(@class, "page-break")]/img/@src', task.pagelinks)
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
      if x.xpath('//div[contains(@class, "post-title")]/h5/a').count == 0 then return no_error end
      x.XPathHREFAll('//div[contains(@class, "post-title")]/h5/a', links, names)
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
      s = "{"..GetBetween("{", "}", s).."}"
      x.parsehtml(s)
      x.xpathstringall('let $c := json(*) return for $k in jn:keys($c) return $c($k)', task.pagelinks)
      return true
    end
    return false
  end
  
  return ChibiManga
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

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = category
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  return m
end

function Init()
  local cat = 'Raw'
  AddWebsiteModule('RawNeko', 'http://trueneko.online', cat)
  
  cat = 'English-Scanlation'
  AddWebsiteModule('TrashScanlations', 'https://trashscanlations.com', cat)
  AddWebsiteModule('ZeroScans', 'https://zeroscans.com', cat)
  AddWebsiteModule('ChibiManga','http://www.cmreader.info', cat)
  
  cat = 'Indonesian'
  AddWebsiteModule('MangaYosh', 'https://mangayosh.com', cat)
  AddWebsiteModule('KomikGo', 'https://komikgo.com', cat)
  
  cat = 'H-Sites'
  AddWebsiteModule('ManhwaHentai', 'https://manhwahentai.com', cat)
  
  cat = 'Spanish-Scanlation'
  AddWebsiteModule('GodsRealmScan', 'https://godsrealmscan.com', cat)
end

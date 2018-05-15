function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstringall('//div[@class="post-title"]/h3/text()', '')
    mangainfo.coverlink=x.xpathstring('//div[@class="summary_image"]/a/img/@src')
    mangainfo.authors=x.xpathstringall('//div[@class="author-content"]/a')
    mangainfo.artists=x.xpathstringall('//div[@class="artist-content"]/a')
    mangainfo.genres=x.xpathstringall('//div[@class="genres-content"]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="summary-heading" and contains(h5, "Status")]/following-sibling::div/div/a'))
    mangainfo.summary=x.xpathstring('//div[@class="summary__content"]/p')
    x.XPathHREFAll('//li[@class="wp-manga-chapter"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  local aurl = MaybeFillHost(module.rooturl, url)
  if Pos('style=list', aurl) == 0 then
    aurl = aurl .. '?style=list'
  end
  if http.get(aurl) then
    x=TXQuery.Create(http.Document)
    v=x.xpathstringall('//div[@class="page-break"]/img/@src', task.pagelinks)
  else
    return false
  end
  return true
end

local perpage = 100

function getnameandlink()
  local q = 'action=madara_load_more&page='.. url ..'&template=madara-core%2Fcontent%2Fcontent-archive&vars%5Bpost_type%5D=wp-manga&vars%5Berror%5D=&vars%5Bm%5D=&vars%5Bp%5D=0&vars%5Bpost_parent%5D=&vars%5Bsubpost%5D=&vars%5Bsubpost_id%5D=&vars%5Battachment%5D=&vars%5Battachment_id%5D=0&vars%5Bname%5D=&vars%5Bstatic%5D=&vars%5Bpagename%5D=&vars%5Bpage_id%5D=0&vars%5Bsecond%5D=&vars%5Bminute%5D=&vars%5Bhour%5D=&vars%5Bday%5D=0&vars%5Bmonthnum%5D=0&vars%5Byear%5D=0&vars%5Bw%5D=0&vars%5Bcategory_name%5D=&vars%5Btag%5D=&vars%5Bcat%5D=&vars%5Btag_id%5D=&vars%5Bauthor%5D=&vars%5Bauthor_name%5D=&vars%5Bfeed%5D=&vars%5Btb%5D=&vars%5Bpaged%5D=1&vars%5Bmeta_key%5D=&vars%5Bmeta_value%5D=&vars%5Bpreview%5D=&vars%5Bs%5D=&vars%5Bsentence%5D=&vars%5Btitle%5D=&vars%5Bfields%5D=&vars%5Bmenu_order%5D=&vars%5Bembed%5D=&vars%5Bignore_sticky_posts%5D=false&vars%5Bsuppress_filters%5D=false&vars%5Bcache_results%5D=true&vars%5Bupdate_post_term_cache%5D=true&vars%5Blazy_load_term_meta%5D=true&vars%5Bupdate_post_meta_cache%5D=true&vars%5Bposts_per_page%5D='.. tostring(perpage) ..'&vars%5Bnopaging%5D=false&vars%5Bcomments_per_page%5D=50&vars%5Bno_found_rows%5D=false&vars%5Border%5D=ASC&vars%5Borderby%5D=post_title&vars%5Btemplate%5D=archive&vars%5Bsidebar%5D=full&vars%5Bpost_status%5D=publish'
  if http.post(module.rooturl .. '/wp-admin/admin-ajax.php', q) then
    if http.headers.values['Content-Length'] == '0' then return no_error end
    x = TXQuery.Create(http.Document)
    if x.xpath('//div[contains(@class, "post-title")]/h5/a').count == 0 then return no_error end
    x.XPathHREFAll('//div[contains(@class, "post-title")]/h5/a', links, names)
    updatelist.CurrentDirectoryPageNumber = updatelist.CurrentDirectoryPageNumber + 1
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = category
  m.lastupdated = 'March 1, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  return m
end

function Init()
  local cat = 'Raw'
  AddWebsiteModule('Rawdevart', 'https://rawdevart.com', cat)
  
  cat = 'English-Scanlation'
  AddWebsiteModule('TrashScanlations', 'https://trashscanlations.com', cat)
  AddWebsiteModule('ZeroScans', 'https://zeroscans.com', cat)
end

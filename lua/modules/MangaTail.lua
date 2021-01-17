function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  http.cookies.values['has_js'] = '1'
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = string.gsub(x.xpathstring('//h1[@class="page-header"]'), ' Manga$', '')
    end
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//*[contains(@class,"field-status")]'))
    x.xpathhrefall('//table[contains(@class,"chlist")]//tr/td[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)    
    
    local s = x.xpathstring('//script[contains(., "jQuery.extend(Drupal")]')
    s = GetBetween('settings,', ');', s)
    x.parsehtml(s)    
    local v = x.xpath('json(*)/authcacheP13nAjaxAssemblies')
    local summaryQuery = x.xpathstring('./span.authcache-p13n-asm-field-node-body', v)
    local artistQuery = x.xpathstring('./span.authcache-p13n-asm-field-node-field-artist', v)
    local authorQuery = x.xpathstring('./span.authcache-p13n-asm-field-node-field-author', v)
    local genresQuery = x.xpathstring('./span.authcache-p13n-asm-field-node-field-genres', v)
    local coverQuery = x.xpathstring('./span.authcache-p13n-asm-field-node-field-image2', v)
    local statusQuery = x.xpathstring('./span.authcache-p13n-asm-field-node-field-status', v)
    
    function getField(aurl, query)
      if aurl ~= '' then
        http.reset()
        http.headers.values['X-Authcache'] = '1'
        if http.xhr(module.rooturl .. aurl) then
          local s = GetBetween(':"', '"}', StreamToString(http.document))
          x.parsehtml(s:gsub('\\"', '"'):gsub('\\/', '/'))
          return x.xpathstring(query)
        end
      end
      return ''
    end
    
    mangainfo.summary = getField(summaryQuery, '*')
    mangainfo.authors = getField(authorQuery, '//div[contains(@class, "field-item")]')
    mangainfo.artists = getField(artistQuery, '//div[contains(@class, "field-item")]')
    mangainfo.coverlink = getField(coverQuery, '//img/@src')
    mangainfo.genres = getField(genresQuery, 'string-join(//a, ", ")')

    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url .. '?page=all')) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//*[@id="images"]//img[not(contains(@src,"adsense"))]/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  local s = module.rooturl .. '/directory'
  if url ~= '0' then s = s .. '?page=' .. url; end
  if http.get(s) then
    local x = TXQuery.Create(http.Document)
    local i = 1
    local v = x.xpath('//ul[@class="pagination"]/li')
    for j = 1, v.count do
      local v1 = v.get(j)
      local x = tonumber(v1.toString)
      if (x ~= nil) and (x > i) then i = x; end
    end
    updatelist.CurrentDirectoryPageNumber = i - 1
    v = x.xpath('//table[contains(@class,"directory_list")]//tr/td[1]/a')
    for j = 1, v.count do
      local v1 = v.get(j)
      local s = v1.toString
      if string.match(s:upper(), ' MANGA$') ~= nil then
        s = s:sub(1, -7)
      end
      links.add(v1.getAttribute('href'))
      names.add(s)
    end
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(site, url)
  local m=NewModule()
  m.category='English'
  m.website=site
  m.rooturl=url
  m.lastupdated='April 5, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  return m
end

function Init()
  AddWebsiteModule('MangaTail', 'https://www.mangatail.me')
  AddWebsiteModule('MangaSail', 'https://www.mangasail.co')
end 
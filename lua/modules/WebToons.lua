local langs = {
  ["en"] = "English",
  ["id"] = "Indonesian",
  ["zh-hant"] = "Chinese",
  ["th"] = "Thai"
}

function getinfo()
  mangainfo.url = mangainfo.url:gsub('(.*)&page=.*', '%1')
  http.cookies.values['ageGatePass'] = 'True'
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    mangainfo.title = x.xpathstring('//meta[@property="og:title"]/@content')
    mangainfo.coverlink=x.xpathstring('//meta[@name="twitter:image"]/@content')
    mangainfo.authors=x.xpathstring('//div[@class="info"]//span[@class="author"]')
    mangainfo.genres=x.xpathstring('//div[@class="info"]/h2')
    if mangainfo.genres == '' then
      mangainfo.genres=x.xpathstring('//div[@class="info challenge"]/p')
    end
    mangainfo.summary=x.xpathstring('//p[@class="summary"]')
    local pages = 1
    local p = 1
    while p <= pages do
      if p > 1 then
        if http.get(mangainfo.url .. '&page=' .. tostring(p)) then
          x=TXQuery.Create(http.document)
        else
          break
        end
      end
      if p == pages then
        local pg = x.xpathstring('//div[@class="detail_lst"]/div[@class="paginate"]/a[last()]/substring-after(@href, "&page=")')
        if pg ~= '' then pages = tonumber(pg) end
      end
      local v=x.xpath('//div[@class="detail_lst"]/ul/li/a')
      for i=1,v.count do
        local v1=v.get(i)
        mangainfo.chapterlinks.add(v1.getAttribute('href'))
        mangainfo.chapternames.add(x.xpathstring('.//span[@class="subj"]/span', v1))
      end
      p = p + 1
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagenumber=0
  task.pagelinks.clear()
  url = url:gsub('(.*)&page=.*', '%1')
  http.Cookies.Values['ageGatePass'] = 'True'
  if http.get(MaybeFillHost(module.rooturl,url)) then
    TXQuery.Create(http.Document).xpathstringall('//div[@id="_imageList"]/img[@class="_images"]/@data-url', task.pagelinks)
    return true
  else
    return false
  end
end

function getnameandlink()
  local selectedLang = module.getoption('lualang')
  local l = langs
  if selectedLang > 0 then
    l = {[findlang(selectedLang)] = ""}
  end
  for key, value in pairs(l) do
    local dirurl = key..'/genre'
    if http.get(module.rooturl..dirurl) then
      local x=TXQuery.Create(http.Document)
	    local v = x.xpath('//div[@class="card_wrap genre"]/ul/li/a')
	    for i = 1, v.count do
        local v1 = v.get(i)
	      names.Add(x.xpathstring('.//div[@class="Info"]//p[@class="subj"]', v1)..' ['..key..']');
		    links.Add(v1.getAttribute('href'));
	    end
	    return no_error
    else
      return net_problem
    end
  end
  if module.getoption('luaincludechallengetitles') then
    getnameandlinkforchallenge()
  end
end

function getnameandlinkforchallenge()
  local selectedLang = module.getoption('lualang')
  local l = langs
  if selectedLang > 0 then
    l = {[findlang(selectedLang)] = ""}
  end
  for key, value in pairs(l) do
    local dirurl = key..'/challenge/list?genreTab=ALL&sortOrder=UPDATE'
    if http.get(module.rooturl..dirurl) then
      local x=TXQuery.Create(http.Document)
      
      
      local pages = 1
      local p = 1
      while p <= pages do
        if p > 1 then
          if http.get(module.rooturl..dirurl..'&page='..tostring(p)) then
            x=TXQuery.Create(http.document)
          else
            break
          end
        end
        if p == pages then
          local pg = x.xpathstring('//div[@class="paginate"]/a[last()]/substring-after(@href, "&page=")')
          if pg ~= '' then pages = tonumber(pg) end
        end
	      local v = x.xpath('//div[@class="challenge_cont_area"]/div[contains(@class,"challenge_lst")]/ul/li/a[contains(@class,"challenge_item")]')
	      for i = 1, v.count do
          local v1 = v.get(i)
	        names.Add(x.xpathstring('./p[@class="subj"]', v1)..' ['..key..']');
		      links.Add(v1.getAttribute('href'));
	      end
        p = p + 1
      end
      
      
	    return no_error
    else
      return net_problem
    end
  end
end

function BeforeDownloadImage()
  http.headers.values['Referer'] = url
  return true
end

function getlang(lang)
  if langs[lang] ~= nil then
    return langs[lang]
  else
    return 'Unknown'
  end
end

function getlanglist()
  local t = {}
  for k, v in pairs(langs) do table.insert(t, v); end
  table.sort(t)
  return t
end

function findlang(lang)
  local t = getlanglist()
  for i, v in ipairs(t) do
    if i == lang then
      lang = v
      break
    end
  end
  for k, v in pairs(langs) do
    if v == lang then return k; end
  end
  return nil
end

function Init()
  local m=NewModule()
  m.category='English'
  m.website='WebToons'
  m.rooturl='https://www.webtoons.com/'
  m.lastupdated='April 14, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
  
  m.addoptioncheckbox('luaincludechallengetitles', 'Include manga titles from WebToons Challenge (takes very very long to create manga list!):', false)
  
  local items = 'All'
  local t = getlanglist()
  for k, v in ipairs(t) do items = items .. '\r\n' .. v; end
  m.addoptioncombobox('lualang', 'Language:', items, 2)
end

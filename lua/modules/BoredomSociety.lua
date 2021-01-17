function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('css("div.titlesinfo_top > h2")')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('css("img.titlesinfo_coverimage")/@src'))
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[contains(span, "Status")]/span[@class="titleinfo_infovalue"]')) 
    mangainfo.summary=x.xpathstringall('//div[contains(span, "Description")]/*[@class="titlesinfo_description"]/text()', '')
    
	  local chapters=x.xpath('//table[@class="titlesinfo_chaptertable"]//tr')
    for i = 1, chapters.count do
      local v1 = chapters.get(i)
	    local s = ''
      local vol = x.xpathstring('td[1]', v1)
      local ch = x.xpathstring('td[2]', v1)
      local title = x.xpathstring('td[3]/a/text()', v1)
      if vol ~= '' then s = s .. string.format('Vol. %s', vol); end
      if s ~= '' then s = s .. ' '; end
      if ch ~= '' then s = s .. string.format('Ch. %s', ch); end
      if title ~= '' then
        if s ~= '' then s = s .. ' - '; end
        s = s .. title
      end
      
      if s ~= '' then
        mangainfo.chapterlinks.add(x.xpathstring('td[3]/a/@href', v1))
        mangainfo.chapternames.add(s)
      end
    end
    
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  http.post(module.rooturl .. '/module/reader/ajax.php', 'readingtype=all')
  http.reset()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//div[@class="reader_mangaimagebox"]/img/@src', task.pagelinks)
    return true
  end
  return false
end

function getnameandlink()
  if http.get(module.rooturl .. '/titles/page/' .. IncStr(url)) then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('css("div.titlelist_name > a")', links, names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. '/titles') then
    x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('//div[@class="titles_pages"]/a[last()-1]'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'BoredomSociety'
  m.rooturl = 'https://www.boredomsociety.xyz'
  m.category = 'English-Scanlation'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
end
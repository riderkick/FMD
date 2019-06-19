function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title=x.xpathstring('//*[@id="manga_name"]')
    end
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//img[@id="cover_img"]/@src'))
    mangainfo.authors=x.xpathstring('//ul[@id="detail_list"]/li[contains(., "Author")]/substring-after(., ":")')
    mangainfo.artists=x.xpathstring('//ul[@id="detail_list"]/li[contains(., "Artist")]/substring-after(., ":")')
    mangainfo.genres=x.xpathstringall('//ul[@id="detail_list"]/li[contains(., "Genres")]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//ul[@id="detail_list"]/li[contains(., "Status")]'))
    mangainfo.summary=x.xpathstring('//ul[@id="detail_list"]/span')
    local v = x.xpath('//table[@id="chapter_table"]//tr')
    for i = 1, v.count do
      local v1 = v.get(i)
      mangainfo.chapterlinks.add(MaybeFillHost(module.RootURL, v1.getAttribute("data-href")))
      mangainfo.chapternames.add(x.xpathstring("./td[1]", v1))
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
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x = TXQuery.Create(http.Document)
    local curCh = x.xpathstring('//select[@id="chapter_select"]/option[@selected]/@value')
    local s = x.xpathstring('//script[contains(., "images")]')
    local slug = Trim(GetBetween("var manga_slug =", ";", s):gsub('"', ''))
	local chapter = Trim(GetBetween("var viewschapter =", ";", s):gsub('"', ''))
    s = GetBetween("var images =", ";", s)
    x.parsehtml(s)
    local v = x.xpath('json(*)()')
    for i = 1, v.count do
      s = string.format("/manga/%s/%s/%s/%s", slug, chapter, curCh, v.get(i).toString)
	  s = string.gsub(s, '//', '/')
	  s = string.gsub(s, '///', '/')
      task.pagelinks.add(MaybeFillHost(module.rooturl, s))
    end
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl .. '/manga/') then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//div[@id="all"]/div[@id="listitem"]/a')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(v1.getAttribute('href'))
      names.add(x.xpathstring('./h1', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'MerakiScans'
  m.rooturl = 'https://merakiscans.com'
  m.category = 'English-Scanlation'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

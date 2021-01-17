function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
	  local v=x.xpath('//ol[@class="breadcrumb"]//li//a')
	  local v1=v.get(3)
	  mangainfo.title = v1.toString
    end
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('//img[@class="thumbnail"]/@src'))
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//ul[@class="manga-info"]/li[contains(., "Status")]//a'))
    mangainfo.authors=x.xpathstring('//ul[@class="manga-info"]/li[contains(., "Author")]//a')
    mangainfo.genres=x.xpathstringall('//ul[@class="manga-info"]/li[contains(., "Genre")]//a')
    mangainfo.summary=x.xpathstring('//h3[text()="Description"]/following-sibling::p')
    if mangainfo.summary == '' then
      mangainfo.summary=x.xpathstring('//div[@class="detail"]/div[@class="content"]')
    end
    x.xpathhrefall('//div[@id="tab-chapper"]//table/tbody/tr/td/a', mangainfo.chapterlinks, mangainfo.chapternames)
    if mangainfo.chapterlinks.count == 0 then
      x.xpathhrefall('//div[@id="list-chapters"]//a[@class="chapter"]', mangainfo.chapterlinks, mangainfo.chapternames)
    end
    for i = 0, mangainfo.chapterlinks.count-1 do
      mangainfo.chapterlinks[i] = module.RootURL .. '/' .. mangainfo.chapterlinks[i]
    end
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  local u = MaybeFillHost(module.rooturl, url)
  if http.get(u) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//img[@class="_lazy chapter-img"]/@src', task.pagelinks)
    task.pagecontainerlinks.text = u
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.RootURL .. '/manga-list.html?listType=allABC') then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//span[@manga-slug]//a')
    for i = 1, v.count do
      local v1 = v.get(i)
      names.Add(Trim(SeparateLeft(v1.toString, '- Raw')));
      links.Add(v1.getAttribute('href'));
    end
    return no_error
  else
    return net_problem
  end
end

function BeforeDownloadImage()
  http.headers.values['Referer'] = task.pagecontainerlinks.text
  return true
end

function AddWebsiteModule(name, url, cat)
  local m = NewModule()
  m.category = cat
  m.Website = name
  m.RootURL = url
  m.LastUpdated = 'March 30, 2019'
  m.totaldirectory=1
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
  return m
end

function Init()
  local cat = 'English-Scanlation'
  AddWebsiteModule('LHTranslation', 'http://lhtranslation.net', cat)
end

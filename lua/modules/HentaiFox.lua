function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//div[@class="info"]/h1')
    end
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="cover"]//img/@src'))
    if module.website == 'HentaiFox' then
      mangainfo.artists=x.xpathstringall('//*[@class="info"]/span[starts-with(.,"Artist")]/substring-after(.,": ")')
      mangainfo.genres=x.xpathstringall('//*[@class="info"]/string-join(./span/a,", ")')
    else
      mangainfo.artists=x.xpathstringall('//div[@class="tags" and contains(h3, "Artist")]/div/a/span/text()')
      mangainfo.genres=x.xpathstringall('//div[@class="tags" and (contains(h3, "Tags") or contains(h3, "Category"))]/div/a/span/text()')
    end
    mangainfo.chapterlinks.add(mangainfo.url)
    mangainfo.chapternames.add(mangainfo.title)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    local v = x.xpath('//*[@class="gallery"]//img/@data-src')
    for i = 1, v.count do
      local s = v.get(i).toString;
      s = s:gsub('^//', 'https://'):gsub('(/%d+)[tT]%.', '%1.')
      task.pagelinks.add(s)
    end
  else
    return false
  end
  return true
end

function getdirectorypagenumber()
  if http.GET(module.RootURL) then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.xpathstring('//*[@class="pagination"]/a[last()-1]'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  if http.get(module.rooturl .. '/pag/' .. IncStr(url) .. '/') then
    local x = TXQuery.Create(http.Document)
    if module.website == 'HentaiFox' then
      x.xpathhrefall('//*[@class="galleries_overview"]//*[contains(@class,"item")]/a', links, names)
    else
      x.xpathhrefall('//*[@class="preview_item"]/*[@class="caption"]/a', links, names)
    end
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(name, url)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = 'H-Sites'
  m.lastupdated='May 31, 2018'
  m.sortedlist = true
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
  return m
end

function Init()
  AddWebsiteModule('HentaiFox', 'https://hentaifox.com')
  AddWebsiteModule('AsmHentai', 'https://asmhentai.com')
end

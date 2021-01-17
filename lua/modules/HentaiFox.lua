function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//div[@class="info"]/h1')
    end
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="cover"]//img/@src'))
    if module.website == 'HentaiFox' then
      mangainfo.artists=x.xpathstringall('//*[@class="artists"]/li/a')
      mangainfo.genres=x.xpathstringall('//*[@class="tags"]//li/a')
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
    if module.website == 'HentaiFox' then
      local galleryId = x.xpathstring('//a[@class="g_button"]/@href'):match('/.-/(%d+)/')
      task.pagenumber = tonumber(x.xpathstring('//span[@class="i_text pages" and contains(., "Pages")]/substring-after(.,": ")'))
      for i = 1, task.pagenumber do
        task.PageContainerLinks.Add(MaybeFillHost(module.RootURL, '/g/' .. galleryId .. '/' .. i))
      end
    else
      local v = x.xpath('//*[@class="gallery"]//img/@data-src')
      for i = 1, v.count do
        local s = v.get(i).toString;
        s = s:gsub('^//', 'https://'):gsub('(/%d+)[tT]%.', '%1.')
        task.pagelinks.add(s)
      end
    end
  else
    return false
  end
  return true
end

function getdirectorypagenumber()
  if http.GET(module.RootURL) then
    local x = TXQuery.Create(http.Document)
    if module.website == 'HentaiFox' then
      page = tonumber(x.xpathstring('(//*[@class="pagination"]/li/a)[last()-1]'))
      if page == nil then page = 1 end
    else
      page = tonumber(x.xpathstring('//*[@class="pagination"]/a[last()-1]'))
      if page == nil then page = 1 end
    end
    return no_error
  else
    return net_problem
  end
end

function getimageurl()
  local u = MaybeFillHost(module.RootURL, task.PageContainerLinks[workid])
  if http.Get(u) then
    task.PageLinks[workid] = TXQuery.Create(http.document).XPathString('//div[@class="full_image"]//img/@src')
    return true
  end
  return false
end

function getnameandlink()
  if http.get(module.rooturl .. '/pag/' .. IncStr(url) .. '/') then
    local x = TXQuery.Create(http.Document)
    if module.website == 'HentaiFox' then
      x.xpathhrefall('//*[@class="lc_galleries"]//*[@class="caption"]//a', links, names)
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
  m.sortedlist = true
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
  m.ongetimageurl = 'getimageurl'
  return m
end

function Init()
  AddWebsiteModule('HentaiFox', 'https://hentaifox.com')
  AddWebsiteModule('AsmHentai', 'https://asmhentai.com')
end

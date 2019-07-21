-- Get manga info and chapter list:
function getinfo()
  local x = nil
  
  mangainfo.url = MaybeFillHost(module.rooturl, url)
  if http.get(mangainfo.url) then
    x = TXQuery.Create(http.document)
    mangainfo.coverlink = x.xpathstring('//figure[@class="image"][1]/img/@src')
    mangainfo.artists = x.xpathstring('//table[@class="view-page-details"]//a[contains(@href, "=artist:")]/text()')
    mangainfo.title = '[' .. mangainfo.artists .. '] ' .. x.xpathstring('//h1[@class="title"]/text()') .. ' (' .. x.xpathstring('//table[@class="view-page-details"]//a[contains(@href, "=publisher:")]/text()') .. ')'
    mangainfo.genres = x.xpathstringall('//table[@class="view-page-details"]//a[contains(@href, "=tag:")]/text()')
    mangainfo.authors = x.xpathstring('//table[@class="view-page-details"]//a[contains(@href, "=publisher:")]/text()')
    mangainfo.status = 0
    mangainfo.chapterlinks.add(url)
    mangainfo.chapternames.add(mangainfo.title)
    
    return no_error
  end
  return net_problem
end

-- Get page number and page container links:
function getpagenumber()
  local x = nil
  
  if http.get(MaybeFillHost(module.rooturl, url)) then
    x = TXQuery.Create(http.document)
    x.xpathstringall('(//div[@class="box"])[2]/div/div/a/@href', task.pagecontainerlinks)
    task.pagenumber = task.pagecontainerlinks.count
    
    return true
  end
  return false
end

-- Get image urls from page containers:
function getimageurl()
  local s = MaybeFillHost(module.rooturl, task.pagecontainerlinks[workid])
  
  if http.get(s) then
    task.pagelinks[workid] = TXQuery.Create(http.document).xpathstring('//img[@id="currImage"]/@src')
    
    return true
  end
  return false
end

-- Get last page number for manga directory:
function getdirectorypagenumber()
  if http.get(module.rooturl) then
    page = tonumber(TXQuery.Create(http.document).xpathstring('(//ul[@class="pagination-list"])[1]/li[last()]/a/text()'))
    
    return no_error
  end
  return net_problem
end

-- Go through all directory pages and get names and links for manga entries:
function getnameandlink()
  local v, x = nil
  
  if http.get(module.rooturl .. '/page/' .. IncStr(url)) then
    x = TXQuery.Create(http.document)
    v = x.xpath('//div[@class="container"]/div/div/a[contains(@href, "/view/")]')
    for i = 1, v.count do
      v1 = v.get(i)
      links.add(v1.getattribute('href'))
      names.add(x.xpathstring('div/header/@title', v1))
    end
    
    return no_error
  end
  return net_problem
end

-- Initialize module:
function Init()
  m=NewModule()
  m.category='H-Sites'
  m.website='HentaiNexus'
  m.rooturl='https://hentainexus.com'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
  m.ongetnameandlink = 'getnameandlink'
  m.ongetimageurl='getimageurl'
end

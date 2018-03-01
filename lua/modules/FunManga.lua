local ALPHA_LIST = '#abcdefghijklmnopqrstuvwxyz'

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//div[@class="content"]//h5')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="content"]//img/@src'))
    mangainfo.authors=x.xpathstringall('//dl[@class="dl-horizontal"]/dt[starts-with(.,"Author")]/following-sibling::dd[1]/a')
    mangainfo.artists=x.xpathstringall('//dl[@class="dl-horizontal"]/dt[starts-with(.,"Artist")]/following-sibling::dd[1]/a')
    mangainfo.genres=x.xpathstringall('//dl[@class="dl-horizontal"]/dt[starts-with(.,"Categories")]/following-sibling::dd[1]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//dl[@class="dl-horizontal"]/dt[starts-with(.,"Status")]/following-sibling::dd[1]'))
    mangainfo.summary=x.xpathstring('//div[@class="content"]/div/div[contains(@class,"note")]')
    local v=x.xpath('//ul[@class="chapter-list"]/li/a')
    for i=1,v.count do
      local v1=v.get(i)
      mangainfo.chapterlinks.add(v1.getattribute('href'))
      mangainfo.chapternames.add(x.xpathstring('span[1]', v1))
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber=0
  local s = MaybeFillHost(module.rooturl, url)
  if Pos('/all-pages', s) == 0 then s = s .. '/all-pages' end
  if http.get(s) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//div[contains(@class,"content-inner")]//img/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  local s = ''
  if module.CurrentDirectoryIndex ~= 0 then
    s = '/'..ALPHA_LIST:sub(module.CurrentDirectoryIndex+1,module.CurrentDirectoryIndex+1)
  end
  local dirurl = '/manga-list'
  if module.website == 'MangaDoom' then dirurl = '/manga-directory' end
  if http.get(module.rooturl .. dirurl .. s) then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//div[@class="content"]/div/div[@class="row"]//li/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(name, url)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = 'English'
  m.lastupdated = 'March 1, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.totaldirectory = ALPHA_LIST:len()
  return m
end 

function Init()
  AddWebsiteModule('FunManga', 'http://www.funmanga.com')
  AddWebsiteModule('MangaDoom', 'http://www.mngdoom.com')
end

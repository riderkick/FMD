local perpage = 30
local apiurl = 'https://api.mghubcdn.com/graphql'
local cdnurl = 'https://img.mghubcdn.com/file/imghub/'

function getx()
  if module.website == 'MangaReaderSite' then
    return "mr01"
  elseif module.website == 'MangaFoxFun' then
    return "mf01"
  elseif module.website == 'MangaKakalotFun' then
    return "mn01"
  elseif module.website == 'MangaHereFun' then
    return "mh01"
  else
    return "m01"
  end
end

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//div[@id="mangadetail"]//h1/text()')
    mangainfo.coverlink=x.xpathstring('//div[@id="mangadetail"]//img/@src')
    mangainfo.authors=x.xpathstring('//div[@id="mangadetail"]//div/span[contains(., "Author")]/following-sibling::span')
    mangainfo.artists=x.xpathstring('//div[@id="mangadetail"]//div/span[contains(., "Artist")]/following-sibling::span')
    mangainfo.genres=x.xpathstringall('//div[@id="mangadetail"]//div/p/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@id="mangadetail"]//div/span[contains(., "Status")]/following-sibling::span'))
    mangainfo.summary=x.xpathstring('//div[contains(@id, "noanim-content-tab-pane")]/div/p')
    v=x.xpath('//div[contains(@id, "noanim-content-tab-pane")]/ul/li/a')
    for i=1,v.count do
      v1=v.get(i)
      mangainfo.chapterlinks.add(v1.getattribute('href'))
      mangainfo.chapternames.add(x.xpathstring('span', v1))
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  local chapter = url:match('/chapter%-(.+)$'):gsub('/$', '')
  local slug = url:match('/chapter/(.+)/')
  local q = '{"query":"{chapter(x:'..getx()..',slug:\\"'..slug..'\\",number:'..chapter..'){id,title,mangaID,number,slug,date,pages,manga{id,title,slug,mainSlug,isWebtoon,isYaoi}}}"}'
  http.mimetype = 'application/json'
  if http.post(apiurl, q) then
    x=TXQuery.Create(http.Document)
    v=x.xpath('json(json(*).data.chapter.pages)/*')
    for i = 1, v.count do
      v1=v.get(i)
      task.pagelinks.add(cdnurl .. v1.toString)
    end
  else
    return false
  end
  return true
end

function getnameandlink()
  local offset = perpage * tonumber(url)
  local q = '{"query":"{search(x:'..getx()..',q:\\"\\",genre:\\"all\\",mod:ALPHABET,count:true,offset:'..tostring(offset)..'){rows{id,title, slug},count}}"}'
  http.mimetype = 'application/json'
  if http.post(apiurl, q) then
    x = TXQuery.Create(http.Document)
    x.xpathstringall('json(*).data.search.rows()/concat("'..module.rooturl..'/manga/", slug)', links)
    x.xpathstringall('json(*).data.search.rows().title', names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  local q = '{"query":"{search(x:'..getx()..',q:\\"\\",genre:\\"all\\",mod:ALPHABET,count:true,offset:0){rows{id,title, slug},count}}"}'
  http.mimetype = 'application/json'
  if http.post(apiurl, q) then
    x = TXQuery.Create(http.Document)
    local total = tonumber(x.xpathstring('json(*).data.search.count'))
    if total == nil then total = 1 end
    page = math.floor(total / perpage)
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
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
  return m
end 

function Init()
  AddWebsiteModule('MangaHubIO', 'https://mangahub.io')
  AddWebsiteModule('MangaReaderSite', 'https://mangareader.site')
  AddWebsiteModule('MangaFoxFun', 'https://mangafox.fun')
  AddWebsiteModule('MangaKakalotFun', 'https://mangakakalot.fun')
  AddWebsiteModule('MangaHereFun', 'https://mangahere.onl')
end

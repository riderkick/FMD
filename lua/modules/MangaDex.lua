function getinfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  http.cookies.values['mangadex_h_toggle'] = '1'
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    if mangainfo.title=='' then mangainfo.title=x.xpathstring('//meta[@property="og:title"]/replace(@content,"\\s\\(\\w+\\)\\s-\\sMangaDex$","")') end
    mangainfo.coverlink=MaybeFillHost(module.rooturl,x.xpathstring('//div[contains(@class, "card-body")]//img[@class]/@src'))
    mangainfo.authors=x.xpathstring('//div[./text()="Author:"]/string-join(./following-sibling::div,", ")')
    mangainfo.artists=x.xpathstring('//div[./text()="Artist:"]/string-join(./following-sibling::div,", ")')
    mangainfo.genres = x.xpathstringall('//div[./text()="Genres:"]/following-sibling::div//a')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[contains(./text(),"status")]/following-sibling::div'))
    mangainfo.summary=x.xpathstring('//div[./text()="Description:"]/following-sibling::div')
    local l='//div[contains(@class, "chapter-container")]//div[@data-id and not(starts-with(./div[contains(@class, "text-warning")], "in "))]'
    local n=''
    if module.getoption('luashowalllang') then
      n='/concat(.," [",../div[6]/img/@title,"]"'
    else
      l=l..'[./div/img[@title="English"]]'
    end
    if module.getoption('luashowscangroup') then
      if n=='' then n='/concat(.' end
      n=n..'," [",../div[7],"]"'
    end
    l=l..'/div[2]'
    if n~='' then n=n..')' end
    n=l..n
    l=l..'/a/@href'
    local nurl=''
    while true do
      x.xpathstringall(l,mangainfo.chapterlinks)
      x.xpathstringall(n,mangainfo.chapternames)
      if http.terminated then break end
      nurl=x.xpathstring('//ul[contains(@class,"pagination")]/li[contains(@class,"active")]/following-sibling::li[@class="page-item"]/a/@href')
      if nurl=='' then break end
      if http.get(MaybeFillHost(module.rooturl,nurl)) then
        x.parsehtml(http.document)
      else
        break
      end      
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  http.cookies.values['mangadex_h_toggle'] = '1'
  local chapterid = url:match('chapter/(%d+)')
  if http.get(MaybeFillHost(module.rooturl,'/api/chapter/'..chapterid)) then
    local x=TXQuery.Create(http.Document)
    local hash = x.xpathstring('json(*).hash')
    local srv = x.xpathstring('json(*).server')
    local v = x.xpath('json(*).page_array()')
    for i = 1, v.count do
      local v1 = v.get(i)
      local s = MaybeFillHost(module.rooturl, srv .. '/' .. hash .. '/' .. v1.toString)
      task.pagelinks.add(s)
    end
    return true
  else
    return false
  end
  return true
end

local dirurl='/titles/2'

function getdirectorypagenumber()
  http.cookies.values['mangadex_h_toggle'] = '1'
  http.cookies.values['mangadex_title_mode'] = '2'
  if http.GET(module.RootURL .. dirurl) then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.xpathstring('(//ul[contains(@class,"pagination")]/li/a)[last()]/@href'):match('/2/(%d+)'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  http.cookies.values['mangadex_h_toggle'] = '1'
  http.cookies.values['mangadex_title_mode'] = '2'
  if http.GET(module.rooturl .. dirurl .. '/' .. IncStr(url) .. '/') then
    local x = TXQuery.Create(http.document)
    x.xpathhrefall('//a[contains(@class, "manga_title")]',links,names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='English'
  m.website='MangaDex'
  m.rooturl='https://mangadex.org'
  m.lastupdated='February 28, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
  
  m.maxtasklimit=1
  m.maxconnectionlimit=2

  m.addoptioncheckbox('luashowalllang', 'Show all language', false)
  m.addoptioncheckbox('luashowscangroup', 'Show scanlation group', false)
end

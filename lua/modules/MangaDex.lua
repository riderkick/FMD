function getinfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    if mangainfo.title=='' then mangainfo.title=x.xpathstring('//meta[@property="og:title"]/replace(@content,"\\s\\(\\w+\\)\\s-\\sMangaDex$","")') end
    mangainfo.coverlink=MaybeFillHost(module.rooturl,x.xpathstring('//img[@title="Manga image"]/@src'))
    mangainfo.authors=x.xpathstring('//tr[./th="Author:"]/string-join(./td,", ")')
    mangainfo.artists=x.xpathstring('//tr[./th="Artist:"]/string-join(./td,", ")')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//tr[./th="Status:"]'))
    mangainfo.summary=x.xpathstring('//tr[./th="Description:"]/td')
    local l='//*[@id="chapters"]//tr[@id]'
    local n=''
    if module.getoption('luashowalllang') then
      n='/concat(.," [",../td[2]/img/@title,"]"'
    else
      l=l..'[./td/img[@title="English"]]'
    end
    if module.getoption('luashowscangroup') then
      if n=='' then n='/concat(.' end
      n=n..'," [",../td[3],"]"'
    end
    l=l..'/td[1]'
    if n~='' then n=n..')' end
    n=l..n
    l=l..'/a/@href'
    local nurl=''
    while true do
      x.xpathstringall(l,mangainfo.chapterlinks)
      x.xpathstringall(n,mangainfo.chapternames)
      if http.terminated then break end
      nurl=x.xpathstring('//ul[@class="pagination"]/li[@class="active"]/following-sibling::li[@class="paging"]/a/@href')
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
  if http.get(MaybeFillHost(module.rooturl,url)) then
    local s=StreamToString(http.document)
    local lurl=MaybeFillHost(module.rooturl,AppendURLDelim(GetBetween('var server = \'','\';',s))..GetBetween('var dataurl = \'','\';',s)..'/')
    local page_array=GetBetween('var page_array = [','];',s)
    task.pagelinks.commatext=GetBetween('var page_array = [','];',s):gsub('\'','')
    for i=0,task.pagelinks.count-1 do
      task.pagelinks[i]=lurl..task.pagelinks[i]
    end
    return true
  else
    return false
  end
  return true
end

function taskstart()
  for i=0, task.pagelinks.count-1 do
    task.pagelinks[i]=MaybeFillHost(module.rooturl,task.pagelinks[i])
  end
  return true
end

local dirurl='/titles'

function getdirectorypagenumber()
  if http.get(module.rooturl..dirurl) then
    x=TXQuery.Create(http.document)
    local perpage=tonumber(RegExprGetMatch('/(\\d+)$',x.xpathstring('//ul[@class="pagination"]/li[@class="active"]/following-sibling::li[@class="paging"]/a/@href'),1))
    local lastpage=tonumber(RegExprGetMatch('/(\\d+)$',x.xpathstring('//ul[@class="pagination"]/li[@class="paging"][last()]/a/@href'),1))
    if perpage==nil then perpage=100 end
    if lastpage==nil then lastpage=perpage end
    module.tag=perpage
    page=Round(lastpage/perpage)+1
    return true
  else
    return false
  end
end

function getnameandlink()
  local lurl=dirurl
  if url~='0' then
    lurl=lurl..'/'..tostring(module.tag*tonumber(url))
  end
  if http.GET(module.rooturl..lurl) then
    TXQuery.Create(http.document).xpathhrefall('//*[@id="content"]//tr/td[2]/a',links,names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='English'
  m.website='MangaDex'
  m.rooturl='https://mangadex.com'
  m.lastupdated='February 28, 2018'
  m.ongetinfo='getinfo'
  m.ontaskstart='taskstart'
  m.ongetpagenumber='getpagenumber'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
  m.ongetnameandlink='getnameandlink'
  
  m.maxtasklimit=1
  m.maxconnectionlimit=2

  m.addoptioncheckbox('luashowalllang', 'Show all language', false)
  m.addoptioncheckbox('luashowscangroup', 'Show scanlation group', false)
end

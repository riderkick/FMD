function getinfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  http.cookies.values['mangadex_h_toggle'] = '1'
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    if mangainfo.title=='' then mangainfo.title=x.xpathstring('//meta[@property="og:title"]/replace(@content,"\\s\\(\\w+\\)\\s-\\sMangaDex$","")') end
    mangainfo.coverlink=MaybeFillHost(module.rooturl,x.xpathstring('//img[@title="Manga image"]/@src'))
    mangainfo.authors=x.xpathstring('//tr[./th="Author:"]/string-join(./td,", ")')
    mangainfo.artists=x.xpathstring('//tr[./th="Artist:"]/string-join(./td,", ")')
    mangainfo.genres = x.xpathstringall('//tr[./th="Genres:"]/td/span/a')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//tr[./th="Status:"]'))
    mangainfo.summary=x.xpathstring('//tr[./th="Description:"]/td')
    local l='//table//tr[@id and not(starts-with(./td/time, "in "))]'
    local n=''
    if module.getoption('luashowalllang') then
      n='/concat(.," [",../td[4]/img/@title,"]"'
    else
      l=l..'[./td/img[@title="English"]]'
    end
    if module.getoption('luashowscangroup') then
      if n=='' then n='/concat(.' end
      n=n..'," [",../td[5],"]"'
    end
    l=l..'/td[2]'
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
  http.cookies.values['mangadex_h_toggle'] = '1'
  if http.get(MaybeFillHost(module.rooturl,url)) then
    local s=StreamToString(http.document)
    if Pos('var page_array', s) == 0 then return false; end
    local lurl=MaybeFillHost(module.rooturl,AppendURLDelim(GetBetween('var server = \'','\';',s))..GetBetween('var dataurl = \'','\';',s)..'/')
    task.pagelinks.commatext=GetBetween('var page_array = [','];',s):gsub('\'','')
    task.pagelinks.text=Trim(task.pagelinks.text)
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

local dirurl='/titles/'
local ALPHA_LIST_UP = '~ABCDEFGHIJKLMNOPQRSTUVWXYZ'
function getnameandlink()
  local lurl=dirurl
  lurl = lurl .. ALPHA_LIST_UP:sub(module.CurrentDirectoryIndex+1,module.CurrentDirectoryIndex+1)
  lurl = lurl .. '/' .. IncStr(url)
  http.cookies.values['mangadex_h_toggle'] = '1'
  if http.GET(module.rooturl..lurl) then
    local x = TXQuery.Create(http.document)
    x.xpathhrefall('//*[@id="content"]//tr/td[2]/a',links,names)
    local page = x.xpathstring('//ul[@class="pagination"]/li[last()]/a/@href')
    page = page:match('/(%d+)/?$')
    page = tonumber(page)
    if page == nil then page = 1; end
    updatelist.CurrentDirectoryPageNumber = page
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
  m.ontaskstart='taskstart'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.totaldirectory = ALPHA_LIST_UP:len()
  
  m.maxtasklimit=1
  m.maxconnectionlimit=2

  m.addoptioncheckbox('luashowalllang', 'Show all language', false)
  m.addoptioncheckbox('luashowscangroup', 'Show scanlation group', false)
end

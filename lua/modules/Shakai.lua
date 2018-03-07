function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if Pos('/element-list', mangainfo.url) > 0 then
    mangainfo.url = mangainfo.url:gsub('/element%-list$', '')
  end
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//*[@class="wrapper__heading"]/h1')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="make__cover"]/img/@src'))
    mangainfo.authors=x.xpathstringall('//table[@class="make__table-info"]//tr[contains(td, "Автор")]/td/a')
    mangainfo.genres=x.xpathstringall('//table[@class="make__table-info"]//tr[contains(td, "Жанры")]/td/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//table[@class="make__table-info"]//tr[contains(td, "Выпуск")]/td'), 'продолжается', 'завершен')
    mangainfo.summary=x.xpathstring('//div[@class="make__description"]')
    if http.get(mangainfo.url .. '/element-list') then
      x.parsehtml(http.document)
      local v = x.xpath('//div[@class="post-element__description"]')
      for i = 1, v.count do
        local v1 = v.get(i)
        mangainfo.chapterlinks.add(x.xpathstring('div[@class="post-element__action"]/a/@href', v1))
        mangainfo.chapternames.add(x.xpathstring('div[@class="post-element__meta"]', v1))
      end
      InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
      return no_error
    else
      return net_problem
    end
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber=0
  local id, chapter = url:match('read/([^/]+)/([^/]+)/')
  local data = 'dataRun=api-manga&dataRequest=' .. id
  if http.post(module.rooturl .. '/take/api-manga/request/shakai', data) then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('json(*).data()[data-first="' .. chapter .. '"].data-second()')
    for i = 1, v.count do
      local v1 = v.get(i)
      task.pagelinks.add(v1.tostring)
    end
  else
    return false
  end
  return true
end

local cataloguri = '/take/catalog/request/shakai'
function getquery(page)
  local query = 'dataRun=catalog&selectCatalog=manga&searchData=&' ..
    'selectPage=%s&' ..
    '&itemMarker=%s&' ..
    'dataModeration=&dataSorting=po-alfavitu,false,false,false,false,false,false&' ..
    'dataType=false,false,false,false,false,false,false,false&dataStatus=false,false,false,false&' ..
    'dataList=&dataGenre=false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false,false&dataSeason=false,false,false,false,false,false'
  return string.format(query, page, os.date("!%Y-%m-%d %X"))
end

function getnameandlink()
  if tonumber(url) < 0 then return no_error end
  if http.post(module.RootURL .. cataloguri, getquery(IncStr(url))) then
    local s = StreamToString(http.document):gsub('&quot;', '\\"')
    local x = TXQuery.Create(s)
    local v = x.xpath('json(*).result()')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(x.xpathstring('output-link', v1))
      names.add(x.xpathstring('output-name', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.post(module.RootURL .. cataloguri, getquery('1')) then
    local s = StreamToString(http.document):gsub('&quot;', '\\"')
    local x = TXQuery.Create(s)
    page = tonumber(x.xpathstring('json(*).create'))
    if page == nil then page = 1; end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'Shakai'
  m.rooturl = 'http://shakai.ru'
  m.category = 'Russian'
  m.lastupdated='March 6, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
end
function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  local id = RegExprGetMatch('(manga|comicDetail)\\/(.+?)\\/', url, 2)
  local data = string.format('url=/manga/getBookDetail/%s&method=GET', id)
  if url:match('comicDetail') == nil then
    data = data .. '&api=/mangaheatapi/web'
  end
  if http.post(module.rooturl .. '/api', data) then
    local x=TXQuery.Create(http.document)
    local json = x.xpath('json(*)')
    if mangainfo.title == '' then
      mangainfo.title=x.xpathstring('./title',json)
    end
    mangainfo.coverlink=x.xpathstring('./cover',json)
    mangainfo.authors=x.xpathstring('./author',json)
    mangainfo.genres=x.xpathstring('./tags',json)
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('./updateStatus',json))
    mangainfo.summary=x.xpathstring('./summary',json)
    local v = x.xpath('jn:members(child)', json)
    for i = 1, v.count do
      local v1 = v.get(i)
      mangainfo.chapterlinks.add(x.xpathstring('./chapterId', v1))
      mangainfo.chapternames.add(x.xpathstring('./title', v1) .. ' - ' .. x.xpathstring('./summary', v1))
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  url = url:gsub('/', '')
  local data = string.format('url=/manga/getChapterImages/%s&method=GET', url)
  if tonumber(url) ~= nil then
    data = data .. '&api=/mangaheatapi/web'
  end
  if http.post(module.rooturl .. '/api', data) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('json(*).images()', task.pagelinks)
  else
    return false
  end
  return true
end

local dirdata = {
  'url=/manga/getBookRecommend/%s?tag=&method=GET',
  'url=/manga/getAZ/%s?tag=&method=GET&api=/mangaheatapi/web'
}

function getnameandlink()
  local data = string.format(dirdata[module.CurrentDirectoryIndex+1], IncStr(url))
  local d = 'manga'
  if module.CurrentDirectoryIndex == 0 then
    d = 'comicDetail'
  end
  if http.post(module.rooturl .. '/api', data) then
    local x = TXQuery.Create(http.Document)
    local hasTitles = false
    local v = x.xpath('json(*).child()')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(module.rooturl .. string.format('/%s/%s/%s', d, x.xpathstring('bookId', v1), x.xpathstring('title', v1)))
      names.add(x.xpathstring('title', v1))
      hasTitles = true
    end
    if hasTitles then
      updatelist.CurrentDirectoryPageNumber = 2 --updatelist.CurrentDirectoryPageNumber + 1
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'ZingBox'
  m.rooturl = 'http://www.zingbox.me'
  m.category = 'English'
  m.lastupdated='May 17, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.totaldirectory = #dirdata
end

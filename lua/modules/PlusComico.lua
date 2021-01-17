function getinfo_store()
  local x=TXQuery.Create(http.document)
  mangainfo.title=x.xpathstring('//div[@class="_title"]')
  mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//meta[@property="og:image"]/@content'))
  mangainfo.authors=x.xpathstring('//*[contains(@class,"__author")]')
  mangainfo.genres=x.xpathstringall('//li[contains(@class,"__list-genre-item")]/p/a')
  mangainfo.summary=x.xpathstring('//p[@class="_description"]')
  local id = mangainfo.url:match('/(%d+)/?$')
  http.reset()
  if http.post(module.rooturl .. '/store/api/getTitleArticles.nhn', 'titleNo='..id) then
    x.parsehtml(http.document)
    local v = x.xpath('json(*).result.list().articleList()')
    for i = 1, v.count do
      local v1 = v.get(i)
      if x.xpathstring('./freeFlg', v1) == 'Y' then
        mangainfo.chapterlinks.add(x.xpathstring('./articleDetailUrl', v1))
        mangainfo.chapternames.add(x.xpathstring('./subtitle', v1))
      end
    end
  end
end

function getinfo_manga()
  local x=TXQuery.Create(http.document)
  mangainfo.title=x.xpathstring('//*[contains(@class,"__ttl")]')
  mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//meta[@property="og:image"]/@content'))
  mangainfo.authors=x.xpathstring('//*[contains(@class,"__author")]')
  mangainfo.genres=x.xpathstringall('//*[contains(@class,"__meta")]//a')
  mangainfo.summary=x.xpathstring('//*[contains(@class,"__description")]')
  local id = mangainfo.url:match('/(%d+)/?$')
  http.reset()
  if http.post(module.rooturl .. '/api/getArticleList.nhn', 'titleNo='..id) then
    x.parsehtml(http.document)
    local v = x.xpath('json(*).result.list()')
    for i = 1, v.count do
      local v1 = v.get(i)
      if x.xpathstring('./freeFlg', v1) == 'Y' then
        mangainfo.chapterlinks.add(x.xpathstring('./articleDetailUrl', v1))
        mangainfo.chapternames.add(x.xpathstring('./subtitle', v1))
      end
    end
  end
end

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    if mangainfo.url:match('/store/') ~= nil then
      getinfo_store()
    else
      getinfo_manga()
    end
    return no_error
  else
    return net_problem
  end
end

function getpagenumber_manga()
  local x=TXQuery.Create(http.Document)
  local s = x.xpathstring('//script[contains(., "imageData:")]')
  s = GetBetween('imageData:', ']', s) .. ']'
  x.parsehtml(s)
  x.xpathstringall('json(*)()', task.pagelinks)
  return true
end

function getpagenumber_store()
  if http.lasturl:match('param=') == nil then return false; end  
  local base = SeparateLeft(http.lasturl, 'index.php')
  local param = GetBetween('param=', '&', http.lasturl)
  local ts = os.time() * 1000
  base = base .. string.format('diazepam_hybrid.php?reqtype=0&param=%s&ts=%d&_=%d', param, ts, ts+1500)
  math.randomseed(os.time())
  math.random(); math.random(); math.random();
  local tpurl = string.format('&mode=7&file=face.xml&callback=jQ%d_%d', math.random(1000), math.random(1000))
  if http.get(base .. tpurl) then
    if http.terminated then return false; end
    local s = GetBetween('("', '")', StreamToString(http.document))
    local x=TXQuery.Create(s)
    local total_pages = tonumber(x.xpathstring('//TotalPage'))
    if total_pages == nil then return false; end
    for i = 0, total_pages-1 do
      task.pagelinks.add(base .. string.format('&mode=8&file=%04d.xml', i))
    end
    task.pagecontainerlinks.text = http.cookies.text
  else
    return false
  end
  return false
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    if url:match('/store/') ~= nil then
      return getpagenumber_store()
    else
      return getpagenumber_manga()
    end
  else
    return false
  end
end

local js = require 'utils.jsunpack'
function downloadimage()
  if url:match('%.xml') == nil then return http.get(url); end
  http.cookies.text = task.pagecontainerlinks.text
  if http.get(url) then
    local x = TXQuery.Create(http.Document)
    local a = js.splitstr(x.xpathstring('//Scramble'), ',')
    local imgurl = SeparateLeft(url, '&mode=')
    imgurl = imgurl .. string.format('&mode=1&file=%04d_0000.bin', workid)
    if http.get(imgurl) then
      local s = TImagePuzzle.Create(4, 4)
      s.multiply = 8
      local n = 0
      for i, v in ipairs(a) do
        local j = tonumber(v)
        if j == nil then j = 0 end
        s.matrix[j] = n;
        n = n + 1
      end
      s.descramble(http.document, http.document)
      return true
    end
    return false
  end
  return false
end

local dirurls = {
--  '/store/ranking/list.nhn',
  '/manga/ranking/list.nhn'
}

function getnameandlink()
  local lurl = dirurls[module.CurrentDirectoryIndex+1]
  local data = 'page='..IncStr(url)..'&rankingType=original'
  if http.post(module.rooturl .. lurl, data) then
    local x = TXQuery.Create(http.Document)
    local total = tonumber(x.xpathstring('json(*).result.totalPageCnt'))
    if total == nil then total = 1 end
    updatelist.CurrentDirectoryPageNumber = total
    local v = x.xpath('json(*).result.list()')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(x.xpathstring('title_url', v1))
      names.add(x.xpathstring('title_name', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'PlusComico'
  m.rooturl = 'http://plus.comico.jp'
  m.category = 'Raw'
  m.lastupdated='May 1, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.totaldirectory = #dirurls
  m.ondownloadimage = 'downloadimage'
end

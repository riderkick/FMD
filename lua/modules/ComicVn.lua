function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//div[@class="manga-info"]//h1')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="manga-info"]/div[contains(@class, "manga-detail")]//img/@src'))
    mangainfo.authors=x.xpathstring('//div[@class="manga-info"]/div[contains(@class, "manga-detail")]//ul/li[contains(span, "Tác giả")]/span[2]')
    mangainfo.artists=x.xpathstring('//div[@class="manga-info"]/div[contains(@class, "manga-detail")]//ul/li[contains(span, "Họa sĩ")]/span[2]')
    mangainfo.genres=x.xpathstringall('//div[@class="manga-info"]/div[contains(@class, "manga-detail")]//ul/li[contains(span, "Thể loại")]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="manga-info"]/div[contains(@class, "manga-detail")]//ul/li[contains(span, "Trạng thái")]'), 'Đang thực hiện', 'Đã hoàn thành')
    mangainfo.summary=x.xpathstring('//div[contains(@class, "manga-summary")]')
    x.xpathhrefall('//div[contains(@class,"manga-chapter")]//ul/li/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber=0
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//div[contains(@class, "manga-chapter-image")]/textarea/img/@src', task.pagelinks)
  else
    return false
  end
  return true
end

local dirurl = '/noi-bat/'
local perpage = 30

function getnameandlink()
  if tonumber(url) < 0 then return no_error end
  if http.get(module.rooturl .. dirurl .. tostring(tonumber(url) * perpage)) then
    local x = TXQuery.Create(http.Document)
    x.xpathhrefall('//div[contains(@class, "manga-list")]//div[contains(@class,"tit")]/a', links, names)
    local s = x.xpathstring('(//ul[@class="pagination"])[1]/li[last()-1]/a/@href')
    s = tonumber(s:match('(%d+)/?$'))
    if s ~= nil then
      s = s / perpage
      updatelist.CurrentDirectoryPageNumber = s
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'ComicVn'
  m.rooturl = 'https://beeng.net'
  m.category = 'Vietnamese'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end
----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga-list/page-'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local pages, x, v = nil
  local u = MaybeFillHost(module.RootURL, url)
  local p = 1
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Coverlink = MaybeFillHost(module.RootURL, x.XPathString('//div[@class="comic-info"]/div/img/@src'))
  mangainfo.Title     = x.XPathString('//div[@class="info"]/h1')
  mangainfo.Authors   = x.XPathStringAll('//div[@class="info"]//div[@class="author"]/a')
  mangainfo.Genres    = x.XPathStringAll('//div[@class="info"]//div[@class="genre"]/a')
  mangainfo.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="info"]//div[@class="update"]/span[last()]'))
  mangainfo.Summary   = x.XPathString('//div[@class="comic-description"]/p')
  
  pages = tonumber(x.XPathString('//div[@class="pagination"]//a[contains(@class, "page-numbers")][last()]/substring-after(@href, "/page-")'))
  if pages == nil then pages = 1 end
  while true do
    for _, v in ipairs(x.XPathI('//div[contains(@class, "chapters-wrapper")]//h2[@class="chap"]/a')) do
      mangainfo.ChapterNames.Add(x.XPathString('text()', v))
      mangainfo.ChapterLinks.Add(v.GetAttribute('href'))
    end
    p = p + 1
    if p > pages then
      break
    elseif http.Get(u .. '/page-' .. tostring(p)) then
      x = TXQuery.Create(http.Document)
    else
      break
    end
  end
  InvertStrings(mangainfo.ChapterLinks, mangainfo.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
  local x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  if x.xPath('//div[@class="chapter-content"]//img[contains(@src, "&url=")]/@src').count > 0 then
    x.XPathStringAll('//div[@class="chapter-content"]//img/substring-after(@src, "&url=")', task.PageLinks)
  else
	x.XPathStringAll('//div[@class="chapter-content"]//img/@src', task.PageLinks)
  end
  task.PageNumber = task.PageLinks.Count
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
  local x = nil
  local u = module.RootURL .. DirectoryPagination .. 1 .. '/'
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetDirectoryPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  page = tonumber(TXQuery.Create(http.Document).XPathString('(//div[@class="pagination"]/a[contains(@class, "page-numbers")])[last()]/substring-after(@href, "/page-")'))
  
  --[[Debug]] LuaDebug.WriteStatistics('DirectoryPages', page)
  
  return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
  local x = nil
  local u = module.RootURL .. DirectoryPagination .. IncStr(url) .. '/'
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.XPathHREFAll('//div[@class="comics-grid"]/div/div/h3/a', links, names)

  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(IncStr(url))
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('HeavenManga', 'http://www.heaventoon.com', 'English')
  AddWebsiteModule('HolyManga', 'http://w16.holymanga.net', 'English')
end

function AddWebsiteModule(name, url, cat)
  local m = NewModule()
  m.Website                     = name
  m.RootURL                     = url
  m.Category                    = cat
  m.OnGetInfo                   = 'GetInfo'
  m.OnGetNameAndLink            = 'GetNameAndLink'
  m.OnGetPageNumber             = 'GetPageNumber'
  m.OnGetDirectoryPageNumber    = 'GetDirectoryPageNumber'
  return m
end

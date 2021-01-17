----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}
function Init() end


----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/comics?page='


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function _M.GetInfo()
  local n, v, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = x.XPathString('//meta[@property="og:title"]/@content')
  mangainfo.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
  mangainfo.Summary   = x.XPathString('//meta[@property="og:description"]/@content')
  
  v = x.XPath('//a[contains(@class, "item-author")]')
  n = x.XPath('//span[contains(@class, "text-muted")]')
  for i = 1, v.Count do
    mangainfo.ChapterNames.Add('Chapter ' .. n.Get(i).ToString .. ' - ' .. v.Get(i).ToString)
    mangainfo.ChapterLinks.Add(v.Get(i).GetAttribute('href'))
  end
  InvertStrings(mangainfo.ChapterLinks, mangainfo.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end


-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
  local u = module.RootURL .. DirectoryPagination .. 1
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetDirectoryPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  page = tonumber(TXQuery.Create(http.Document).XPathString('(//ul[@class="pagination"])[last()]//a[@class="page-link" and not(@rel)]/text()'))
  
  --[[Debug]] LuaDebug.WriteStatistics('DirectoryPages', page)
  
  return no_error
end


-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
  local x = nil
  local u = module.RootURL .. DirectoryPagination .. IncStr(url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.XPathHREFAll('//div[@id="content"]//a[contains(@class, "list-title")]', links, names)
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(IncStr(url))
  
  return no_error
end


-- Get the page count for the current chapter.
function _M.GetPageNumber()
  local v, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.ParseHTML(GetBetween('window.chapterPages = ', ';', x.XPathString('//script[contains(., "window.chapterPages = ")]')):gsub('\\/', '/'))
  v = x.XPath('json(*)()')
  for i = 1, v.Count do
    task.PageLinks.Add(MaybeFillHost(module.RootURL, v.Get(i).ToString))
  end
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M

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

DirectoryPagination = '/browse?sort=title&page='


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function _M.GetInfo()
  local lang, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = x.XPathString('//h3[@class="item-title"]/a') .. _M.GetLanguageCodeSuffix(x.XPathString('//h3[@class="item-title"]/parent::*/span[contains(@class, "flag")]/@class'))
  mangainfo.CoverLink = x.XPathString('//div[contains(@class, "attr-cover")]/img/@src')
  mangainfo.Authors   = x.XPathStringAll('//div[@class="attr-item" and contains(b, "Authors")]/span')
  mangainfo.Genres    = _M.GetGenres(x.XPathString('//div[@class="attr-item" and contains(b, "Genres")]/span'))
  mangainfo.Summary   = MangaInfoStatusIfPos(x.XPathString('//div[@class="attr-item" and contains(b, "Status")]/span'))
  
  x.XPathHREFAll('//div[contains(@class, "chapter-list")]/div[@class="main"]/div/a', mangainfo.ChapterLinks, mangainfo.ChapterNames)
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
  
  page = tonumber(TXQuery.Create(http.Document).XPathString('(//ul[contains(@class, "pagination")])[1]/li[last()-1]'))
  
  --[[Debug]] LuaDebug.WriteStatistics('DirectoryPages', page)
  
  return no_error
end


-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
  local v, x = nil
  local u = module.RootURL .. DirectoryPagination .. IncStr(url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  v = x.XPath('//div[@id="series-list"]/div/div')
  for i = 1, v.Count do
    links.Add(x.XPathString('a/@href', v.Get(i)))
    names.Add(x.XPathString('a', v.Get(i)) .. _M.GetLanguageCodeSuffix(x.XPathString('span[contains(@class, "flag")]/@class', v.Get(i))))
  end
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(IncStr(url))
  
  return no_error
end


-- Get the page count for the current chapter.
function _M.GetPageNumber()
  local s, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.ParseHTML(GetBetween('var images = ', ';', x.XPathString('//script[contains(., "var images = ")]')))
  x.XPathStringAll('let $c := json(*) return for $k in jn:keys($c) return $c($k)', task.PageLinks)
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Get the language suffix by given flag.
function _M.GetLanguageCodeSuffix(s)
  local suffix = ' [EN]'
  
  if s ~= '' then
    s = RegExprGetMatch('flag_(\\w+)\\b', s, 1)
    if s ~= 'united_kingdom' then suffix = ' [' .. string.upper(s) .. ']' end
  end
  
  return suffix
end

-- Get the genre list as string.
function _M.GetGenres(s)
  local genres = ''
  
  for i in string.gmatch(s, '([^/]+)') do
    if genres == '' then genres = genres .. (Trim(i)) else genres = genres .. ', ' .. (Trim(i)) end
  end
  
  return genres
end


----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
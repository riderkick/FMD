----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/page/'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = x.XPathString('//meta[@property="og:title"]/@content')
  mangainfo.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
  mangainfo.Artists   = x.XPathStringAll('//div[@class="entry-wrap"]//a[contains(@href, "/artist/")]')
  mangainfo.Genres    = x.XPathStringAll('//div[@class="entry-wrap"]//a[contains(@href, "/tag/")]')
  mangainfo.ChapterLinks.Add(x.XPathString('//a[contains(@title, "Read")]/@href'))
  mangainfo.ChapterNames.Add(mangainfo.Title)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end


-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
  local u = module.RootURL
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetDirectoryPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  page = tonumber(TXQuery.Create(http.Document).XPathString('//a[@class="last" and @title="Last Page"]'))
  
  --[[Debug]] LuaDebug.WriteStatistics('DirectoryPages', page)
  
  return no_error
end


-- Get links and names from the manga list of the current website.
function GetNameAndLink()
  local x = nil
  local u = module.RootURL .. DirectoryPagination .. IncStr(url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.XPathHREFAll('//h2[@class="entry-title"]/a', links, names)
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(IncStr(url))
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  local s, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.ParseHTML(GetBetween('var pages = ', ';', x.XPathString('//script[contains(., "var pages = ")]')):gsub('\\/', '/'))
  x.XPathStringAll('json(*)().url', task.PageLinks)
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('HentaiCafe', 'https://hentai.cafe', 'H-Sites')
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.Website                  = name
  m.RootURL                  = url
  m.Category                 = category
  m.FavoriteAvailable        = false
  m.OnGetInfo                = 'GetInfo'
  m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
  return m
end
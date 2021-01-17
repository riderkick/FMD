----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/webtoons/page/'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local n, v, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = x.XPathString('//*[@class="entry-title"]')
  mangainfo.CoverLink = x.XPathString('//*[@class="post-thumbnail"]/img/@data-src')
  mangainfo.Authors   = x.XPathString('//*[@class="author"]//a')
  mangainfo.Genres    = x.XPathStringAll('//*[@class="tags"]/a')
  mangainfo.Summary   = x.XPathString('//*[contains(@class, "entry-content")]')
  
  v = x.XPath('//*[@class="chapter-list-items"]/a')
  n = x.XPath('//*[@class="chapter-name"]')
  for i = 1, v.Count do
    mangainfo.ChapterNames.Add(n.Get(i).ToString)
    mangainfo.ChapterLinks.Add(v.Get(i).GetAttribute('href'))
  end
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end


-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
  local u = module.RootURL .. DirectoryPagination .. 1
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetDirectoryPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  page = tonumber(TXQuery.Create(http.Document).XPathString('(//span[@class="pages"])/substring-after(., "Page 1 of ")'))
  
  --[[Debug]] LuaDebug.WriteStatistics('DirectoryPages', page)
  
  return no_error
end


-- Get links and names from the manga list of the current website.
function GetNameAndLink()
  local n, v, x = nil
  local u = module.RootURL .. DirectoryPagination .. IncStr(url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  v = x.XPath('//*[contains(@rel, "bookmark")]')
  n = x.XPath('//*[contains(@class, "entry-title")]')
  for i = 1, v.Count do
    links.Add(v.Get(i).GetAttribute('href'))
    names.Add(n.Get(i).ToString)
  end
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(IncStr(url))
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  local x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.XPathStringAll('//*[@class="container"]//img/@src', task.PageLinks)
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('LewdManhwa', 'https://lewdmanhwa.com', 'H-Sites')
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.Website                  = name
  m.RootURL                  = url
  m.Category                 = category
  m.OnGetInfo                = 'GetInfo'
  m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
  return m
end

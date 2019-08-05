----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}
function Init() end


----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'Modules.LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryParameters = '/changeMangaList?type=text'
XPathTokenStatus    = 'Status'
XPathTokenGenres    = 'Categories'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function _M.GetInfo()
  local x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = x.XPathString('(//div[contains(@class, "container")]//h2)[1]')
  mangainfo.CoverLink = x.XPathString('//div[@class="boxed"]/img/@src')
  mangainfo.Status    = MangaInfoStatusIfPos(x.XPathString('//dt[text()="' .. XPathTokenStatus .. '"]/following-sibling::dd[1]/span'), 'Ongoing', 'Complete')
  mangainfo.Authors   = x.XPathStringAll('//dt[text()="Author(s)"]/following-sibling::dd[1]/a')
  mangainfo.Artists   = x.XPathStringAll('//dt[text()="Artist(s)"]/following-sibling::dd[1]/a')
  mangainfo.Genres    = x.XPathStringAll('//dt[text()="' .. XPathTokenGenres .. '"]/following-sibling::dd[1]/a')
  mangainfo.Summary   = x.XPathString('//div[contains(@class, "well")]/p')
  x.XPathHREFAll('//ul[@class="chapters"]/li/h5/a', mangainfo.ChapterLinks, mangainfo.ChapterNames)
  InvertStrings(mangainfo.ChapterLinks, mangainfo.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end


-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
  local v, x = nil
  local u = module.RootURL .. DirectoryParameters
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  v = x.XPath('//a[@class="alpha-link"]')
  for i = 1, v.Count do
    links.Add(v.Get(i).GetAttribute('href'))
    names.Add(x.XPathString('h6', v.Get(i)))
  end
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(1)
  
  return no_error
end


-- Get the page count for the current chapter.
function _M.GetPageNumber()
  local s, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.XPathStringAll('//div[@id="all"]//img/@data-src', task.PageLinks)
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local AlphaList = '#abcdefghijklmnopqrstuvwxyz'
local DirectoryPagination = '/manga-list/'
local LoadAllImages = '/all-pages'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local v, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = x.XPathString('(//h5[@class="widget-heading"])[1]')
  mangainfo.CoverLink = x.XPathString('//div[@class="content"]//img/@src')
  mangainfo.Status    = x.XPathString('//dt[contains(., "Status")]/following-sibling::dd[1]')
  mangainfo.Authors   = x.XPathString('//dt[contains(., "Author")]/following-sibling::dd[1]')
  mangainfo.Artists   = x.XPathString('//dt[contains(., "Artist")]/following-sibling::dd[1]')
  mangainfo.Genres    = x.XPathStringAll('//dt[contains(., "Categories")]/following-sibling::dd[1]/a')
  mangainfo.Summary   = Trim(x.XPathString('//div[contains(@class, "note")]'))
  
  v = x.XPath('//div[@id="chapter_list"]/ul/li/a')
  for i = 1, v.Count do
    mangainfo.ChapterLinks.Add(v.Get(i).GetAttribute('href'))
    mangainfo.ChapterNames.Add(x.XPathString('span[1]', v.Get(i)))
  end
  InvertStrings(mangainfo.ChapterLinks, mangainfo.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end


-- Get links and names from the manga list of the current website.
function GetNameAndLink()
  local x = nil
  local u = module.RootURL .. DirectoryPagination .. AlphaList:sub(module.CurrentDirectoryIndex + 1, module.CurrentDirectoryIndex + 1)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.XPathHREFAll('//ul[contains(@class, "manga-list")]/li/a', links, names)
  updatelist.CurrentDirectoryPageNumber = 1
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(module.CurrentDirectoryIndex + 1)
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  local s, x = nil
  local u = MaybeFillHost(module.RootURL, url) .. LoadAllImages
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.ParseHTML(GetBetween('var images = ', ';', x.XPathString('//script[contains(., "var images = ")]')):gsub('\\/', '/'))
  x.XPathStringAll('json(*)().url', task.PageLinks)
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


-- Prepare the url, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
  http.Headers.Values['referer'] = module.RootURL
  
  return true
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('MangaInn', 'http://www.mangainn.net', 'English')
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.Website               = name
  m.RootURL               = url
  m.Category              = category
  m.TotalDirectory        = AlphaList:len()
  m.OnGetInfo             = 'GetInfo'
  m.OnGetNameAndLink      = 'GetNameAndLink'
  m.OnGetPageNumber       = 'GetPageNumber'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
  return m
end
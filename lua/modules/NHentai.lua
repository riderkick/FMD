----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/?page='


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
  mangainfo.Title     = x.XPathString('//h1')
  if module.website == 'NHentai' then
    mangainfo.CoverLink = x.XPathString('//div[@id="cover"]//img/@data-src')
    mangainfo.Artists   = x.XPathStringAll('//*[@class="tags"]/a[contains(@href, "artist")]/*[@class="name"]')
    mangainfo.Genres    = x.XPathStringAll('//*[@class="tags"]/a[contains(@href, "tag")]/*[@class="name"]')
  else
    mangainfo.CoverLink = x.XPathString('//div[@id="cover"]//img/@src')
    mangainfo.Artists   = x.XPathStringAll('//section[@id="tags"]//a[contains(@href, "artists")]/text()')
    mangainfo.Genres    = x.XPathStringAll('//section[@id="tags"]//a[contains(@href, "tags")]/text()')
    mangainfo.Summary   = x.XPathString('//div[contains(@class, "drop-discription")]/p/text()')
  end
   
  mangainfo.ChapterLinks.Add(url)
  mangainfo.ChapterNames.Add(mangainfo.Title)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')

  
  return no_error
end


-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
  local u = module.RootURL .. DirectoryPagination .. 1
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetDirectoryPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  if module.website == 'NHentai' then
    page = tonumber(TXQuery.Create(http.Document).XPathString('//a[@class="last"]/@href/substring-after(.,"=")'))
  else
    page = tonumber(TXQuery.Create(http.Document).XPathString('//section[@class="pagination"]/li[last()]/a/@href'):match('?page=(%d+)&order='))
  end
  
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
  x.XPathHREFAll('//div[@id="content"]/div/div[@class="gallery"]/a', links, names)
  
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
  x.XPathStringAll('//a[@class="gallerythumb"]/@href', task.PageContainerLinks)
  task.PageNumber = task.PageContainerLinks.Count
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageContainerLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
  local u = MaybeFillHost(module.RootURL, task.PageContainerLinks[workid])
  
  if http.Get(u) then
    task.PageLinks[workid] = TXQuery.Create(http.document).XPathString('//section[@id="image-container"]//img/@src')
    return true
  end
  
  return false
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('NHentai', 'https://nhentai.net', 'H-Sites')
  AddWebsiteModule('HentaiHand', 'https://hentaihand.com', 'H-Sites')
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
  m.OnGetImageURL            = 'GetImageURL'
  m.SortedList               = True
  return m
end

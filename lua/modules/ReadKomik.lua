----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/p/blog-page.html'


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
  mangainfo.Title     = Trim(x.XPathString('//title/substring-before(., " - ReadKomik")'))
  if mangainfo.Title  == "" then mangainfo.Title = Trim(x.XPathString('//title/substring-after(., "ReadKomik: ")')) end
  mangainfo.CoverLink = x.XPathString('//div[@class="kerangka-gambar"]//a/@href')
  mangainfo.Status    = MangaInfoStatusIfPos(x.XPathString('//td[contains(., "Status")]/following-sibling::td'))
  mangainfo.Genres    = x.XPathString('//td[contains(., "Genre")]/following-sibling::td')
  mangainfo.Summary   = x.XPathString('//div[@class="isi-sinopsis"]')
  
  for _, v in ipairs(x.XPathI('//*[@dir="ltr" or @class="tabel-chapter"]/a')) do
    mangainfo.ChapterNames.Add(x.XPathString('span', v))
    mangainfo.ChapterLinks.Add(v.GetAttribute('href'))
  end
  if mangainfo.ChapterLinks.count < 1 then
    x.XPathHREFAll('//div[contains(@itemprop, "description")]/a', mangainfo.ChapterLinks, mangainfo.ChapterNames)
  end
  if mangainfo.ChapterLinks.count < 1 then
    x.XPathHREFAll('//h2[@class="entry-title"]/a', mangainfo.ChapterLinks, mangainfo.ChapterNames)
  end
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end


-- Get links and names from the manga list of the current website.
function GetNameAndLink()
  local x = nil
  local u = module.RootURL .. DirectoryPagination
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.XPathHREFAll('//div[contains(@itemprop, "description")]/a', links, names)
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(1)
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  local x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.XPathStringAll('//div[@class="separator"]/a/@href', task.PageLinks)
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('ReadKomik', 'https://www.readkomik.com', 'Webcomics')
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.Website                  = name
  m.RootURL                  = url
  m.Category                 = category
  m.OnGetInfo                = 'GetInfo'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
  return m
end

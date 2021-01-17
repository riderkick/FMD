----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/backend/ajax/searchengine.php'
local DirectoryParameters = 'contentType=manga&retrieveCategories=true&retrieveAuthors=true&perPage=18&page='


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local c, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.url       = x.XPathString('//meta[@property="og:url"]/@content')
  mangainfo.Title     = x.XPathString('//meta[@property="og:title"]/@content')
  mangainfo.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
  mangainfo.Status    = MangaInfoStatusIfPos(x.XPathString('//*[@class="infom"]/div/div[2]//p[contains(.,"Estado")]'), 'Activo', 'Finalizado')
  mangainfo.Authors   = x.XPathString('//*[@class="infom"]/div/div[2]//p[contains(.,"Autor") and not(contains(.,"No Disponible"))]/substring-after(normalize-space(.),": ")')
  mangainfo.Artists   = x.XPathString('//*[@class="infom"]/div/div[2]//p[contains(.,"Artist") and not(contains(.,"No Disponible"))]/substring-after(normalize-space(.),": ")')
  mangainfo.Genres    = x.XPathString('//*[@class="panel-footer" and contains(.,"Géneros")]/string-join(.//a,", ")')
  mangainfo.Summary   = x.XPathString('//meta[@property="og:description"]/@content')
  
  x.XPathHREFAll('//table[contains(@class, "table")]//h4[@class="title"]/a', mangainfo.ChapterLinks, mangainfo.ChapterNames)
  c = GetPageChapterListPageCount(x.XPathString('//script[contains(., "php_pagination")]'))
  
  if c > 1 then
    for i = 2, c do
      if http.Get(mangainfo.url .. '/p/' .. i) then
        x = TXQuery.Create(http.Document)
        x.XPathHREFAll('//table[contains(@class, "table")]//h4[@class="title"]/a', mangainfo.ChapterLinks, mangainfo.ChapterNames)
      end
    end
  end
  
  InvertStrings(mangainfo.ChapterLinks, mangainfo.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end


-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
  local u = module.RootURL .. DirectoryPagination
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetDirectoryPageNumber', 'url ->  ' .. u .. '   (Params: [' .. DirectoryParameters .. '])')
  if not http.Post(u, DirectoryParameters .. '1') then return net_problem end
  
  page = math.ceil(tonumber(TXQuery.Create(http.Document).XPathString('json(*).totalContents')) / 18)
  
  --[[Debug]] LuaDebug.WriteStatistics('DirectoryPages', page)
  
  return no_error
end


-- Get links and names from the manga list of the current website.
function GetNameAndLink()
  local c, x = nil
  local u = module.RootURL .. DirectoryPagination
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u .. '   (Params: [' .. DirectoryParameters .. '])')
  if not http.Post(u, DirectoryParameters .. IncStr(url)) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  c = x.XPathCount('json(*).contents()')
  
  for i = 1, c do
    names.Add(x.XPathString('json(*).contents(' .. i .. ').name'))
    links.Add(x.XPathString('json(*).contents(' .. i .. ')/concat("/manga/",id,"/",slug)'))
  end
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(DirectoryParameters .. IncStr(url))
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  local c, p, x = nil
  local u = MaybeFillHost(module.RootURL, url:gsub('/c/', '/leer/'))
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  c = tonumber(x.XPathString('//script[contains(., "konekomangareader")]'):match('\'pages\':(.-),'))
  p = x.XPathString('//script[contains(., "konekomangareader")]'):match('\'pageFormat\':\'(.-)\',')
  
  for i = 1, c do
    task.PageLinks.Add(p:gsub('{pnumber}', tostring(i)))
  end
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Get count of chapter list pages.
function GetPageChapterListPageCount(s)
  local count = 1
  
  s = GetBetween('php_pagination(', ');', s)
  count = math.ceil(tonumber(s:match('.-,.-,.-,.-,(.-),.-,.-')) / tonumber(s:match('.-,.-,.-,.-,.-,(.-),.-')))
  
  return count
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('KuManga', 'http://www.kumanga.com', 'Spanish')
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.Website                  = name
  m.RootURL                  = url
  m.Category                 = category
  m.OnGetInfo                = 'GetInfo'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
  m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
  return m
end
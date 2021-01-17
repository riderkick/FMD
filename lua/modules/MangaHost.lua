----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/mangas/page/'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local t, v, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = x.XPathString('//meta[@property="og:title"]/@content')
  mangainfo.CoverLink = x.XPathString('//img[contains(@class, "thumbnail")]/@src')
  mangainfo.Status    = MangaInfoStatusIfPos(x.XPathString('//li[contains(strong, "Status:")]/text()'), 'Ativo', 'Completo')
  mangainfo.Authors   = x.XPathString('//li[contains(strong, "Autor:")]/text()')
  mangainfo.Artists   = x.XPathString('//li[contains(strong, "Desenho (Art):")]/text()')
  mangainfo.Genres    = x.XPathStringAll('//li[contains(strong, "Categoria(s):")]/a')
  mangainfo.Summary   = Trim(x.XPathString('//div[@id="divSpdInText"]/p[1]'))
  
  if x.XPathCount('//ul[@class="list_chapters"]') > 0 then
    v = x.XPath('//ul[@class="list_chapters"]//a')
    for i = 1, v.Count do
      t = TXQuery.New()
      t.ParseHTML(v.Get(i).GetAttribute('data-content'))
      mangainfo.ChapterLinks.Add(t.XPathString('//a/@href'))
      mangainfo.ChapterNames.Add(v.Get(i).GetAttribute('title'))
    end
  else
    x.XPathHREFAll('//a[@class="capitulo"]', mangainfo.ChapterLinks, mangainfo.ChapterNames)
  end
  InvertStrings(mangainfo.ChapterLinks, mangainfo.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end


-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
  local u = module.RootURL .. DirectoryPagination .. 1
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetDirectoryPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  page = tonumber(TXQuery.Create(http.Document).XPathString('(//div[@class="wp-pagenavi"])[1]//a[@class="last"]/@href'):match('.-/page/(%d+)'))
  
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
  x.XPathHREFAll('//div[@class="list clearfix"]//h3/a', links, names)
  
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
  x.ParseHTML(GetBetween('var images = ["', '"];', x.XPathString('//script[contains(., "var images = ")]')):gsub('"', ''):gsub('\'', '"'))
  x.XPathStringAll('//img/@src', task.PageLinks)
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('MangaHost', 'https://mangahosted.com', 'Portuguese')
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
----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template   = require 'templates.MangaReaderOnline'
-- DirectoryParameters = '/'            --> Override template variable by uncommenting this line.
XPathTokenStatus    = 'Estado'
-- XPathTokenAuthors   = 'Author(s)'    --> Override template variable by uncommenting this line.
-- XPathTokenArtists   = 'Artist(s)'    --> Override template variable by uncommenting this line.
XPathTokenGenres    = 'Categorías:'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  Template.GetInfo()
  local x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = x.XPathString('(//section[contains(@class, "container")]//h3)[1]'):gsub('%(Manga%)', '')
  mangainfo.CoverLink = x.XPathString('//div[@class="list-group"]//img/@src')
  mangainfo.Status    = MangaInfoStatusIfPos(x.XPathString('normalize-space(//div[@class="content-wrapper"]//span[contains(b, "' .. XPathTokenStatus .. '")])'):gsub('Estado: ', ''), 'Ongoing', 'Complete')
  mangainfo.Genres    = x.XPathStringAll('//b[text()="' .. XPathTokenGenres .. '"]/following-sibling::a')
  mangainfo.Summary   = x.XPathString('normalize-space(//div[@class="content-wrapper"]//span[contains(b, "Resumen")])'):gsub('Resumen Resumen: ', '')
  
  x.XPathHREFAll('//table[contains(@class, "table")]//a', mangainfo.ChapterLinks, mangainfo.ChapterNames)
  InvertStrings(mangainfo.ChapterLinks, mangainfo.ChapterNames)
  
  return no_error
end


-- Get links and names from the manga list of the current website.
function GetNameAndLink()
  Template.GetNameAndLink()
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  Template.GetPageNumber()
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('LeoManga', 'https://leomanga.me', 'Spanish')
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
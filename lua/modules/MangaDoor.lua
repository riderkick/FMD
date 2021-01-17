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
XPathTokenAuthors   = 'Autor(es)'
-- XPathTokenArtists   = 'Artist(s)'    --> Override template variable by uncommenting this line.
XPathTokenGenres    = 'Categorías'


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
  mangainfo.Status    = MangaInfoStatusIfPos(x.XPathString('//dt[text()="' .. XPathTokenStatus .. '"]/following-sibling::dd[1]/span'), 'En curso', 'Completa')
  mangainfo.Artists   = x.XPathString('//dt[text()="' .. XPathTokenArtists .. '"]/following-sibling::dd[1]')
  
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
  AddWebsiteModule('MangaDoor', 'http://mangadoor.com', 'Spanish')
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
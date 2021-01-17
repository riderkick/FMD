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
-- XPathTokenStatus    = 'Status'       --> Override template variable by uncommenting this line.
XPathTokenAuthors   = 'Autor'
XPathTokenArtists   = 'Artista'
XPathTokenGenres    = 'Género'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  Template.GetInfo()
  
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
  AddWebsiteModule('MangAs', 'https://mangas.in', 'Spanish')
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
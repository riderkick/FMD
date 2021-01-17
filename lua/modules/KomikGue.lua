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
-- XPathTokenAuthors   = 'Author(s)'    --> Override template variable by uncommenting this line.
-- XPathTokenArtists   = 'Artist(s)'    --> Override template variable by uncommenting this line.
XPathTokenGenres    = 'Genre'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  Template.GetInfo()
  local v, x = nil
  local u = MaybeFillHost(module.RootURL, url)

  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = x.XPathString('(//div[contains(@class, "container")]//h1)[1]')
  mangainfo.Status    = MangaInfoStatusIfPos(x.XPathString('//dt[text()="' .. XPathTokenStatus .. '"]/following-sibling::dd[1]//span'), 'Ongoing', 'Complete')
  mangainfo.Authors   = x.XPathStringAll('//dt[text()="' .. XPathTokenAuthors .. '"]/following-sibling::dd[1]//a')
  mangainfo.Artists   = x.XPathString('//dt[text()="' .. XPathTokenArtists .. '"]/following-sibling::dd[1]')
  mangainfo.Summary   = x.XPathString('//div[contains(@class, "well")]/div')
  
  v = x.XPath('//div[@class="chapter-wrapper"]/table//td[@class="chapter"]/a')
  for i = 1, v.Count do
    mangainfo.ChapterLinks.Add(v.Get(i).GetAttribute('href'))
    mangainfo.ChapterNames.Add(x.XPathString('normalize-space(.)', v.Get(i)))
  end
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
  AddWebsiteModule('KomikGue', 'http://www.komikgue.com', 'Indonesian')
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
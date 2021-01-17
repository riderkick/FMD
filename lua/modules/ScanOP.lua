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
XPathTokenStatus    = 'Statut'
-- XPathTokenAuthors   = 'Author(s)'    --> Override template variable by uncommenting this line.
-- XPathTokenArtists   = 'Artist(s)'    --> Override template variable by uncommenting this line.
-- XPathTokenGenres    = 'Categories'   --> Override template variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local v, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = x.XPathString('(//div[contains(@class, "container")]//h2)[1]')
  mangainfo.CoverLink = x.XPathString('//div[@class="boxed"]/img/@src')
  mangainfo.Status    = MangaInfoStatusIfPos(x.XPathString('//dt[text()="' .. XPathTokenStatus .. '"]/following-sibling::dd[1]/span'), 'En cours', 'Complete')
  mangainfo.Authors   = x.XPathStringAll('//dt[text()="' .. XPathTokenAuthors .. '"]/following-sibling::dd[1]/a')
  mangainfo.Artists   = x.XPathStringAll('//dt[text()="' .. XPathTokenArtists .. '"]/following-sibling::dd[1]/a')
  mangainfo.Genres    = x.XPathStringAll('//dt[text()="' .. XPathTokenGenres .. '"]/following-sibling::dd[1]/a')
  mangainfo.Summary   = x.XPathString('//div[contains(@class, "well")]/p')

  for _, v in ipairs(x.XPathI('//ul[@class="chapters"]/li/h5')) do
    if x.XPathString('normalize-space(.)', v):find('RAW') then
      if module.getoption('luaincluderaw') then
        mangainfo.ChapterLinks.Add(x.XPathString('a/@href', v))
        mangainfo.ChapterNames.Add(x.XPathString('normalize-space(.)', v))
      end
    else
      mangainfo.ChapterLinks.Add(x.XPathString('a/@href', v))
      mangainfo.ChapterNames.Add(x.XPathString('normalize-space(.)', v))
    end
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
  AddWebsiteModule('ScanOP', 'https://www.scan-op.com', 'French')
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.Website                  = name
  m.RootURL                  = url
  m.Category                 = category
  m.OnGetInfo                = 'GetInfo'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
  
  m.addoptioncheckbox('luaincluderaw', 'Show [RAW] chapters', false)
  return m
end
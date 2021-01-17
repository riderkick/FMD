----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/truyen/all?page='


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
  mangainfo.Title     = x.XPathString('//meta[@property="og:title"]/@content')
  mangainfo.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
  mangainfo.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="__info"]/p[starts-with(.,"Tình trạng:")]'), 'Đang tiến hành', 'Ngưng')
  mangainfo.Authors   = SeparateRight(x.XPathString('//div[@class="__info"]/p[starts-with(.,"Tác giả:")]'), ':')
  mangainfo.Genres    = Trim(SeparateRight(x.XPathString('//div[@class="__info"]/p[starts-with(.,"Thể loại:")]'), ':'))
  mangainfo.Summary   = x.XPathString('//meta[@property="og:description"]/@content')
  
  x.XPathHREFAll('//div[@class="table-scroll"]/table/tbody/tr/td[1]/a', mangainfo.ChapterLinks, mangainfo.ChapterNames)
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
  
  page = tonumber(TXQuery.Create(http.Document).XPathString('(//*[starts-with(@class,"pagination")]//a)[last()-1]'))
  
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
  x.XPathHREFAll('//table[1]/tbody/tr/td[1]/a', links, names)
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(IncStr(url))
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  local s, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  TXQuery.Create(http.Document).XPathStringAll('//*[@class="manga-container"]/img/@src', task.PageLinks)
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('AcademyVN', 'https://hocvientruyentranh.net', 'Vietnamese')
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
----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/category/index_'
DirectorySuffix     = '.html'
MangaInfoParameters = '?waring=1'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local x = nil
  local u = MaybeFillHost(module.RootURL, url) .. MangaInfoParameters
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = Trim(SeparateLeft(x.XPathString('//div[@class="book-info"]/h1'), ' Manga'))
  mangainfo.CoverLink = x.XPathString('//div[@class="book-info"]//img/@src')
  mangainfo.Authors   = x.XPathString('string-join(//dd[@class="about-book"]//span[starts-with(.,"Author")]/following-sibling::a)')
  mangainfo.Genres    = x.XPathString('string-join(//ul[@class="inset-menu"]/li/a[not(contains(.,"Manga Reviews"))], ", ")')
  mangainfo.Status    = MangaInfoStatusIfPos(x.XPathString('//dd[@class="about-book"]//span[starts-with(.,"Status")]/following-sibling::a'));
  mangainfo.Summary   = x.XPathString('//dd[@class="short-info"]//span')
  
  for _, v in ipairs(x.XPathI('//ul[@class="chapter-box"]/li//a')) do
    mangainfo.ChapterNames.Add(x.XPathString('text()[not(parent::span/@class="new_up")]', v))
    mangainfo.ChapterLinks.Add(v.GetAttribute('href'))
  end
  InvertStrings(mangainfo.ChapterLinks, mangainfo.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end


-- Get links and names from the manga list of the current website.
function GetNameAndLink()
  local v, x = nil
  local u = module.RootURL .. DirectoryPagination .. IncStr(url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  if x.XPath('//dd[@class="book-list"]/a[not(@class="follow")]').Count == 0 then return no_error end
  for _, v in ipairs(x.XPathI('//dd[@class="book-list"]/a[not(@class="follow")]')) do
    links.Add(v.GetAttribute('href'))
    names.Add(x.XPathString('b', v))
  end  
  updatelist.CurrentDirectoryPageNumber = updatelist.CurrentDirectoryPageNumber + 1
  
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
  x.XPathStringAll('(//select[@class="sl-page"])[last()]/option/@value', task.PageContainerLinks)
  task.PageNumber = task.PageContainerLinks.Count
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
  local u = MaybeFillHost(module.RootURL, task.PageContainerLinks[workid])
  
  if http.Get(u) then
    task.PageLinks[workid] = TXQuery.Create(http.document).XPathString('//img[contains(@class,"manga_pic")]/@src')
    return true
  end
  
  return false
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('MangaDeep', 'http://www.mangadeep.com', 'English')
  AddWebsiteModule('Manga99', 'http://www.manga99.com', 'English')
  AddWebsiteModule('TenManga', 'http://www.tenmanga.com', 'English')
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.Website                  = name
  m.RootURL                  = url
  m.Category                 = category
  m.OnGetInfo                = 'GetInfo'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
  m.OnGetImageURL            = 'GetImageURL'
  return m
end

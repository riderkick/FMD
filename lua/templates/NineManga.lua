----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}
function Init() end


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
function _M.GetInfo()
  local s, x = nil
  local u = MaybeFillHost(module.RootURL, url) .. MangaInfoParameters
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = x.XPathString('//div[@class="manga"]/div[@class="ttline"]/h1')
  mangainfo.CoverLink = x.XPathString('//a[@class="bookface"]/img/@src')
  mangainfo.Authors   = x.XPathString('//ul[@class="message"]/li[starts-with(.,"Author")]/string-join(a,", ")')
  mangainfo.Genres    = x.XPathString('//ul[@class="message"]/li[starts-with(.,"Genre")]/string-join(a,", ")')
  mangainfo.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="message"]/li[starts-with(.,"Status")]/a[1]'));
  mangainfo.Summary   = Trim(x.XPathString('//p[@itemprop="description"]/substring-after(.,":")'))
  
  s = mangainfo.Title:match('^(.*) Manga$')
  if s ~= nil and s ~= '' then mangainfo.Title = s end
  
  x.XPathHREFAll('//div[@class="chapterbox"]//li/a', mangainfo.ChapterLinks, mangainfo.ChapterNames)
  InvertStrings(mangainfo.ChapterLinks, mangainfo.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end


-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
  local x = nil
  local u = module.RootURL .. DirectoryPagination .. IncStr(url) .. DirectorySuffix
    
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.XPathHREFAll('//dl[@class="bookinfo"]//dd/a[@class="bookname"]', links, names)
  
  if tonumber(IncStr(url)) < tonumber(x.XPathString('(//ul[@class="pagelist"]/li[last()-1])[1]')) then
    updatelist.CurrentDirectoryPageNumber = tonumber(IncStr(url)) + 1
  end
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(IncStr(url))
  
  return no_error
end


-- Get the page count for the current chapter.
function _M.GetPageNumber()
  local x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.XPathStringAll('(//select[@id="page"])[last()]/option/@value', task.PageContainerLinks)
  task.PageNumber = task.PageContainerLinks.Count
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageContainerLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


-- Extract/Build/Repair image urls before downloading them.
function _M.GetImageURL()
  local u = MaybeFillHost(module.RootURL, task.PageContainerLinks[workid])
  
  if http.Get(u) then
    task.PageLinks[workid] = TXQuery.Create(http.document).XPathString('//img[contains(@class,"manga_pic")]/@src')
    return true
  end
  
  return false
end


----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
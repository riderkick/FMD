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

DirectoryPagination = '/list?sortType=created'
DirectoryParameters = '&offset='
DirectoryOffset     = 70


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function _M.GetInfo()
  local rtitle = ''
  local x, v = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  rtitle              = x.XPathString('//h1[@class="names"]/span[@class="name"]')
  mangainfo.Title     = x.XPathString('//h1[@class="names"]/span[@class="eng-name"]')
  mangainfo.CoverLink = x.XPathString('//div[@class="picture-fotorama"]/img/@src')
  mangainfo.Authors   = x.XPathStringAll('//p[@class="elementList"]/span[contains(@class, "elem_author")]/a[@class="person-link"]/text()|//p[@class="elementList"]/span[contains(@class, "elem_screenwriter")]/a[@class="person-link"]/text()')
  mangainfo.Artists   = x.XPathStringAll('//p[@class="elementList"]/span[contains(@class, "elem_illustrator")]/a[@class="person-link"]/text()')
  mangainfo.Genres    = x.XPathStringAll('//p[@class="elementList"]/span[contains(@class, "elem_genre")]/a/text()|//p[@class="elementList"]/span[contains(@class, "elem_tag")]/a/text()')
  mangainfo.Summary   = x.XPathString('//div[@class="manga-description"]')
  
  if mangainfo.Title == '' then mangainfo.Title = rtitle end
  if Pos('продолжается', x.XPathString('//*[starts-with(@class,"subject-meta")]/*[starts-with(.,"Перевод:")]')) > 0 then mangainfo.Status = 1 else mangainfo.Status = 0 end
  
  v = x.XPath('//table[@class="table table-hover"]/tbody/tr/td/a')
  for i = 1, v.Count do
    mangainfo.ChapterLinks.Add(v.Get(i).GetAttribute('href'))
    mangainfo.ChapterNames.Add(v.Get(i).ToString:gsub(rtitle, ''))
  end
  InvertStrings(mangainfo.ChapterLinks, mangainfo.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end


-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
  local u = module.RootURL .. DirectoryPagination
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetDirectoryPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  page = tonumber(TXQuery.Create(http.Document).XPathString('(//span[@class="pagination"])[last()]/a[@class="step"][last()]'))
  
  --[[Debug]] LuaDebug.WriteStatistics('DirectoryPages', page)
  
  return no_error
end


-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
  local v, x = nil
  local u = module.RootURL .. DirectoryPagination
  
  if url ~= '0' then u = u .. DirectoryParameters .. (DirectoryOffset * tonumber(url)) end
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  x.XPathHREFAll('//div[@class="tiles row"]//div[@class="desc"]/h3/a', links, names)
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(IncStr(url))
  
  return no_error
end


-- Get the page count for the current chapter.
function _M.GetPageNumber()
  local json, x = nil
  local u = MaybeFillHost(module.RootURL, url)
  
  if Pos('mtr=1', url) == 0 then u = u .. '?mtr=1' end
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  json = GetBetween('[[', ', 0, ', Trim(GetBetween('rm_h.init(', 'false);', x.XPathString('//script[@type="text/javascript" and contains(., "rm_h.init")]'))))
  json = json:gsub('%],%[', ';'):gsub('\'', ''):gsub('"', ''):gsub(']]', ';')
  for i in json:gmatch('(.-);') do
    i1, i2 = i:match('(.-),.-,(.-),.-,.-')
    task.PageLinks.Add(i1 .. i2)
  end
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', task.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M

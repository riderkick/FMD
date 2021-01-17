----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/'   --> Override template variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local u = MaybeFillHost(module.RootURL, url)
  
  -- [Debug] LuaDebug.WriteLogWithHeader('GetInfo', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  mangainfo.Title     = x.XPathString('//div[@id="breadcrumbs"]/substring-before(substring-after(., "Start"), "|")'):gsub('^%s*(.-)%s*$', '%1')
  mangainfo.Summary   = x.XPathString('//table[@class="infobox"]//tr[6]//td[2]')

  local v for _,v in ipairs(x.xpathi('//table[@class="list"]//tr[./td/@onclick|./td[2]]')) do
    mangainfo.chapternames.add(x.xpathstring('string-join((td[1],td[2])," ")', v))
    if mangainfo.Title == "Kapitel" then
      -- remove last /1 for quick getimageurl later
      mangainfo.chapterlinks.add(x.xpathstring('td[@onclick]/substring-before(substring-after(@onclick, "\'"),"\'")', v):gsub('/1$',''))
    else
      -- remove last /1 for quick getimageurl later
      mangainfo.chapterlinks.add(x.xpathstring('substring-before(substring-after(@onclick, "\'"),"\'")', v):gsub('/1$',''))
    end
  end

  -- [Debug] LuaDebug.PrintMangaInfo()
  -- [Debug] LuaDebug.WriteStatistics('Chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  
  return no_error
end


-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
  return no_error
end


-- Get links and names from the manga list of the current website.
-- DirectoryPagination = RootURL + Manga List
function GetNameAndLink()
  local v, x = nil
  local u = module.RootURL .. DirectoryPagination
  
  -- [Debug] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'url ->  ' .. u)

  if not http.Get(u) then return net_problem end
  
  x = TXQuery.Create(http.Document)
  v = x.XPath('//div[@id="mangalist"]//a[not(@id="SpinOffOpen")]')

  for i = 1, v.Count do
    links.Add(module.RootURL .. x.XPathString('@href', v.Get(i)))
    names.Add(x.XPathString('text()', v.Get(i)))
  end

  -- [Debug] LuaDebug.PrintMangaDirectoryEntries(u)
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  if http.get(MaybeFillHost(module.rooturl,url) .. '/1') then
    x=TXQuery.Create(http.Document)
    -- get total page number
    task.pagenumber = tonumber(x.xpathstring('//td[@id="tablecontrols"]/a[last()]')) or 0
    -- first page image url
    task.pagelinks.add(x.xpathstring('//img[@id="p"]/@src'))
    return true
  else
    return false
  end
end

function GetImageURL()
  if http.get(AppendURLDelim(MaybeFillHost(module.RootURL, url)) .. IncStr(workid)) then
    task.PageLinks[workid] = TXQuery.Create(http.Document).xpathstring('//img[@id="p"]/@src')
    return true
  else
    return false
  end
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('OnePiece-Tube', 'https://onepiece-tube.com', 'German')
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
  m.OnGetImageURL            = 'GetImageURL'
  return m
end
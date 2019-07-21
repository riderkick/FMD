----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}
function Init() end


----------------------------------------------------------------------------------------------------
-- Global Options
----------------------------------------------------------------------------------------------------

LuaDebugging  = false   --> When enabled, scripts will print more information to the log.
LuaStatistics = false   --> When enabled, scripts will print count results of directory pages, mangas and chapters.


----------------------------------------------------------------------------------------------------
-- Global Functions
----------------------------------------------------------------------------------------------------

-- Writes a log entry if LuaDebugging is enabled.
function _M.WriteLog(text)
  if LuaDebugging then print('[lua][' .. module.Website .. ']  ' .. text) end
end

-- Writes a log entry with a custom header if LuaDebugging is enabled.
function _M.WriteLogWithHeader(header, text)
  if LuaDebugging then print('[lua][' .. module.Website .. '][' .. header .. ']  ' .. text) end
end

-- Writes the info for the current manga into the log.
function _M.PrintMangaInfo()
  if LuaDebugging then
    _M.WriteLog('Title:    ' .. mangainfo.Title)
    _M.WriteLog('Cover:    ' .. mangainfo.CoverLink)
    _M.WriteLog('Status:   ' .. mangainfo.Status)
    _M.WriteLog('Summary:  ' .. mangainfo.Summary)
    _M.WriteLog('Authors:  ' .. mangainfo.Authors)
    _M.WriteLog('Artists:  ' .. mangainfo.Artists)
    _M.WriteLog('Genres:   ' .. mangainfo.Genres)
    if mangainfo.ChapterNames.Count > 0 then _M.WriteLog('ChapterNames:\n' .. Trim(mangainfo.ChapterNames.Text)) else _M.WriteLog('ChapterNames:  "N/A"') end
    if mangainfo.ChapterLinks.Count > 0 then _M.WriteLog('ChapterLinks:\n' .. Trim(mangainfo.ChapterLinks.Text)) else _M.WriteLog('ChapterLinks:  "N/A"') end
  end
end

-- Writes the manga entries for the current directory page into the log.
function _M.PrintMangaDirectoryEntries(page)
  if LuaDebugging then
    for i = 0, names.Count - 1 do _M.WriteLogWithHeader('Page ' .. page, names[i] .. '  (' .. links[i] .. ')') end
  end
end

-- Writes the page links for the current chapter into the log.
function _M.PrintChapterPageLinks()
  if LuaDebugging then
    for i = 0, task.PageLinks.Count - 1 do _M.WriteLogWithHeader('Page ' .. i + 1, task.PageLinks[i]) end
  end
end

-- Writes a log entry if LuaStatistics is enabled.
function _M.WriteStatistics(header, text)
  if LuaStatistics then print('[lua][' .. module.Website .. '][Statistics][' .. header .. ']  ' .. text) end
end


----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
local LuaDebug   = require 'LuaDebugging'

function getinfo()
	local x = nil
  	local u = MaybeFillHost(module.RootURL, url)
  
  	--debug url manga info 
  	LuaDebug.WriteLogWithHeader('getinfo', 'url ->  ' .. u)
  	if not http.Get(u) then return net_problem end

  	x = TXQuery.Create(http.Document)
  	mangainfo.CoverLink	= MaybeFillHost(module.RootURL, x.xpathstring('//*[@class="img-fluid not-lazy"]/@src'))
  	mangainfo.title     = x.xpathstring('//h1[contains(@class, "title")]')
  	mangainfo.genres    = x.xpathstringall('//*[contains(@class, "genres")]/a')
  	-- mangainfo.Status	= MangaInfoStatusIfPos()
  	mangainfo.Summary	= x.xpathstring('//*[contains(@class, "description")]'):gsub('Description', '')

	  local pages = 1
    local p = 1
    while p <= pages do
      if p > 1 then
        if http.get(mangainfo.url .. '?page=' .. tostring(p)) then
          x=TXQuery.Create(http.document)
        else
          break
        end
      end
      if p == pages then
        local pg = x.xpathstring('//*[contains(@class, "pagination")]/li[last()]/a/substring-after(@href, "?page=")')
        if pg ~= '' then pages = tonumber(pg) end
      end
      local v=x.xpath('//div[contains(@class, "list-group")]//div')
      for i=1,v.count do
        local v1=v.get(i)
        mangainfo.chapternames.Add(Trim(x.xpathstring('.//a/span', v1)))
	    mangainfo.chapterlinks.Add(x.xpathstring('.//a/@href', v1));
      end
      p = p + 1
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)

  	LuaDebug.PrintMangaInfo()
  	LuaDebug.WriteStatistics('chapters', mangainfo.ChapterLinks.Count .. '  (' .. mangainfo.Title .. ')')
  	return no_error	
end

function getpagenumber()
  local x = nil
  local u = MaybeFillHost(module.RootURL, url)
  LuaDebug.WriteLogWithHeader('getpagenumber', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  x = TXQuery.Create(http.Document)
  x.xpathstringall('//*[contains(@class, "mb-3")]/img/@data-src', task.pagelinks)
  if task.pagelinks.count < 1 then x.xpathstringall('//*[contains(@class, "img-fluid not-lazy")]/@data-src', task.pagelinks) end
  LuaDebug.PrintMangaDirectoryEntries(IncStr(url))
  return no_error
end

function getnameandlink()
  local x = nil
  local u = module.RootURL .. '/comic/'
  LuaDebug.WriteLogWithHeader('getnameandlink', 'url ->  ' .. u)
  if not http.Get(u) then return net_problem end
  x = TXQuery.Create(http.Document)
  local pages = 1
    local p = 1
    while p <= pages do
      if p > 1 then
        if http.get(u .. '?page=' .. tostring(p)) then
          x=TXQuery.Create(http.document)
        else
          break
        end
      end
      if p == pages then
        local pg = x.xpathstring('//*[contains(@class, "pagination")]/li[last()]/a/substring-after(@href, "?page=")')
        if pg ~= '' then pages = tonumber(pg) end
      end
      local v=x.xpath('//div[contains(@class, "row mb-3")]//div')
      for i=1,v.count do
        local v1=v.get(i)
        names.Add(Trim(x.xpathstring('.//*[contains(@class, "title")]/span', v1)))
        links.Add(x.xpathstring('.//a[contains(@class, "head")]/@href', v1));
      end
      p = p + 1
    end
  LuaDebug.PrintMangaDirectoryEntries(IncStr(url))
end

function Init()
	AddWebsiteModule('RawDevart', 'https://rawdevart.com', 'Raw')
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.Website                  = name
  m.RootURL                  = url
  m.Category                 = category
  m.OnGetInfo                = 'getinfo'
  m.OnGetNameAndLink         = 'getnameandlink'
  m.OnGetPageNumber          = 'getpagenumber'
  return m
end

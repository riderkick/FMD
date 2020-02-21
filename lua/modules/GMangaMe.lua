local domain = 'gmanga.me'
local mediaUrl = 'https://media.' .. domain .. '/uploads'

function getinfo()
  function urlencode(str)
    if (str) then
      str = string.gsub(str, "\n", "\r\n")
      str = string.gsub(str, "([^%w ])",
         function (c) return string.format ("%%%02X", string.byte(c)) end)
      str = string.gsub(str, " ", "_")
      str = string.gsub(str, '%.', '')
    end
    return str    
  end
  
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    local s = x.xpathstring('//script[@type="application/json" and @class]')
    x.parsehtml(s)
    local pageurl = x.xpathstring('json(*).globals.pageUrl')
    local id = x.xpathstring('json(*).mangaDataAction.mangaData.id')
    local cover = x.xpathstring('json(*).mangaDataAction.mangaData.cover')
    mangainfo.coverlink = mediaUrl .. '/manga/cover/' .. id .. '/' .. cover
    if mangainfo.title == '' then
      mangainfo.title=x.xpathstring('json(*).mangaDataAction.mangaData.title')
    end
    mangainfo.authors=x.xpathstringall('json(*).mangaDataAction.mangaData.authors().name')
    mangainfo.artists=x.xpathstringall('json(*).mangaDataAction.mangaData.artists().name')
    mangainfo.genres=x.xpathstringall('json(*).mangaDataAction.mangaData.categories().name')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('json(*).mangaDataAction.mangaData.status'),'publish','finish')
    mangainfo.summary=x.xpathstring('json(*).mangaDataAction.mangaData.summary')
    if http.xhr(MaybeFillHost(module.RootURL, '/api/mangas/' .. id)) then
      x = TXQuery.Create(http.document)
      local v = x.xpath('json(*).mangaReleases()')
      local t = {}
      local data = {}
      for i = 1, v.count do
        local v1 = v.get(i)
        local ch = tonumber(x.xpathstring('chapter', v1))
        local team = x.xpathstring('team_name', v1)
        local key = string.format('%08.2f %s', ch, team)
		
        table.insert(t, key)
        data[key] = {
          name = string.format('%s - %s [%s]', tostring(ch), x.xpathstring('title', v1), team),
          link = MaybeFillHost(module.rooturl, pageurl .. '/' .. tostring(ch) .. '/' .. urlencode(team))
        }
      end
      table.sort(t)
      for _, k in ipairs(t) do
        mangainfo.chapterlinks.add(data[k].link)
        mangainfo.chapternames.add(data[k].name)
      end
      return no_error
    else
      return net_problem
    end
  else
    return net_problem
  end
end

function getpagenumber()
  local js = require 'utils.jsunpack'
  if http.get(MaybeFillHost(module.rooturl,url)) then
    local x = TXQuery.Create(http.Document);
    local s = x.xpathstring('//script[@type="application/json" and @class]')
    x.parsehtml(s)
	local mediakey = x.xpathstring('json(*).globals.mediaKey')
    local pages = js.splitstr(x.xpathstring('json(*).readerDataAction.readerData.release.hq_pages'), '\r\n')
    for _, k in ipairs(pages) do
      task.pagelinks.add(mediaUrl .. '/releases/' .. k .. '?ak=' .. mediakey)
    end
    return true
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl..'/mangas') then
    local x = TXQuery.Create(http.Document);
    local s = HTMLDecode(x.xpathstring('//*[@data-store-name="mangasIndexStore"]/@data-props'))
    x.parsehtml(s)
    local v = x.xpath('json(*).mangas()')
    for i = 1, v.count do
      local v1 = v.get(i)
      names.add(x.xpathstring('title', v1))
      links.add(MaybeFillHost(module.rooturl, '/mangas/' .. x.xpathstring('id', v1) .. '/' .. x.xpathstring('slug', v1)))
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Arabic'
  m.website='GManga'
  m.rooturl='https://' .. domain
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

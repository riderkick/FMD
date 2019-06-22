function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//title'):gsub(' | Imangascans Reader', '')
    x.xpathhrefall('//ul[@class="dropdown-menu"]/li/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(., "var pages")]')
    x.parsehtml(SeparateRight(s, '='))
    local v = x.xpath('json(*)()')
    local base = v.get(1).toString
    for i = 2, v.count do
      task.pagelinks.add(MaybeFillHost(module.RootURL, base .. '/' .. v.get(i).toString))
    end
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl .. '/series-list') then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//h4[@class="series-title"]/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'IMangaScans'
  m.rooturl = 'https://reader.imangascans.org'
  m.category = 'English-Scanlation'
  m.lastupdated='April 13, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

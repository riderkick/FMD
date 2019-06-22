function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.Document)
    if mangainfo.title == '' then
      mangainfo.title=x.XPathString('css("div.title")')
    end
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('css(".cover > img")/@src'))
    local status = x.XPathString('//li[@class="info_block" and contains(span, "Status")]/span[@class="info_content"]')
    mangainfo.status = MangaInfoStatusIfPos(status, "In corso", "Completo")
    mangainfo.authors = x.XPathString('//li[@class="info_block" and contains(span, "Autore")]/span[@class="info_content"]')
    mangainfo.artists = x.XPathString('//li[@class="info_block" and contains(span, "Artista")]/span[@class="info_content"]')
    mangainfo.genres = x.XPathString('//li[@class="info_block" and contains(span, "Genere")]/span[@class="info_content"]')
    mangainfo.summary = x.XPathString('//div[@class="plot"]')
    x.xpathhrefall('css("div.chapter_list > ul > li > .ch_top > a")', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)    
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    local isExt = (x.xpathstring('//script[contains(@src, "rext.js")]/@src') ~= '')
    local title=x.xpathstring('//title')
    local s=x.xpathstring('//script[contains(., "current_page")]')
    s=ExecJS(s .. ';JSON.stringify({m:m,ch:ch,chs:chs});')
    x.parsehtml(s)
    local m=x.xpathstring('json(*).m')
    local ch=x.xpathstring('json(*).ch')
    local chs=x.xpathstring('json(*).chs')
    local data=string.format('info[manga]=%s&info[chapter]=%s&info[ch_sub]=%s&info[title]=%s', m, ch, chs, title)
    if isExt then data = data .. '&info[external]=1' end
    http.reset()
    if http.post(MaybeFillHost(module.rooturl, '/reader/c_i'), EncodeURL(data)) then
      x.parsehtml(ExecJS(StreamToString(http.document)))
      local path = x.xpathstring('json(*)()[3]')
      local v=x.xpath('json(*)()[2]()')
      local t={}
      for i = 1, v.count do
        local v1 = v.get(i)
        table.insert(t, v1.toString)
      end
      v=x.xpath('json(*)()[1]()')
      for i = 1, v.count do
        local v1=v.get(i)
        if isExt then
          s = string.format('%s/%s%s', t[i], x.xpathstring('./name', v1), x.xpathstring('./ex', v1))
        else
          s = string.format('/reader/%s/%s%s%s', path, x.xpathstring('./name', v1), t[i], x.xpathstring('./ex', v1))
        end
        task.pagelinks.add(MaybeFillHost(module.rooturl, s))
      end
      return true
    end
  end
  return false
end

function GetNameAndLink()
  if http.get(module.rooturl..'/reader/series') then
    local x=TXQuery.Create(http.Document)
    x.xpathhrefall('css(".manga_title > a")', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Italian-Scanlation'
  m.website='DigitalTeam'
  m.rooturl='https://dgtread.com'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
end

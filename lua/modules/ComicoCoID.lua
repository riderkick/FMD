function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//div[@class="con"]/h2')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//*[@class="bg_img_small"]/img/@src'))
    mangainfo.authors=x.xpathstringall('//div[@class="con"]/dl/dd[@class="name"]/span[@class="aln"]')
    mangainfo.genres=x.xpathstring('//div[@class="con"]/dl/dd[@class="name"]/p'):gsub('%s+', ''):gsub(',', ', ')
    mangainfo.summary=x.xpathstring('//div[@class="con"]/dl/dd[@class="dsc"]')
    if http.get(mangainfo.url .. '/chapters') then
      x.parsehtml(http.document)
      local v = x.xpath('json(*).data.list()')
      for i = 1, v.count do
        local v1 = v.get(i)
        mangainfo.chapterlinks.add(mangainfo.url .. '/chapters/' .. x.xpathstring('id', v1))
        local s = x.xpathstring('name', v1)
        if x.xpathstring('./salePolicy/isFree', v1) == 'false' then
          s = s .. ' [LOCKED]'
        end
        mangainfo.chapternames.add(s)
      end
    end
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    x=TXQuery.Create(http.Document)
    json = HexToStr(GetBetween('keyList : "', '"', x.xpathstring('//script[contains(., "keyList")]')))
    x.ParseHTML(json)
    x.xpathstringall('json(*).list().url', task.pagelinks)
  else
    return false
  end
  return true
end

local dirurls = {
  '/weekly/list?page=',
  '/titles/completed?page='
}

function getnameandlink()
  local lurl = dirurls[module.CurrentDirectoryIndex+1]
  if http.get(module.rooturl .. lurl .. IncStr(url)) then
    local x = TXQuery.Create(http.Document)
    local s = 'json(*).data().list()'
    if module.CurrentDirectoryIndex == 1 then
      s = 'json(*).data.list()'
    end
    local v = x.xpath(s)
    local hasTitles = false
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(module.rooturl..'/titles/'..x.xpathstring('id', v1))
      names.add(x.xpathstring('name', v1))
      hasTitles = true
    end
    if hasTitles then
      updatelist.CurrentDirectoryPageNumber = updatelist.CurrentDirectoryPageNumber + 1
    end
  end
  return net_problem
end

function Init()
  local m = NewModule()
  m.website = 'ComicoCoID'
  m.rooturl = 'http://www.comico.co.id'
  m.category = 'Indonesian'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.totaldirectory = 2
end

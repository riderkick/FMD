function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//*[contains(@class,"entry-content")]/h2')
    if mangainfo.title == '' then
      mangainfo.title = Trim(x.XPathString('//*[contains(@class,"entry-content")]/p[starts-with(.,"Title")]/substring-after(.,":")'))
    end
    if mangainfo.title == '' then
      mangainfo.title = x.XPathString('//*[contains(@class,"entry-content")]/p[1]/strong')
    end
    if mangainfo.title == '' then
      mangainfo.title = x.XPathString('//h1[@class="entry-title"]')
    end
    mangainfo.authors=Trim(x.xpathstring('//*[contains(@class,"entry-content")]/p[starts-with(.,"Author")]/substring-after(.,":")'))
    mangainfo.genres=x.xpathstring('//header[@class="entry-header"]/string-join(./p[position()>1]//a,", ")')
    mangainfo.chapterlinks.add(mangainfo.url)
    mangainfo.chapternames.add(mangainfo.title)
    local v = x.xpath('//*[contains(@class,"entry-pagination")]/a')
    for i = 1, v.count do
      local v1 = v.get(i)
      if string.match(v1.toString, '^Next') == nil then
        mangainfo.chapterlinks.add(v1.getAttribute('href'));
        mangainfo.chapternames.add(mangainfo.title .. ' - ' .. v1.toString);
      end
    end
    if mangainfo.chapternames.count > 1 then
      mangainfo.chapternames[0] = mangainfo.chapternames[0] .. ' - 1'
    end
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//*[contains(@class,"entry-content")]//img/@data-lazy-src', task.pagelinks)
    if task.pagelinks.count == 0 then
      x.xpathstringall('//div[@class="separator" and @style]//img/@data-lazy-src', task.pagelinks)
    end
  else
    return false
  end
  return true
end

function getdirectorypagenumber()
  if http.GET(module.RootURL) then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.xpathstring('//*[contains(@class,"archive-pagination")]/ul/li[last()-1]'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  if http.get(module.rooturl .. '/page/' .. IncStr(url) .. '/') then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//h2[@class="entry-title"]/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewModule()
  m.website = 'MyReadingManga'
  m.rooturl = 'https://myreadingmanga.info'
  m.category = 'H-Sites'
  m.lastupdated='April 10, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
  m.sortedlist = true
end

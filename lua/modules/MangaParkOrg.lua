function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  http.cookies.values['set'] = 'h=1'
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.Document)
    if mangainfo.title == '' then
      mangainfo.title=x.XPathString('//h3')
    end
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('css("img.img-fluid")/@src'))
    mangainfo.status = MangaInfoStatusIfPos(x.XPathString('css("table.table-borderless")//tr[th="Status"]/td'))
    mangainfo.authors = x.XPathString('css("table.table-borderless")//tr[th="Author(s)"]/td')
    mangainfo.artists = x.XPathString('css("table.table-borderless")//tr[th="Artist(s)"]/td')
    mangainfo.genres = x.XPathStringAll('css("table.table-borderless")//tr[th="Genre(s)"]/td')
    mangainfo.genres = mangainfo.genres:gsub('%s+/%s+', ', ')
    mangainfo.summary = x.XPathString('//h4[.="Summary"]/following-sibling::p')
    local v=x.xpath('css("div.card-header")')
    for i=1, v.count do
      local v1=v.get(i)
      local src = x.xpathstring('./div/a', v1)
      local w = x.xpath('./following-sibling::div/ul/li//a', v1)
      for j = 1, w.count do
        local w1 = w.get(j)
        mangainfo.chapterlinks.add(w1.getAttribute('href'))
        mangainfo.chapternames.add(string.format('%s (%s)', w1.toString, src))
      end
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)    
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  if http.get(MaybeFillHost(module.rooturl,url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('json(//script[contains(.,"var _load_pages")]/substring-after(substring-before(.,";")," = "))()/u', task.pagelinks)
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if http.get(module.rooturl..'/browse?order_by=create&page='..IncStr(url)) then
    x=TXQuery.Create(http.Document)
    x.xpathhrefall('css("#browse h6")/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. '/browse?order_by=create') then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('css("#paging nav.d-none ul.pagination")/li[last()-1]/a'))
    if page == nil then
      page = 1
    end
    return true
  else
    return false
  end
end

function Init()
  m=NewModule()
  m.category='English'
  m.website='MangaParkOrg'
  m.rooturl='https://mangapark.org'
  m.lastupdated = 'April 09, 2019'
  m.sortedlist = true
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
end

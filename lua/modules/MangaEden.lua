local diren = '/en/en-directory/?order=-0'
local dirit = '/en/it-directory/?order=-0'

function GetInfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//*[@class="manga-title"]')
    end
    local coverlink = x.xpathstring('//div[@class="mangaImage2"]//img/@src')
    if coverlink ~= '' then
      if coverlink:find('^//') then coverlink = 'https:' .. coverlink; end
      mangainfo.coverlink=MaybeFillHost(module.RootURL, coverlink)
    end
    mangainfo.authors=x.xpathstringall('//*[@class="rightBox"]/a[contains(@href,"/?author=")]')
    mangainfo.artists=x.xpathstringall('//*[@class="rightBox"]/a[contains(@href,"/?artist=")]')
    mangainfo.genres=x.xpathstringall('//*[@class="rightBox"]/a[contains(@href,"/?categories")]')
    mangainfo.summary=x.xpathstring('//*[@id="mangaDescription"]')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//*[@class="rightBox"]'), 'Ongoing', 'Completed')
    x.xpathhrefall('//table//tr/td/a[@class="chapterLink"]', mangainfo.chapterlinks, mangainfo.chapternames)
    for i = 0, mangainfo.chapternames.count-1 do
      mangainfo.chapternames[i] = mangainfo.chapternames[i]:gsub("Chapter", " ")
    end
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(., "var pages")]')
    s = GetBetween('=', ';', s)
    x.parsehtml(s)
    x.xpathstringall('json(*)().fs', task.PageLinks)
    for i = 0, task.PageLinks.count-1 do
      if string.find(task.PageLinks[i], '^//') then
        task.PageLinks[i] = 'https:' .. task.PageLinks[i]
      end;
    end
    task.PageContainerLinks.text = MaybeFillHost(module.rooturl, url)
    return true
  else
    return false
  end
end

function GetDirUrl(website)
  if module.website == 'MangaEden_IT' or module.website == 'PervEden_IT' then
    return dirit
  else
    return diren
  end
end

function GetDirectoryPageNumber()
  if http.GET(AppendURLDelim(module.RootURL)..GetDirUrl(module.website)) then
    x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('//*[@class="pagination pagination_bottom"]/a[last()-1]'))
    if page == nil then
      page = 1
    end
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if http.get(module.rooturl..GetDirUrl(module.website).."&page="..IncStr(url)) then
    x=TXQuery.Create(http.Document)
    x.xpathhrefall('//table[@id="mangaList"]//tr/td[1]/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function BeforeDownloadImage()
  http.headers.values['Referer'] = task.PageContainerLinks.text
  return true
end

function InitModule(website, rooturl, category)
  m=NewModule()
  m.category=category
  m.website=website
  m.rooturl=rooturl
  m.lastupdated='May 17, 2019'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
  m.ongetnameandlink='GetNameAndLink' 
  m.onbeforedownloadimage = 'BeforeDownloadImage'
end

function Init()
  InitModule('MangaEden', 'http://www.mangaeden.com', 'English')
  InitModule('MangaEden_IT', 'http://www.mangaeden.com', 'Italian')
  InitModule('PervEden', 'http://www.perveden.com', 'H-Sites')
  InitModule('PervEden_IT', 'http://www.perveden.com', 'H-Sites')
end

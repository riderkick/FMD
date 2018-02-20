local ALPHA_LIST_UP = '#ABCDEFGHIJKLMNOPQRSTUVWXYZ'

function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.Document)
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('//div[@class="bookfrontpage"]/a/img/@src'))
    if module.website == 'WieManga' then
      mangainfo.title=x.XPathString('//div[@class="bookmessagebox"]/h1/substring-before(., " Manga")')
      mangainfo.summary = x.xpathstring('//h4[text()="Beschreibung"]/following-sibling::text()')
      mangainfo.artists = x.XPathStringAll('//div[@class="bookmessgae"]//dd[contains(span/text(), "Zeichner")]/a')
      mangainfo.authors = x.XPathStringAll('//div[@class="bookmessgae"]//dd[contains(span/text(), "Autor")]/a')
      mangainfo.genres = x.XPathStringAll('//div[@class="bookmessgae"]//dd[contains(span/text(), "Genre")]/a')
      mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="bookmessgae"]//dd[contains(span/text(), "Status")]/a'), 'ongoing', 'finished')
    elseif module.website == 'MangaRussia' then
      mangainfo.title=x.XPathString('//div[@class="bookmessagebox"]/h1/substring-after(., "Манга ")')
      mangainfo.summary = x.xpathstring('//h4[text()="Описание"]/following-sibling::text()')
      mangainfo.authors = x.XPathStringAll('//div[@class="bookmessgae"]//dd[contains(span/text(), "Автор")]/a')
      mangainfo.genres = x.XPathStringAll('//div[@class="bookmessgae"]//dd[contains(span/text(), "Жанры")]/a')
      mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="bookmessgae"]//dd[contains(span/text(), "Перевод")]/a'), 'ongoing', 'finished')
    end
    x.XPathHREFAll('//div[@class="chapterlist"]/table//td[@class="col1"]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterLinks, mangainfo.chapterNames)
    return no_error
  else
    return net_error
  end
end

function GetPageNumber()
  task.pagelinks.clear()
  task.pagenumber=0
  local s=MaybeFillHost(module.rooturl,url):gsub('/$', '') .. '-1.html'
  if http.get(s) then  
    x=TXQuery.Create(http.Document)
    task.pagenumber=x.xpath('(//select[@id="page"])[1]/option').count
    return true
  else
    return false
  end
end

function GetImageURL()
  local baseurl = MaybeFillHost(module.rooturl,url)
  local s = baseurl:gsub('/$','') .. '-' .. tostring(workid+1) .. '.html'
  http.headers.values['Referer'] = baseurl
  if http.get(s) then
    x=TXQuery.Create(http.Document)
    task.pagelinks[workid]=x.xpathstring('//img[@id="comicpic"]/@src')
    return true
  else
    return false
  end
end

function GetNameAndLink()
  local s = '0-9'
  if module.CurrentDirectoryIndex ~= 0 then
    s = ALPHA_LIST_UP:sub(module.CurrentDirectoryIndex+1,module.CurrentDirectoryIndex+1)
  end
  if http.get(module.rooturl..'/category/' .. s .. '_'.. IncStr(url) .. '.html') then
    x=TXQuery.Create(http.Document)
    v=x.xpath('//*[@class="booklist"]//span[@class="pagetor"]//text()')
    local i = 1
    for j=1,v.count do
      v1 = v.get(j)
      local tmp = tonumber(v1.toString)
      if (tmp ~= nil) and (tmp > i) then i = tmp end
    end
    updatelist.CurrentDirectoryPageNumber = i
    x.XPathHREFtitleAll('//*[@class="booklist"]/table//dl/dd/a[1]', links, names)
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.category=category
  m.website=name
  m.rooturl=url
  m.totaldirectory = ALPHA_LIST_UP:len()
  m.lastupdated='February 19, 2018'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
  m.OnGetImageURL='GetImageURL'
  return m
end

function Init()
  AddWebsiteModule('WieManga', 'https://www.wiemanga.com', 'German')
  AddWebsiteModule('MangaRussia', 'http://www.mangarussia.com', 'Russian')
end 

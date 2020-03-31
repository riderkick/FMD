function getauthors()
  if module.website == 'NiAddRU' then
    return "Авторы"
  else
    return "Aut"
  end
end

function getartists()
  if module.website == 'NiAddRU' then
    return "Исполнитель"
  elseif module.website == 'NiAddDE' then
    return "Künstler"
  else
    return "Art"
  end
end

function getgenres()
  if module.website == 'NiAddES' or module.website == 'NiAddBR' then
    return "Géneros"
  elseif module.website == 'NiAddIT' then
    return "generi"
  elseif module.website == 'NiAddRU' then
    return "Жанры"
  else
    return "Genres"
  end
end

function getdirpagenumber()
  if module.website == 'NiAddES' or module.website == 'NiAddBR' then
    return "Todos"
  elseif module.website == 'NiAddIT' then
    return "tutti"
  elseif module.website == 'NiAddRU' then
    return "все"
  elseif module.website == 'NiAddDE' then
    return "alle"
  elseif module.website == 'NiAddFR' then
    return "Tous"
  else
    return "All"
  end
end

function getinfo()
  if url:find('/chapters') then
    mangainfo.url=MaybeFillHost(module.RootURL, url)
  else
    mangainfo.url=MaybeFillHost(module.RootURL, url..'/chapters'):gsub('.html', '')
  end
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title=x.xpathstring('//h1')
    end
    mangainfo.coverlink=x.xpathstring('//img[@itemprop="image"]/@src')
    mangainfo.authors=x.xpathstringall('//td[@class="bookside-general-type"]//div[@itemprop="author" and contains(span, "'..getauthors()..'")]/a/span')
    mangainfo.artists=x.xpathstringall('//td[@class="bookside-general-type"]//div[@itemprop="author" and contains(span, "'..getartists()..'")]/a/span')
    mangainfo.genres=x.xpathstringall('//td[@class="bookside-general-type"]//div[contains(span, "'..getgenres()..'")]/a/span')
    local n = x.xpath('//span[@class="chp-title"]')
    local v = x.xpath('//ul[contains(@class, "chapter-list")]/a')
    for i = 1, v.count do
      local v1 = v.get(i)
      mangainfo.chapternames.Add(n.get(i).toString)
      mangainfo.chapterlinks.Add(v.get(i).getAttribute('href'))
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('(//select[@class="sl-page"])[last()]/option/@value', task.pagecontainerlinks)
    task.pagenumber = task.pagecontainerlinks.count
  else
    return false
  end
  return true
end

function getnameandlink()
  local s = string.format("/category/index_%s.html?sort=name", IncStr(url))
  if http.get(module.rooturl .. s) then
    local x = TXQuery.Create(http.Document)
    local p=x.xpathstring('//div[@class="page-all-num"]/substring-after(.,"'..getdirpagenumber()..' ")')
    p = tonumber(p)
    if p ~= nil then
      updatelist.CurrentDirectoryPageNumber = p
    end
    local v = x.XPath('//div[contains(@class, "manga-list")]//div[@class="manga-item"]//td[2]/a[1]')
    for i = 1, v.Count do
      links.Add(v.Get(i).GetAttribute('href'):gsub('.html', '') .. '/chapters')
      names.Add(x.XPathString('div', v.Get(i)))
    end
    return no_error
  else
    return net_problem
  end
end

function getimageurl()
  local s = MaybeFillHost(module.RootURL, task.pagecontainerlinks[workid])
  if http.GET(s) then      
    task.pagelinks[workid] = TXQuery.Create(http.Document).xpathstring('//img[contains(@class,"manga_pic")]/@src')
    return true
  else
    return false
  end
end

function AddWebsiteModule(name, url, category)
  local m=NewModule()
  m.website=name
  m.rooturl=url
  m.category=category
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetimageurl='getimageurl'
end

function Init()
local cat = 'English'
  AddWebsiteModule('NiAdd', 'https://www.niadd.com', cat)
  
  cat = 'Spanish'
  AddWebsiteModule('NiAddES', 'https://es.niadd.com', cat)
  
  cat = 'Italian'
  AddWebsiteModule('NiAddIT', 'https://it.niadd.com', cat)
  
  cat = 'Russian'
  AddWebsiteModule('NiAddRU', 'https://ru.niadd.com', cat)
  
  cat = 'Portuguese'
  AddWebsiteModule('NiAddBR', 'https://br.niadd.com', cat)
  
  cat = 'German'
  AddWebsiteModule('NiAddDE', 'https://de.niadd.com', cat)
  
  cat = 'French'
  AddWebsiteModule('NiAddFR', 'https://fr.niadd.com', cat)
end 

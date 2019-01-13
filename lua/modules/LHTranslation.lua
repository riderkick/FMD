function GetInfo() 
  mangainfo.url = MaybeFillHost(module.RootURL, url)
  if http.GET(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then 
      mangainfo.title = x.XPathString('//h1[@class="postsby"]/substring-after(.,":")')
    end
    mangainfo.title = Trim(mangainfo.title)
    mangainfo.coverLink = x.XPathString('(//div[@class="featured-thumbnail"])[1]/img/@src')    
    while true do
      x.xpathhreftitleall('//h2[@class="title"]/a',mangainfo.chapterlinks,mangainfo.chapternames)
      local nextpage = x.xpathstring('//nav/div[@class="nav-links"]/*[contains(@class, "current")]/following-sibling::a[1]/@href')
      if nextpage == '' then break; end
      if http.get(nextpage) then
        x.parsehtml(http.document)
      else
        break
      end
    end    
    InvertStrings(mangainfo.chapterLinks, mangainfo.chapterNames)
    return no_error
  else
    return net_problem
  end
end

local readrooturl = 'http://read.lhtranslation.com'
function GetPageNumber()
  local s = '/read-' .. url:gsub('/', '') .. '.html'
  if http.get(MaybeFillHost(readrooturl, s)) then
    local x=TXQuery.Create(http.document)
    x.xpathstringall('//img[contains(@class,"chapter-img")]/@src',task.pagelinks)
    if task.pagelinks.count > 0 then return true; end
  end
  s = s:gsub('(%d+)-(%d+)%.html$', '%1.%2.html')
  if http.get(MaybeFillHost(readrooturl, s)) then
    local x=TXQuery.Create(http.document)
    x.xpathstringall('//img[contains(@class,"chapter-img")]/@src',task.pagelinks)
    if task.pagelinks.count > 0 then return true; end
  end
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x = TXQuery.Create(http.document)
    if http.get(x.xpathstring('//div[@class="commentmetadata"][1]/p/a/@href')) then
      x.parsehtml(http.document)
      x.xpathstringall('//img[contains(@class,"chapter-img")]/@src',task.pagelinks)
      if task.pagelinks.count > 0 then return true; end
    end    
  end
  return false
end

function GetNameAndLink()
  if http.GET(module.rooturl) then
    local x=TXQuery.Create(http.document)
    local v=x.xpath('//select[@id="cat"]/option[@value!="-1"]')
    for i=1,v.count do
      local v1=v.get(i)
      links.add(module.rooturl..'/?cat='..v1.getattribute('value'))
      names.add(Trim(v1.tostring:gsub('%(%d+%)%s*$', '')))
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m=NewModule()
  m.category = 'English-Scanlation'
  m.Website = 'LHTranslation'
  m.RootURL = 'http://lhtranslation.net'
  m.OnGetNameAndLink = 'GetNameAndLink'
  m.OnGetInfo = 'GetInfo'
  m.OnGetPageNumber = 'GetPageNumber'
end

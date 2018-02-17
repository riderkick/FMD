function GetInfo() 
  mangainfo.url = MaybeFillHost(module.RootURL, url)
  if http.GET(mangainfo.url) then
    x=TXQuery.Create(http.document)
    if mangainfo.title == '' then 
      mangainfo.title = x.XPathString('//h1[@class="postsby"]/substring-after(.,":")')
    end
    mangainfo.title = Trim(mangainfo.title)
    mangainfo.coverLink = x.XPathString('(//div[@class="featured-thumbnail"])[1]/img/@src')
    x.xpathhreftitleall('//h2[@class="title"]/a',mangainfo.chapterlinks,mangainfo.chapternames)
    InvertStrings(mangainfo.chapterLinks, mangainfo.chapterNames)
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  if http.GET(MaybeFillHost(module.rooturl, url)) then
    x=TXQuery.Create(http.document)
    if http.get(x.xpathstring('//div[@class="commentmetadata"][1]/p/a/@href')) then
      x.parsehtml(http.document)
      x.xpathstringall('//img[@class="chapter-img"]/@src',task.pagelinks)
      return true
    end    
  end
  return false
end

function GetNameAndLink()
  if http.GET(module.rooturl) then
    x=TXQuery.Create(http.document)
    v=x.xpath('//select[@id="cat"]/option[@value!="-1"]')
    for i=1,v.count do
      v1=v.get(i)
      links.add(module.rooturl..'/?cat='..v1.getattribute('value'))
      names.add(v1.tostring)
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category = 'English-Scanlation'
  m.Website = 'LHTranslation'
  m.RootURL = 'http://lhtranslation.com'
  m.OnGetNameAndLink = 'GetNameAndLink'
  m.OnGetInfo = 'GetInfo'
  m.OnGetPageNumber = 'GetPageNumber'
end

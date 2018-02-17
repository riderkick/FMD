function getinfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.Document)
    mangainfo.title=x.XPathString('//select[@name="manga"]/option[@selected]')
    s=x.xpathstring('//select[@name="manga"]/option[@selected]/@value')
    v=x.xpath('//select[@name="chapter"]/option')
    for i = 1, v.count do
      v1=v.get(i)
      mangainfo.chapternames.add(v1.toString)
      mangainfo.chapterlinks.add(module.rooturl .. '/' .. s .. '/' .. v1.getAttribute('value'))
    end
    InvertStrings(mangainfo.chapterLinks, mangainfo.chapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber=0
  print(MaybeFillHost(module.rooturl,url))
  if http.get(MaybeFillHost(module.rooturl,url)) then  
    local x=TXQuery.Create(http.Document)
    task.pagenumber = x.xpath('//select[@name="page"]/option').count
    return true
  else
    return false
  end
end

function getnameandlink()
  if http.get(module.rooturl) then
    x=TXQuery.Create(http.Document)
    v=x.xpath('//select[@name="manga"]/option[@value!="0"]')
    for i = 1, v.count do
      v1=v.get(i)
      names.add(v1.toString)
      links.add(module.rooturl .. '/' .. v1.getAttribute('value'))
    end
    return no_error
  else
    return net_problem
  end
end

function getimageurl()
  local s = MaybeFillHost(module.rooturl,url) .. '/' .. tostring(workid+1)
  if http.get(s) then
    x=TXQuery.Create(http.Document)
    task.pagelinks[workid]=MaybeFillHost(module.rooturl,x.xpathstring('//img[@id="manga_img"]/@src'))
    return true
  else
    return false
  end
end

function AddWebsiteModule(name, url)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = 'Turkish'
  m.lastupdated = 'February 16, 2018'
  m.OnGetInfo = 'getinfo'
  m.OnGetPageNumber = 'getpagenumber'
  m.OnGetImageURL = 'getimageurl'
  m.OnGetNameAndLink = 'getnameandlink'
  return m
end

function Init()
  AddWebsiteModule('MangaOku', 'http://www.mangaoku.net')
  AddWebsiteModule('Turkcraft', 'http://turkcraft.com')
end 
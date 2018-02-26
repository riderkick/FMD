function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)	
    mangainfo.title=x.xpathstring('//div[@id="LayerContenido"]/div[@id][1]')
    mangainfo.coverlink=MaybeFillHost(module.rooturl,x.xpathstring('//div[@id="LayerContenido"]/div[@id][2]//tr[1]/td[2]//img/@src'))
    mangainfo.genres=x.xpathstring('string-join(//div[@id="LayerContenido"]/div[@id][2]//tr[5]//a,", ")')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@id="LayerContenido"]/div[@id][2]//tr[6]/td[1]'),'En proceso','Completo')
    mangainfo.summary=x.xpathstring('//div[@id="LayerContenido"]/div[@id][2]//tr[1]/td[1]')
    x.xpathstringall('//div[@id="tomo"]//a/@href',mangainfo.chapterlinks)
    x.xpathstringall('//div[@id="tomo"]//a/string-join((../b,.)," ")',mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  if http.get(MaybeFillHost(module.rooturl,url):gsub('%.html?$','-1%1')) then
    task.pagenumber=TXQuery.Create(http.Document).xpathcount('//select[@id="nav-jump"]/option')
    return true
  else
    return false
  end
  return true
end

function getimageurl()
  if http.get(MaybeFillHost(module.rooturl,url):gsub('%.html?$','-'..(workid+1)..'%1')) then
    task.pagelinks[workid]=TXQuery.create(http.document).xpathstring('//img[@id="photo"]/@src')
    return true
  end
  return false
end

function getnameandlink()
  if http.get(module.rooturl..'/mangas.htm?ord=todos') then
    TXQuery.Create(http.document).xpathhrefall('//*[@id="manga"]/div/a',links,names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Spanish'
  m.website='AnimExtremist'
  m.rooturl='http://www.animextremist.com'
  m.lastupdated='February 18, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetimageurl='getimageurl'
  m.ongetnameandlink='getnameandlink'
end

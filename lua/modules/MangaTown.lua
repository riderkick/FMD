function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)	
    mangainfo.title=x.xpathstring('//h1')
    mangainfo.coverlink=x.xpathstring('//*[@class="detail_info clearfix"]/img/@src')
    mangainfo.authors=x.xpathstring('//*[@class="detail_info clearfix"]/ul/li[starts-with(.,"Author(s):")]/substring-after(.,":")')
    mangainfo.artists=x.xpathstring('//*[@class="detail_info clearfix"]/ul/li[starts-with(.,"Artist(s):")]/substring-after(.,":")')
    mangainfo.genres=x.xpathstring('//*[@class="detail_info clearfix"]/ul/li[starts-with(.,"Genre(s):")]/substring-after(.,":")')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//*[@class="detail_info clearfix"]/ul/li[starts-with(.,"Status(s):")]'))
    mangainfo.summary=x.xpathstring('//*[@class="detail_info clearfix"]/ul/li/span[@id="show"]/normalize-space(text())')
    v=x.xpath('//ul[@class="chapter_list"]/li')
    for i=1,v.count do
      v2=v.get(i)
      mangainfo.chapterlinks.add(x.xpathstring('a/@href',v2))
      mangainfo.chapternames.add(x.xpathstring('string-join((a/text(),span[not(@class)])," ")',v2))
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  if http.get(MaybeFillHost(module.rooturl,url)) then
    task.pagenumber=TXQuery.Create(http.Document).xpathcount('(//select[not(@id)])[1]/option[not(contains(@value,"featured.html"))]')
    return true
  else
    return false
  end
  return true
end

function getimageurl()
  local s=url
  if workid>0 then
   s=AppendURLDelim(s)..(workid+1)..'.html'
  end
  if http.get(MaybeFillHost(module.rooturl,s)) then
    task.pagelinks[workid]=TXQuery.create(http.document).xpathstring('//*[@id="viewer"]//img[@alt]/@src')
    return true
  end
  return false
end

function getdirectorypagenumber()
  if http.get(module.rooturl..'/directory/?name.az') then
    page=TXQuery.Create(http.document).xpathcount('(//select)[last()]/option')
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  if http.get(module.rooturl..'/directory/'..IncStr(url)..'.htm?name.az') then
    TXQuery.Create(http.document).xpathhreftitleall('//ul[@class="manga_pic_list"]/li/a',links,names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='English'
  m.website='MangaTown'
  m.rooturl='http://www.mangatown.com'
  m.lastupdated='February 17, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetimageurl='getimageurl'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
  m.ongetnameandlink='getnameandlink'
end

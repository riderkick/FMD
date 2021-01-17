local dirurl = '/mangas'

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//title/substring-before(.," - Union Mangás")')
    end
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('//img[@class="img-thumbnail"]/@src'))
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="table"]/div[@class="row"]/div[6]'), 'En Cours', 'Termine')
    mangainfo.authors=x.xpathstring('//h4[starts-with(./label,"Autor")]/substring-after(.,":")')
    mangainfo.artists=x.xpathstring('//h4[starts-with(./label,"Artista")]/substring-after(.,":")')
    mangainfo.genres=x.xpathstring('//h4[starts-with(./label,"Gênero")]/substring-after(.,":")')
    mangainfo.summary=x.xpathstring('//*[@class="panel-body"]')
    x.xpathhrefall('//*[contains(@class,"lancamento-linha")]/div[1]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x=TXQuery.Create(http.Document)
    x.xpathstringall('//img[contains(@class, "img-manga") and contains(@src, "/leitor/")]/@src', task.pagelinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  local s = module.RootURL .. dirurl
  s = s .. '/a-z/' .. IncStr(url) .. '/*'
  if http.get(s) then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//div[contains(@class,"bloco-manga")]/a[2]', links, names)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. dirurl) then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.xpathstring('//ul[@class="pagination"]/li[last()]/a/substring-before(substring-after(@href,"a-z/"),"/")'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m=NewModule()
  m.category='Portuguese'
  m.website='UnionMangas'
  m.rooturl='https://unionleitor.top'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
end

local hitmanga = 'http://www.hitmanga.eu'
local hitmangalistener = 'http://www.hitmanga.eu/listener/'

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local s=TStrings.Create()
    s.loadfromstream(http.document)
    local str=string.gsub(s.text, '%-%-!>', '-->')
    local x=TXQuery.Create(str)
    mangainfo.title=x.xpathstring('//div[@id="picture"]//h2')
    mangainfo.coverlink=MaybeFillHost(module.rooturl,x.xpathstring('//div[@id="picture"]/div/img/@src'))
    mangainfo.authors=x.xpathstringall('//div[@id="mangafiche"]//table//tr[contains(th, "Auteur")]/td/a')
    mangainfo.authors=x.xpathstringall('//div[@id="mangafiche"]//table//tr[contains(th, "Illustrateur")]/td/a')
    mangainfo.genres=x.xpathstringall('//div[@id="mangafiche"]//table//tr[contains(th, "Genre")]/td/a')
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('//div[@id="mangafiche"]//table//tr[contains(th, "Statut")]/td/a'), 'cours', 'termin')
    mangainfo.summary=x.xpathstring('//section[@id="synopsis"]/p')
    v=x.xpath('//section[contains(@class, "listchapseries")]/ul/li')
    for i=1,v.count do
      v1=v.get(i)
      mangainfo.chapterlinks.add(x.xpathstring('div/a[contains(i/@class, "fa-book")]/@href', v1))
      mangainfo.chapternames.add(x.xpathstring('p/span[@class="chapter"]', v1))
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames) 
    return no_error
  else
    return net_problem
  end
end

function getpath(str)
  local path = ''
  local i = 1
  for l in string.gmatch(str, "[^/]*") do
    if i == 6 then break end
    path = path .. l .. '/'
    i = i + 1
  end
  return path
end

function getpagenumber()
  if http.get(MaybeFillHost(hitmanga,url)) then
    local s=TStrings.Create()
    s.loadfromstream(http.document)
    local str=string.gsub(s.text, '%-%-!>', '-->')
    local x=TXQuery.Create(str)
    local link = x.xpathstring('//*[@id="loadReadPages"]/@data-permalink')
    local num = x.xpathstring('//*[@id="loadReadPages"]/@data-number')
    local src = getpath(x.xpathstring('//*[@id="chpimg"]/@src'))
    local q = 'type=chap-pages&permalink='..link..'&number='..num
    http.reset()
    if http.post(hitmangalistener, q) then
      local s = TStrings.Create()
      s.loadfromstream(http.document)
      for l in string.gmatch(s.text, "[^|]+") do
        task.pagelinks.add(src .. string.gsub(l, '#', '%%23'))
      end
      return true
    else
      return false
    end
    return true
  else
    return false
  end
  return true
end

function getnameandlink()
  -- FIXME: manga directory shows only first 300 titles.
  return no_error
end

function Init()
  m=NewModule()
  m.category='French'
  m.website='MyMangaIO'
  m.rooturl='http://www.mymanga.io'
  m.lastupdated='February 17, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end

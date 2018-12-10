function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//h1[@class="bg-header"]')
      mangainfo.title = string.gsub(mangainfo.title, '^Manga ', '')
      mangainfo.title = string.gsub(mangainfo.title, ' VF$', '')
    end
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[@class="table"]/div[@class="row"]/div[6]'), 'En Cours', 'Termine')
    mangainfo.authors=x.xpathstring('//div[@class="table"]/div[@class="row"]/div[1]')
    mangainfo.genres=x.xpathstring('//div[@class="table"]/div[@class="row"]/div[4]')
    mangainfo.summary=x.xpathstring('//div[@id="synopsis"]/string-join(text(),codepoints-to-string(10))')
    local v = x.xpath('//*[@id="liste_chapitres"]/ul/li/a')
    for i = 1,v.count do
      local v1 = v.get(i)
      mangainfo.chapterlinks.add(v1.getAttribute('href'))
      local s = v1.toString
      if Pos('[email protected]', s) ~= 0 then
        s = mangainfo.title .. ' ' .. x.xpathstring('text()[2]', v1)
      end
      s = string.gsub(s, '^Scan ', '')
      mangainfo.chapternames.add(s)
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
    x.xpathstringall('//select[@id="pages"]/option[position() < last()]/@value', task.pagelinks)
    task.pagecontainerlinks.text = http.cookies.text
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl .. '/mangas/') then
    local x = TXQuery.Create(http.Document)
    x.XPathHREFAll('//*[@id="liste_mangas"]/div[@class="row"]/div[@class="cell"][1]/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function downloadimage()
  http.reset()
  http.cookies.text = task.pagecontainerlinks.text
  if http.get(MaybeFillHost(module.rooturl, url)) then
    local x = TXQuery.Create(http.document)
    local img = x.xpathstring('//img[@id="image"]/@src')
    if img ~= '' then
      return http.get(img)
    end
    local nom = x.xpathstring('//*[@id="mangas"]/@data-nom'):gsub('/', '_'):gsub('%?', '')
    local mangauri = x.xpathstring('//*[@id="mangas"]/@data-uri')
    local chapnom = x.xpathstring('//*[@id="chapitres"]/@data-nom')
    local chapuri = x.xpathstring('//*[@id="chapitres"]/@data-uri')
    local imgnom = x.xpathstring('//select[@id="pages"]/option[@value="'..url..'"]/@data-img')
    local imgurl = 'http://cdn.japscan.to/cr_images/' .. nom .. '/'
    if chapnom == '' then
      imgurl = imgurl .. chapuri
    else
      imgurl = imgurl .. chapnom
    end
    imgurl = imgurl .. '/' .. imgnom
    if http.get(imgurl) then
      local s = TImagePuzzle.Create(5, 5)
      local n = 0
      local x = {2,4,0,3,1}
      local y = {4,3,2,1,0}
      for i = 1, 5 do
        for j = 1, 5 do
          s.matrix[y[i]*5+x[j]] = n
          n = n + 1
        end
      end
      s.descramble(http.document, http.document)
      return true
    end
  end
  return false
end

function Init()
  local m=NewModule()
  m.category='French'
  m.website='Japscan'
  m.rooturl='http://www.japscan.to'
  m.lastupdated='April 6, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ondownloadimage='downloadimage'
end

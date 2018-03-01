function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    mangainfo.title=x.xpathstring('//h1[@class="ebook_title"]')
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//img[contains(@class, "ebook_cover")]/@src'))
    mangainfo.authors=x.xpathstring('//table[@class="details_table"]/tbody/tr[contains(td, "Author")]/td[2]')
    mangainfo.genres=x.xpathstringall('//table[@class="details_table"]/tbody/tr[contains(td, "Genres")]/td[2]/a')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//table[@class="details_table"]/tbody/tr[contains(td, "Status")]/td[2]'))
    mangainfo.summary=x.xpathstring('//*[@class="ebook_description"]')
    x.xpathhrefall('//div[contains(@class,"chapters")]/span[not(@id="show_all")]/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber=0
  if http.get(MaybeFillHost(module.rooturl, url)) then
    task.pagenumber=TXQuery.Create(http.Document).xpathcount('//select[@id="jumpto"]/option')
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl .. '/manga-list') then
    local x = TXQuery.Create(http.Document)
    x.xpathhrefall('//ul[@class="ul_list"]/li/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function getimageurl()
  if http.get(MaybeFillHost(module.rooturl,url):gsub('%d+$',tostring(workid+1))) then
    local x = TXQuery.create(http.document)
    local s = x.xpathstring('//img[contains(@class, "ebook_img")]/@src')
    task.pagelinks[workid] = MaybeFillHost(module.rooturl, s)
    return true
  end
  return false
end

function Init()
  local m = NewModule()
  m.website = 'ReadMangaEU'
  m.rooturl = 'http://www.readmanga.eu'
  m.category = 'English'
  m.lastupdated='March 1, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetimageurl='getimageurl'
end

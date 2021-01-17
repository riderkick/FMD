function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)	
    mangainfo.title=x.xpathstring('//h3')
    mangainfo.coverlink=x.xpathstring('//*[@alt="cover"]/@src')
    mangainfo.artists=x.xpathstringall('//*[@class="tag tag-accepted"][contains(@href,"=artist")]/text()')
    mangainfo.genres=x.xpathstringall('//*[@class="list-inline"][2]/a/text()')
    mangainfo.chapterlinks.add(url)
    mangainfo.chapternames.add(mangainfo.title)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  task.pagenumber=0
  url = module.rooturl..url..'/paginated/1'
  url = string.gsub(url, 'contents', 'reader')
  if http.get(url) then
    task.pagenumber=TXQuery.Create(http.Document).xpathcount('//select[@id="select-page"]/option')
  else
    return false
  end
  return true
end

function getimageurl()
  url = module.rooturl..url..'/paginated/'
  url = string.gsub(url, 'contents', 'reader')
  if http.get(url..(workid+1)) then
  	local img = TXQuery.create(http.document).xpathstring('//*[@class="content-image lazy"]/@data-original')
    task.pagelinks[workid]=module.rooturl..img
    return true
  end
  return false
end

function getnameandlink()
  local s = '/section/all?page='..IncStr(url)
  if http.GET(module.RootURL .. s) then
    local x = TXQuery.Create(http.Document)
    local v = x.xpath('//*[@class="content-title truncate"]/a')
    local hasTitles = false
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(v1.getAttribute('href'))
      names.add(v1.getAttribute('title'))
      hasTitles = true
    end
    if hasTitles then
      updatelist.CurrentDirectoryPageNumber = updatelist.CurrentDirectoryPageNumber + 1
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='H-Sites'
  m.website='TMOHentai'
  m.rooturl='https://www.tmohentai.com'
  m.lastupdated='May 20, 2019'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetimageurl='getimageurl'
  m.ongetnameandlink='getnameandlink'
  m.sortedlist=true
end

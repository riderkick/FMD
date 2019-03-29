local domain = 'pururin.io'

function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.Document)
    mangainfo.title=x.XPathString('//*[@class="title"]/h1')
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('//*[@class="cover-wrapper"]//v-lazy-image/@src'))
    mangainfo.artists = x.XPathStringAll('//table[contains(@class,"table-gallery-info")]//tr/td[contains(.,"Artist")]/following-sibling::td//a')
    mangainfo.genres = x.XPathStringAll('//table[contains(@class,"table-gallery-info")]//tr/td[contains(.,"Contents")]/following-sibling::td//a')
    mangainfo.chapterlinks.add(x.XPathString('//*[@class="gallery-action"]/a/@href'))
    mangainfo.chapternames.add(mangainfo.title)
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  local path = 'https://cdn.' .. domain .. '/assets/images/data'
  if http.get(MaybeFillHost(module.rooturl,url)) then
    local x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//gallery-read/@gallery')
    x.parsehtml(s)
    local ext = x.xpathstring('json(*).image_extension')
    local cnt = x.xpathstring('json(*).total_pages')
    local id = x.xpathstring('json(*).id')
    for i = 1, cnt do
      task.pagelinks.add(string.format('%s/%s/%d.%s', path, id, i, ext))
    end
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if http.get(module.rooturl..'/browse/newest?page='..IncStr(url)) then
    local x=TXQuery.Create(http.Document)
    local v=x.xpath('//*[@class="row-gallery"]/a')
    for i=1,v.count do
      local v1 = v.get(i)
      links.add(v1.getAttribute('href'))
      names.add(x.xpathstring('.//*[@class="title"]/text()[1]', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if http.GET(module.RootURL) then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('//ul[contains(@class,"pagination")]/li[last()-1]'))
    if page == nil then page = 1 end
    return true
  else
    return false
  end
end

function Init()
  m=NewModule()
  m.category='H-Sites'
  m.website='Pururin'
  m.rooturl='https://' .. domain
  m.lastupdated = 'March 29, 2019'
  m.sortedlist=true
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
end
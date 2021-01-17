function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.Document)
    mangainfo.title=x.XPathString('css("div#info > h1")')
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('//div[@id="cover"]//v-lazy-image/@src'))
    mangainfo.artists = x.XPathStringAll('//section[@id="tags"]/div[contains(text(), "Artist")]//a')
    mangainfo.genres = x.XPathStringAll('//section[@id="tags"]/div[contains(text(), "Tag")]//a')
    mangainfo.chapterlinks.add(mangainfo.url)
    mangainfo.chapternames.add(mangainfo.title)
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  local id = url:match('g/(%d+)')
  http.mimetype = 'application/json'
  if http.post(module.rooturl..'/api/getBookByID', string.format('{"id":%s}', id)) then
    local x=TXQuery.Create(http.Document)
    local srv=x.xpathstring('json(*).results.image_server')
    local pages=tonumber(x.xpathstring('json(*).results.total_page'))
    for i = 1, pages do
      task.pagelinks.add(string.format('%s%s/%d.jpg', srv, id, i))
    end
    return true
  else
    return false
  end
end

query = '{"search":{"text":"","page":%s,"sort":0,"pages":{"range":[0,10000]},"tag":{"text":"","type":1,"tags":[],"items":{"included":[],"excluded":[]}}}}'

function GetNameAndLink()
  http.mimetype = 'application/json'
  if http.post(module.rooturl..'/api/getBook', string.format(query, url)) then
    local x=TXQuery.Create(http.Document)
    local v = x.xpath('json(*).results()')
    for i = 1, v.count do
      local v1 = v.get(i)
      links.add(string.format('%s/g/%s', module.rooturl, x.xpathstring('id', v1)))
      names.add(x.xpathstring('title', v1))
    end
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  http.mimetype = 'application/json'
  if http.post(module.RootURL..'/api/getBook', string.format(query, '0')) then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('json(*).total_count'))
    if page == nil then page = 1 end
    return true
  else
    return false
  end
end

function Init()
  m=NewModule()
  m.category='H-Sites'
  m.website='9hentai'
  m.rooturl='https://9hentai.com'
  m.sortedlist=true
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
end 
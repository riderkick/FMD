function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)		
    mangainfo.title=x.xpathstring('//meta[@property="og:title"]/@content')
    mangainfo.coverlink=x.xpathstring('//meta[@property="og:image"]/@content')
    mangainfo.authors=x.xpathstring('//a[@class="post-account"]')
    mangainfo.summary=x.xpathstring('//p[@class="post-image-description"]')
    mangainfo.chapterlinks.add(url)
    mangainfo.chapternames.add(mangainfo.title)
    return no_error
  else
    return net_problem
  end
end

function taskstart()
  task.pagelinks.clear()
  return true
end

function getpagenumber()
  local hash = url:match('.*/(.+)$')
  if hash ~= nil then
    -- album
    if http.get(module.rooturl .. '/ajaxalbums/getimages/' .. hash .. '/hit.json') then
      local x = TXQuery.Create(http.Document)
      local v = x.xpath('json(*).data.images()')
      for i = 1, v.count do
        local v1 = v.get(i)
        task.pagelinks.add('https://i.imgur.com/' .. x.xpathstring('hash', v1) .. x.xpathstring('ext', v1))
      end
      return true
    else
      return false
    end
  else
    -- single image
    if http.get(MaybeFillHost(module.rooturl,url)) then
      x=TXQuery.Create(http.Document)
      x.xpathStringAll('//div[contains(@class,"post-image")]/a/img/concat("https:",@src)', task.pagelinks)
      return true
    else
      return false
    end
  end
end

function Init()
  m=NewModule()
  m.website='Imgur'
  m.rooturl='https://imgur.com'
  m.OnGetInfo='getinfo'
  m.OnTaskStart='taskstart'
  m.OnGetPageNumber='getpagenumber'
end

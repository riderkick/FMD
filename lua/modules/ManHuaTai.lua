function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)	
    mangainfo.title=x.xpathstring('//div[contains(@class, "mhjsbody")]/div/ul/li[contains(., "名称")]/substring-after(., "名称：")')
    mangainfo.coverlink=MaybeFillHost(module.rooturl,x.xpathstring('//div[@class="comic-cover"]/img/@src'))
    mangainfo.authors=x.xpathstring('//div[contains(@class, "mhjsbody")]/div/ul/li[contains(., "作者")]/substring-after(., "作者：")')
    mangainfo.genres=x.xpathstring('//div[contains(@class, "mhjsbody")]/div/ul/li[contains(., "类型")]/substring-after(., "类型：")')
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('//div[contains(@class, "mhjsbody")]/div/ul/li[contains(., "状态")]/substring-after(., "状态：")'), '连载至', '已完结');
    mangainfo.summary=x.xpathstringall('//div[contains(@class,"wz")]/div/text()', '')
    v=x.xpath('//div[@class="mhlistbody"]/ul')
    for i=v.count,1,-1 do
      v1=v.get(i)
      w = x.xpath('./li/a', v1)
      for j = 1, w.count do
        w1 = w.get(j)
        mangainfo.chapterlinks.add(mangainfo.url .. '/' .. w1.getAttribute('href'))
        mangainfo.chapternames.add(w1.getAttribute('title'))
      end
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  local servers = {
    'http://mhpic.mh51.com',
    'http://mhpic.manhualang.com',
    'http://mhpic.jumanhua.com',
    'http://mhpic.yyhao.com',
  }
  
  math.randomseed(os.time())
  math.random(); math.random(); math.random();
  
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(., "mh_info")]')
    local imgpath = GetBetween('imgpath:"', '",', s)
    imgpath = imgpath:gsub('\\\\', '\\'):gsub("\\'", "'"):gsub('\\"', '"')
    local pageid = tonumber(s:match('pageid:%s*(%d+)'))
    local start = tonumber(s:match('startimg:%s*(%d+)'))
    local total = tonumber(s:match('totalimg:%s*(%d+)'))
    local size = GetBetween('comic_size:"', '",', s)
    imgpath = imgpath:gsub('(.)',
      function (a)
        return string.char(string.byte(a) - pageid % 10)
      end
    )
    local srv = servers[math.random(#servers)]
    for i=start,total do
      local d = tostring(start+i-1) .. '.jpg' .. size
      task.pagelinks.add(srv .. '/comic/' .. imgpath .. d)
    end
    return true
  else
    return false
  end
end

function BeforeDownloadImage()
  http.headers.values['Referer'] = module.rooturl
  return true
end

function getdirectorypagenumber()
  if http.GET(module.RootURL .. '/all.html') then
    x = TXQuery.Create(http.Document)
    page = tonumber(x.XPathString('//div[@class="pages"]/a[last()-1]'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  if http.get(module.rooturl..'/all_p'..IncStr(url)..'.html') then
    TXQuery.Create(http.document).XPathHREFtitleAll('//a[contains(div/ul/li/@class, "title")]',links,names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Raw'
  m.website='ManHuaTai'
  m.rooturl='http://www.manhuatai.com'
  m.lastupdated='February 21, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
  m.ongetnameandlink='getnameandlink'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end 
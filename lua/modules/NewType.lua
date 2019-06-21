-- Get manga info and chapter list:
function getinfo()
  local j, v, x = nil
  
  mangainfo.url = MaybeFillHost(module.rooturl, url)
  http.cookies.values['contents_cid'] = string.gsub(url, '/contents/', ''):gsub('/', '')
  http.cookies.values['contents_detail_pg'] = '1000'
  if http.get(mangainfo.url) then
    x = TXQuery.Create(http.document)
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('//figure/img/@src'))
    mangainfo.title = x.xpathstring('//div[@class="Breadcrumb"]/ul/li/div[@class="current"]/text()')
    mangainfo.summary = x.xpathstring('//section[@id="workInfo"]/p')
    v = x.xpath('//section[@id="episodeWrap"]//li[@class="ListCard"]/a')
    for i = 1, v.count do
      v1 = v.get(i)
      mangainfo.chapterlinks.add(v1.getattribute('href'))
      mangainfo.chapternames.add(x.xpathstring('div[@class="ListCard-content"]/h3', v1) .. ' (' .. x.xpathstring('div[@class="ListCard-info-nodot"]', v1) .. ')')
    end
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
    
    return no_error
  end
  return net_problem
end

-- Get page number and page container links:
function getpagenumber()
  local x = nil

  if http.get(MaybeFillHost(module.rooturl, url)) then
    if http.get(MaybeFillHost(module.rooturl, TXQuery.Create(http.document).xpathstring('//div[@class="ViewerContainer"]/@data-url'))) then
      x = TXQuery.Create(http.document)
      x.parsehtml(GetBetween('{', '}', x.xpathstring('//*')))
      x.xpathstringall('json(*)()', task.pagelinks)
      for i = 0, task.pagelinks.count do
        task.pagelinks[i] = MaybeFillHost(module.rooturl, task.pagelinks[i])
      end
    else
      return false
    end
    
    return true
  end
  return false
end

-- Go through all directory pages and get names and links for manga entries:
function getnameandlink()
  local v, x = nil
  
  http.cookies.values['contents_list_pg'] = '1000'
  if http.get(module.rooturl .. '/contents/all') then
    x = TXQuery.Create(http.document)
    v = x.xpath('//li[@class="OblongCard--border"]/a')
    for i = 1, v.count do
      v1 = v.get(i)
      links.add(v1.getattribute('href'))
      names.add(x.xpathstring('div/h3[@class="OblongCard-title"]', v1))
    end
    
    return no_error
  end
  return net_problem
end

-- Initialize module:
function Init()
  m=NewModule()
  m.category='Raw'
  m.website='NewType'
  m.rooturl='https://comic.webnewtype.com'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink = 'getnameandlink'
end

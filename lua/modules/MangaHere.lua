function GetInfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  http.cookies.values['isAdult'] = '1'
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.Document)
    if mangainfo.title == '' then
      mangainfo.title=x.XPathString('//span[@class="detail-info-right-title-font"]')
    end
    mangainfo.coverLink=MaybeFillHost(module.rooturl, x.XPathString('//img[@class="detail-info-cover-img"]/@src'))
    mangainfo.status = MangaInfoStatusIfPos(x.XPathString('css("table.table-borderless")//tr[th="Status"]/td'))
    mangainfo.authors = x.XPathString('//p[@class="detail-info-right-say"]/a')
    mangainfo.genres = x.XPathStringAll('//p[@class="detail-info-right-tag-list"]/a')
    mangainfo.summary = x.XPathString('//p[@class="fullcontent"]')
    mangainfo.status = MangaInfoStatusIfPos(x.XPathString('//span[@class="detail-info-right-title-tip"]'))
    local v=x.xpath('//ul[@class="detail-main-list"]/li/a')
    for i=1, v.count do
      local v1=v.get(i)
      mangainfo.chapterlinks.add(v1.getAttribute('href'):gsub('1%.html', ''))
      mangainfo.chapternames.add(x.xpathstring('./div/p[@class="title3"]', v1))
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)    
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  http.cookies.values['isAdult'] = '1'
  local aurl = MaybeFillHost(module.rooturl, url) .. '1.html'
  if http.get(aurl) then
    local x=TXQuery.Create(http.Document)
    local s = x.XPathString('//script[contains(., "eval")]')
    s = 'var $=function(){return{val:function(){}}},newImgs,guidkey;' .. s
    s = s .. ';newImgs||guidkey;'
    local key = ExecJS(s)
    if string.len(key) > 16 then
      task.pagelinks.commatext = key
    else
      s = x.XPathString('//script[contains(., "chapterid")]');
      local cid = Trim(GetBetween('chapterid', ';', s):gsub('=', ''))
      task.pagenumber = tonumber(Trim(GetBetween('imagecount', ';', s):gsub('=', '')))
      if task.pagenumber == nil then task.pagenumber = 1 end
      local page = 1
      while page <= task.pagenumber do
        http.reset()
        http.headers.values['Pragma'] = 'no-cache'
        http.headers.values['Cache-Control'] = 'no-cache'
        http.headers.values['Referer'] = aurl
        s = string.format('chapterfun.ashx?cid=%s&page=%d&key=%s', cid, page, key)
        if http.xhr(MaybeFillHost(module.rooturl, url) .. s) then
          s = Trim(StreamToString(http.document))
          if s ~= '' then
            s = ExecJS(s .. ';d;')
            local lst = TStrings.Create()
            lst.commatext = s
            if page > 1 then lst.delete(0) end
            task.pagelinks.addtext(lst.text)
            lst = nil
          end
        end
        if task.pagelinks.count >= task.pagenumber then break end
        page = page + 1
        Sleep(3000)
      end
    end
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if http.get(module.rooturl..'/mangalist/') then
    local x=TXQuery.Create(http.Document)
    x.xpathhrefall('css(".browse-new-block > .browse-new-block-content > a")', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='English'
  m.website='MangaHere'
  m.rooturl='https://www.mangahere.cc'
  m.ongetinfo='GetInfo'
  m.ongetpagenumber='GetPageNumber'
  m.ongetnameandlink='GetNameAndLink'
end

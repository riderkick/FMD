function GetInfo()
  mangainfo.url = MaybeFillHost(module.rooturl, url)
  http.cookies.values['isAdult'] = '1'
  if http.get(mangainfo.url) then
    local x = TXQuery.Create(http.Document)
    mangainfo.title = x.XPathString('//img[@class="detail-bg-img"]/@alt')
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.XPathString('//img[@class="detail-info-cover-img"]/@src'))
    mangainfo.status    = MangaInfoStatusIfPos(x.XPathString('css("table.table-borderless")//tr[th="Status"]/td'))
    mangainfo.authors   = x.XPathString('//p[@class="detail-info-right-say"]/a')
    mangainfo.genres    = x.XPathStringAll('//p[@class="detail-info-right-tag-list"]/a')
    mangainfo.summary   = x.XPathString('//p[@class="fullcontent"]')
    mangainfo.status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="detail-info-right-title-tip"]'))
	for _, v in ipairs(x.xpathi('//ul[@class="detail-main-list"]/li/a')) do
      mangainfo.chapterlinks.add(v.getAttribute('href'):gsub('1%.html$', ''))
      mangainfo.chapternames.add(x.xpathstring('./div/p[@class="title3"]', v))
    end
    InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)    
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  task.pagelinks.clear()
  task.pagenumber = 0
  http.cookies.values['isAdult'] = '1'
  local aurl = MaybeFillHost(module.rooturl, url):gsub('1%.html$', '')
  local lurl = aurl .. '1.html'
  if http.get(lurl) then
    local x = TXQuery.Create(http.Document)
    local key = ExecJS('var $=function(){return{val:function(){}}},newImgs,guidkey;' .. x.XPathString('//script[contains(., "eval")]') .. ';newImgs||guidkey;')
    if key:len() > 16 then
      task.pagelinks.commatext = key
    else
      local s = x.XPathString('//script[contains(., "chapterid")]')
	  local cid = s:match('chapterid%s*=%s*(.-)%s*;') or '0'
	  task.pagenumber = tonumber(s:match('imagecount%s*=%s*(%d-)%s*;') or '0')
      if task.pagenumber == nil then task.pagenumber = 1 end
      local page = 1
      while page <= task.pagenumber do
        http.reset()
        http.headers.values['Pragma'] = 'no-cache'
        http.headers.values['Cache-Control'] = 'no-cache'
        http.headers.values['Referer'] = lurl
        if http.xhr(aurl .. string.format('chapterfun.ashx?cid=%s&page=%d&key=%s', cid, page, key)) then
		  s = http.document.tostring()
          if s ~= '' then
            s = ExecJS(s .. ';d;')
			for i in s:gmatch('[^,]+') do
			  task.pagelinks.add(i)
			end
          end
        end
		-- sometimes the server will give more images than the actual chapter have
		-- it is an ads or file not found(404 not found)
		-- remove invalid images here
		while task.pagelinks.count>task.pagenumber do
		  task.pagelinks.delete(task.pagelinks.count-1)
		end
        if task.pagelinks.count >= task.pagenumber then break end
        page = task.pagelinks.count + 1
        Sleep(2000) -- without minimum delay of 2 seconds server will only give 2 images for each xhr request
      end
    end
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if http.get(module.rooturl..'/mangalist/') then
    TXQuery.Create(http.Document).xpathhrefall('css(".browse-new-block > .browse-new-block-content > a")', links, names)
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

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document) 
    mangainfo.title=x.xpathstring('//*[@id="series-data"]//*[@class="series-title"]/h1')
    mangainfo.coverlink=x.xpathstring('//*[@id="series-data"]//img[@class="cover"]/@src')
    mangainfo.authors=x.xpathstring('//*[@id="series-data"]//*[@class="series-author"]/normalize-space(text())')
    mangainfo.genres=x.xpathstringall('//*[@id="series-data"]//ul[contains(@class, "tags")]/li/a')
    if x.xpathstring('//*[@id="series-data"]//*[@class="complete-series"]') == '' then
      mangainfo.status = '1'
    else
      mangainfo.status = '0'
    end
    mangainfo.summary=x.xpathstring('//*[@id="series-data"]//*[@class="series-desc"]')
    
    local id = x.xpathstring('//ul[@data-id-serie]/@data-id-serie')
    local page = 1
    while true do
      if http.xhr(module.RootURL..'/series/chapters_list.json?page='..tostring(page)..'&id_serie='..id) then
        if http.terminated then break end
        x=TXQuery.Create(http.document)
        if x.xpathstring('json(*).chapters'):lower() == 'false' then break end
        v=x.xpath('json(*).chapters()')
        for i=1,v.count do
          v1=v.get(i)
          w=x.xpath('./releases/*', v1)
          for j=1,w.count do
            w1=w.get(j)
			local s = x.xpathstring('./concat(number, " - ", chapter_name)', v1)
			local sc = x.xpathstring('./scanlators[1]/name', w1)
			if sc ~= '' then
			  s = s .. ' [' .. sc .. ']'
			end
			mangainfo.chapternames.add(s)
			mangainfo.chapterlinks.add(x.xpathstring('./link', w1))
          end
        end
        page=page+1
      else
        break
      end
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
    x=TXQuery.Create(http.Document)
	s=x.xpathstring('//script[contains(@src, "token=")]/@src')
	local token = s:match('%&token=(%w+)&?')
	local id = s:match('&id_release=(%w+)&?')
	if http.get(module.rooturl .. string.format('/leitor/pages/%s.json?key=%s', id, token)) then
	  x=TXQuery.Create(http.Document)
	  x.xpathstringall('json(*).images()', task.pagelinks)
    task.pagelinks[0] = MaybeFillHost(module.rooturl, task.pagelinks[0])
	else
	  return false
	end
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl..'/lista-de-mangas/ordenar-por-nome/todos?page=' .. IncStr(url)) then
    local x=TXQuery.Create(http.Document)
    local p=x.xpathstring('//ul[contains(@class,"content-pagination")]/li[last()-1]/a')
    p = tonumber(p)
    if p ~= nil then
      updatelist.CurrentDirectoryPageNumber = p
    end
    x.XPathHREFtitleAll('//ul[@class="seriesList"]/li/a', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Portuguese'
  m.website='LeitorNet'
  m.rooturl='https://leitor.net'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end 
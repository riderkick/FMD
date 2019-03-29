local domain = 'hitomi.la'

function set_https(s)
  if s:match('^//') then
    return 'https:' .. s
  else
    return s
  end
end

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(mangainfo.url) then
    local x=TXQuery.Create(http.document)
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('//div[starts-with(@class,"gallery")]/h1')
    end
    mangainfo.coverlink=MaybeFillHost(module.RootURL, x.xpathstring('//div[@class="cover"]//img/@src'))
    mangainfo.coverlink = set_https(mangainfo.coverlink)
    mangainfo.authors=x.xpathstringall('//div[starts-with(@class,"gallery")]/h2/ul/li/a')
    mangainfo.genres=x.xpathstringall('//div[@class="gallery-info"]/table//tr/td//a')
    mangainfo.chapterlinks.add(x.xpathstring('//div[contains(@class,"cover-column")]/a/@href'))
    mangainfo.chapternames.add(mangainfo.title)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  task.pagelinks.clear()
  if http.get(MaybeFillHost(module.rooturl, url)) then
	local subdomain = nil
	local isfixed = 0
    local galleryid = tonumber(ReplaceRegExpr('(?i)^.*reader/(\\d+).*$', url, '$1'))
	local prefix = string.char(97 + (galleryid % 2)) .. 'a.'
    if galleryid ~= nil then
      subdomain = prefix ..domain
    end
    local x=TXQuery.Create(http.Document)
    local v=x.xpath('//div[@class="img-url"]')
    for i=1,v.count do
      local s=v.get(i).toString
      if subdomain ~= nil then
        s=s:gsub('//[^/]+/', '//'..subdomain..'/')
      end
      s = set_https(s)
	  
	  if isfixed == 0 then
	    if http.get(s) then
	      err=TXQuery.Create(http.Document)
		  e = err.xpathstring('//center/h1')
		  if e == '403 Forbidden' then
		    prefix = string.char(97 + (galleryid % 2) - 1) .. 'a.'
		    subdomain = prefix ..domain
		    s=v.get(i).toString
		    s=s:gsub('//[^/]+/', '//'..subdomain..'/')
		    s = set_https(s)
		  end
		  isfixed = 1
		end
	  end
      task.pagelinks.add(s)
    end
  else
    return false
  end
  return true
end

function getnameandlink()
  if http.get(module.rooturl) then
    local x = TXQuery.Create(http.Document)
    if http.get('https://ltn.'..domain..'/index-all.nozomi') then
      local s = StreamToString(http.document)
      for i=1,s:len(),4 do
        local b1,b2,b3,b4=s:byte(i),s:byte(i+1),s:byte(i+2),s:byte(i+3)
        local n = b4 + (b3 << 8) + (b2 << 16) + (b1 << 24)
        links.add('https://'..domain..'/galleries/'..n..'.html')
        names.add('')
      end
      return no_error
    end
  end
  return net_problem
end

function Init()
  local m = NewModule()
  m.website = 'HitomiLa'
  m.rooturl = 'https://'..domain
  m.category = 'H-Sites'
  m.lastupdated = 'March 29, 2019'
  m.sortedlist=true
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
end
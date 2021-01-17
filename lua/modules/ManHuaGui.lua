local js = require 'utils.jsunpack'
local lz = require 'utils.lzstring'

function getinfo()
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  http.cookies.values['isAdult']=' 1'
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)	
	
	mangainfo.coverlink = MaybeFillHost(module.rooturl,x.xpathstring('//p[@class="hcover"]/img/@src'))
	mangainfo.title     = x.xpathstring('//div[@class="book-title"]/h1')
	mangainfo.authors   = SeparateRight(x.xpathstring('//ul[@class="detail-list cf"]/li[2]/span[2]'), '：')
	mangainfo.genres    = SeparateRight(x.xpathstring('//ul[@class="detail-list cf"]/li[2]/span[1]'), '：')
	mangainfo.status    = MangaInfoStatusIfPos(x.xpathstring('//ul[@class="detail-list cf"]/li[@class="status"]'), '连载中')
	mangainfo.summary   = x.xpathstring('//div[@id="intro-all"]')

	if x.xpath('//*[@id="checkAdult"]').count ~= 0 then
		local s = x.xpathstring('//*[contains(@class,"chapter")]/input/@value')
		if s~='' then x.parsehtml(lz.decompressFromBase64(s)) end
	end
	x.xpathhreftitleall('//*[contains(@id,"chapter-list")]/ul/li/a', mangainfo.chapterlinks, mangainfo.chapternames)
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  local servers = {
    'http://i.hamreus.com',
    'http://us.hamreus.com',
    'http://dx.hamreus.com',
    'http://eu.hamreus.com',
    'http://lt.hamreus.com',
  }
  
  math.randomseed(os.time())
  math.random(); math.random(); math.random();
  
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    local s = x.xpathstring('//script[contains(., "p,a,c,k")]')
    s = SeparateRight(s, "}('")
    local text = SeparateLeft(s, "',");
    local a = tonumber(GetBetween("',", ",", s))
    s = SeparateRight(s, "',")
    local c = tonumber(GetBetween(",", ",'", s))
    local w = js.splitstr(lz.decompressFromBase64(GetBetween(",'", "'", s)), '|')
    s = js.unpack36(text, a, c, w)
    s = s:gsub('^var%s+.+=%s*{', '{'):gsub('||{};$', ''):gsub('"status":,', '')
    s = GetBetween("SMH.imgData(", ").preInit();", s)
    x.parsehtml(s)
    local cid = x.xpathstring('json(*).cid')
    local md5 = x.xpathstring('json(*).sl.md5')
    local path = x.xpathstring('json(*).path')
    local srv = servers[math.random(#servers)]
	local v for _, v in ipairs(x.xpathi('json(*).files()')) do
		task.pagelinks.add(srv .. path .. v.toString .. '?cid=' .. cid .. '&md5=' .. md5)
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
  if http.GET(module.RootURL .. '/list/') then
    x = TXQuery.Create(http.Document)
    local s = x.XPathString('//div[contains(@id, "AspNetPager")]/a[last()]/@href')
    page = tonumber(s:match('%d+'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  if http.get(module.rooturl..'/list/index_p'..IncStr(url)..'.html') then
    TXQuery.Create(http.document).xpathhrefall('//ul[@id="contList"]/li/p/a',links,names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Raw'
  m.website='ManHuaGui'
  m.rooturl='https://www.manhuagui.com'
  m.lastupdated='February 21, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetdirectorypagenumber='getdirectorypagenumber'
  m.ongetnameandlink='getnameandlink'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end 

function getinfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  http.cookies.values['mangadex_h_toggle'] = '1'
  local id = url:match('title/(%d+)')
  if id == nil then id = url:match('manga/(%d+)'); end
  delay()
  if http.get(MaybeFillHost(module.rooturl, '/api/manga/' .. id)) then
    local resp = HTMLEncode(StreamToString(http.document))
    local x = TXQuery.Create(resp)
    
    local info = x.xpath('json(*)')
    if mangainfo.title == '' then
      mangainfo.title = x.xpathstring('manga/title', info)
    end
    mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('manga/cover_url', info))
    mangainfo.authors = x.xpathstring('manga/author', info)
    mangainfo.artists = x.xpathstring('manga/artist', info)
    mangainfo.summary = x.xpathstring('manga/description', info)
    mangainfo.status = MangaInfoStatusIfPos(x.xpathstring('manga/status', info), '1', '2')
    
    local genres = ''
    local v = x.xpath('jn:members(manga/genres)', info)
    if v.count > 0 then genres = getgenre(v.get(1).toString); end
    for i = 2, v.count do
      local v1 = v.get(i)
      genres = genres .. ', ' .. getgenre(v1.toString)
    end
    if x.xpathstring('manga/hentai', info) == '1' then
      if genres ~= '' then genres = genres .. ', ' end
      genres = genres .. 'Hentai'
    end
    mangainfo.genres = genres
    
    local selLang = module.getoption('lualang')
    local selLangId = findlang(selLang)
    local chapters = x.xpath('let $c := json(*).chapter return for $k in jn:keys($c) ' ..
      'return jn:object(object(("chapter_id", $k)), $c($k))')
    for i = 1, chapters.count do
      local v1 = chapters.get(i)
      
      if not IgnoreChaptersByGroupId(x.xpathstring('group_id', v1)) then
        
        local lang = x.xpathstring('lang_code', v1)
        local ts = tonumber(x.xpathstring('timestamp', v1))
        if (selLang == 0 or lang == selLangId) and (ts <= os.time()) then
          mangainfo.chapterlinks.add('/chapter/' .. x.xpathstring('chapter_id', v1))
          
          local s = ''
          local vol = x.xpathstring('volume', v1)
          local ch = x.xpathstring('chapter', v1)
          if vol ~= '' then s = s .. string.format('Vol. %s', vol); end
          if s ~= '' then s = s .. ' '; end
          if ch ~= '' then s = s .. string.format('Ch. %s', ch); end
          
          local title = x.xpathstring('title', v1)
          if title ~= '' then
            if s ~= '' then s = s .. ' - '; end
            s = s .. title
          end
          
          if selLang == 0 then
            s = string.format('%s [%s]', s, getlang(lang))
          end
          
          if module.getoption('luashowscangroup') then
            local group = x.xpathstring('group_name', v1)
            local group2 = x.xpathstring('group_name_2', v1)
            local group3 = x.xpathstring('group_name_3', v1)
            if group2:len() > 0 and group2 ~= 'null' then
              group = group .. ' | ' .. group2
            end
            if group3:len() > 0 and group3 ~= 'null' then
              group = group .. ' | ' .. group3
            end
            s = string.format('%s [%s]', s, group)
          end
          
          mangainfo.chapternames.add(s)
        end
      end
    end
    InvertStrings(mangainfo.chapterlinks,mangainfo.chapternames)
    return no_error
  else
    return net_problem
  end
end

function IgnoreChaptersByGroupId(id)
  local groups = {
    ["9097"] = "MangaPlus"
  }
  
  if groups[id] ~= nil then
    return true
  else
    return false
  end
end

function getgenre(genre)
  local genres = {
    ["1"] = "4-koma",
    ["2"] = "Action",
    ["3"] = "Adventure",
    ["4"] = "Award Winning",
    ["5"] = "Comedy",
    ["6"] = "Cooking",
    ["7"] = "Doujinshi",
    ["8"] = "Drama",
    ["9"] = "Ecchi",
    ["10"] = "Fantasy",
    ["11"] = "Gyaru",
    ["12"] = "Harem",
    ["13"] = "Historical",
    ["14"] = "Horror",
    ["15"] = "Josei",
    ["16"] = "Martial Arts",
    ["17"] = "Mecha",
    ["18"] = "Medical",
    ["19"] = "Music",
    ["20"] = "Mystery",
    ["21"] = "Oneshot",
    ["22"] = "Psychological",
    ["23"] = "Romance",
    ["24"] = "School Life",
    ["25"] = "Sci-Fi",
    ["26"] = "Seinen",
    ["27"] = "Shoujo",
    ["28"] = "Shoujo Ai",
    ["29"] = "Shounen",
    ["30"] = "Shounen Ai",
    ["31"] = "Slice of Life",
    ["32"] = "Smut",
    ["33"] = "Sports",
    ["34"] = "Supernatural",
    ["35"] = "Tragedy",
    ["36"] = "Long Strip",
    ["37"] = "Yaoi",
    ["38"] = "Yuri",
    ["39"] = "[no chapters]",
    ["40"] = "Video Games",
    ["41"] = "Isekai",
    ["42"] = "Adaptation",
    ["43"] = "Anthology",
    ["44"] = "Web Comic",
    ["45"] = "Full Color",
    ["46"] = "User Created",
    ["47"] = "Official Colored",
    ["48"] = "Fan Colored",
    ["49"] = "Gore",
    ["50"] = "Sexual Violence",
    ["51"] = "Crime",
    ["52"] = "Magical Girls",
    ["53"] = "Philosophical",
    ["54"] = "Superhero",
    ["55"] = "Thriller",
    ["56"] = "Wuxia",
    ["57"] = "Aliens",
    ["58"] = "Animals",
    ["59"] = "Crossdressing",
    ["60"] = "Demons",
    ["61"] = "Delinquents",
    ["62"] = "Genderswap",
    ["63"] = "Ghosts",
    ["64"] = "Monster Girls",
    ["65"] = "Loli",
    ["66"] = "Magic",
    ["67"] = "Military",
    ["68"] = "Monsters",
    ["69"] = "Ninja",
    ["70"] = "Office Workers",
    ["71"] = "Police",
    ["72"] = "Post-Apocalyptic",
    ["73"] = "Reincarnation",
    ["74"] = "Reverse Harem",
    ["75"] = "Samurai",
    ["76"] = "Shota",
    ["77"] = "Survival",
    ["78"] = "Time Travel",
    ["79"] = "Vampires",
    ["80"] = "Traditional Games",
    ["81"] = "Virtual Reality",
    ["82"] = "Zombies",
    ["83"] = "Incest",
    ["84"] = "Mafia",
    ["85"] = "Villainess"		
  }
  if genres[genre] ~= nil then
    return genres[genre]
  else
    return genre
  end
end

local langs = {
  ["sa"] = "Arabic",
  ["bd"] = "Bengali",
  ["bg"] = "Bulgarian",
  ["mm"] = "Burmese",
  ["ct"] = "Catalan",
  ["cn"] = "Chinese (Simp)",
  ["hk"] = "Chinese (Trad)",
  ["cz"] = "Czech",
  ["dk"] = "Danish",
  ["nl"] = "Dutch",
  ["gb"] = "English",
  ["ph"] = "Filipino",
  ["fi"] = "Finnish",
  ["fr"] = "French",
  ["de"] = "German",
  ["gr"] = "Greek",
  ["hu"] = "Hungarian",
  ["id"] = "Indonesian",
  ["it"] = "Italian",
  ["jp"] = "Japanese",
  ["kr"] = "Korean",
  ["my"] = "Malay",
  ["mn"] = "Mongolian",
  ["ir"] = "Persian",
  ["pl"] = "Polish",
  ["br"] = "Portuguese (Br)",
  ["pt"] = "Portuguese (Pt)",
  ["ro"] = "Romanian",
  ["ru"] = "Russian",
  ["rs"] = "Serbo-Croatian",
  ["es"] = "Spanish (Es)",
  ["mx"] = "Spanish (LATAM)",
  ["se"] = "Swedish",
  ["th"] = "Thai",
  ["tr"] = "Turkish",
  ["ua"] = "Ukrainian",
  ["vn"] = "Vietnamese"
}

function getlang(lang)
  if langs[lang] ~= nil then
    return langs[lang]
  else
    return 'Unknown'
  end
end

function getlanglist()
  local t = {}
  for k, v in pairs(langs) do table.insert(t, v); end
  table.sort(t)
  return t
end

function findlang(lang)
  local t = getlanglist()
  for i, v in ipairs(t) do
    if i == lang then
      lang = v
      break
    end
  end
  for k, v in pairs(langs) do
    if v == lang then return k; end
  end
  return nil
end

function getpagenumber()
  http.cookies.values['mangadex_h_toggle'] = '1'
  local chapterid = url:match('chapter/(%d+)')
  delay()
  if http.get(MaybeFillHost(module.rooturl,'/api/chapter/'..chapterid)) then
    local x=TXQuery.Create(http.Document)
    x.ParseHTML(StreamToString(http.Document):gsub('<', ''):gsub('>', ''):gsub('&quot;', ''))
    local hash = x.xpathstring('json(*).hash')
    local srv = x.xpathstring('json(*).server')
	if srv:sub(-1) ~= '/' then srv = srv .. '/' end
    local v = x.xpath('json(*).page_array()')
    for i = 1, v.count do
      local v1 = v.get(i)
      local s = MaybeFillHost(module.rooturl, srv .. hash .. '/' .. v1.toString)
      task.pagelinks.add(s)
    end
    return true
  else
    return false
  end
  return true
end

local dirurl='/titles/2'

function getdirectorypagenumber()
  http.cookies.values['mangadex_h_toggle'] = '1'
  http.cookies.values['mangadex_title_mode'] = '2'
  delay()
  if http.GET(module.RootURL .. dirurl) then
    local x = TXQuery.Create(http.Document)
    page = tonumber(x.xpathstring('(//ul[contains(@class,"pagination")]/li/a)[last()]/@href'):match('/2/(%d+)'))
    if page == nil then page = 1 end
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  http.cookies.values['mangadex_h_toggle'] = '1'
  http.cookies.values['mangadex_title_mode'] = '2'
  delay()
  if http.GET(module.rooturl .. dirurl .. '/' .. IncStr(url) .. '/') then
    local x = TXQuery.Create(http.document)
    x.xpathhrefall('//a[contains(@class, "manga_title")]',links,names)
    return no_error
  else
    return net_problem
  end
end

function delay()
  local interval = tonumber(module.getoption('luainterval'))
  local delay = tonumber(module.getoption('luadelay')) -- * module.ActiveConnectionCount
  
  if (interval == nil) or (interval < 0) then interval = 1000; end
  if (delay == nil) or (delay < 0) then delay = 1000; end
  
  local lastDelay = module.storage['lastDelay']
  if lastDelay ~= '' then
    lastDelay = tonumber(lastDelay)
    if GetCurrentTime() - lastDelay < interval then
    print(GetCurrentTime() - lastDelay)
      Sleep(delay)
    end
  end
  
  module.storage['lastDelay'] = tostring(GetCurrentTime())
end

function getFormData(formData)
	local t=tostring(os.time())
	local b=string.rep('-',39-t:len())..t
	local crlf=string.char(13)..string.char(10)
	local r=''
	for k,v in pairs(formData) do
		r=r..'--'..b..crlf..
			'Content-Disposition: form-data; name="'..k..'"'..crlf..
			crlf..
			v..crlf
	end
	r=r..'--'..b..'--'..crlf
	return 'multipart/form-data; boundary='..b,r
end

function Login()
	module.ClearCookies()
	module.account.status=asChecking
	local login_url=module.rooturl..'/login'
	if not http.GET(login_url) then
		module.account.status=asUnknown
		return false
	end
	local login_post_url=TXQuery.Create(http.document).xpathstring('//form[@id="login_form"]/@action') or ''
	if login_post_url=='' then
		module.account.status=asUnknown
		return false
	end
	login_post_url=module.rooturl..login_post_url:gsub('&nojs=1','')
	http.reset()
	
	http.headers.values['Origin']= ' '..module.rooturl
	http.headers.values['Referer']= ' '..login_url
	http.headers.values['Accept']=' */*'
	http.headers.values['X-Requested-With']=' XMLHttpRequest'
	
	local post_data
	http.mimetype,post_data=getFormData({
		login_username=module.account.username,
		login_password=module.account.password,
		two_factor='',
		remember_me='1'})

	http.POST(login_post_url,post_data)
	if http.resultcode==200 then
		if http.cookies.values['mangadex_rememberme_token']~='' then
			module.account.status=asValid
		else
			module.account.status=asInvalid
		end
	else
		module.account.status=asUnknown
	end
	return true
end

function Init()
  m=NewModule()
  m.category='English'
  m.website='MangaDex'
  m.rooturl='https://mangadex.org'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetnameandlink='getnameandlink'
  m.ongetdirectorypagenumber = 'getdirectorypagenumber'
  
  m.maxtasklimit=1
  m.maxconnectionlimit=2

  m.addoptionspinedit('luainterval', 'Min. interval between requests (ms)', 1000)
  m.addoptionspinedit('luadelay', 'Delay (ms)', 1000)
  m.addoptioncheckbox('luashowscangroup', 'Show scanlation group', false)
  
  m.AccountSupport=true
  m.OnLogin='Login'
  
  local items = 'All'
  local t = getlanglist()
  for k, v in ipairs(t) do items = items .. '\r\n' .. v; end
  m.addoptioncombobox('lualang', 'Language:', items, 11)
end

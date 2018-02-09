local dirurl = '/directory/'
local dirurlreader = '/reader/directory/'
local dirurlfoolslide = '/foolslide/directory/'
local dirurlslide = '/slide/directory/'
local dirurlslideU = '/Slide/directory/'
local dirurlonline = '/online/directory/'
local dirurlhelvetica = '/r/directory/'
local dirurllector = '/lector/directory/'

function getWithCookie(lurl)
    local needCookie = {
        ['SeinagiAdultoFansub'] = true,
        ['TripleSevenScan'] = true,
        ['DokiFansubs'] = true,
        ['RavensScans'] = true,
        ['YamiTenshiNoFansub'] = true
    }
    if needCookie[module.website] and Pos(dirurl, lurl) then
        return http.post(lurl, 'adult=true')
    else
        return http.get(lurl)
    end
end

function getdirurl(website)
    local dirs = {
        ['GoManga'] = dirurlreader,
        ['Jaiminisbox'] = dirurlreader,
        ['TripleSevenScan'] = dirurlreader,
        ['DokiFansubs'] = dirurlreader,
        ['AtelierDuNoir'] = dirurlreader,
        ['OneTimeScans'] = dirurlfoolslide,
        ['DejameProbar'] = dirurlslide,
        ['MenudoFansub'] = dirurlslide,
        ['NeoProjectScan'] = dirurlslide,
        ['SolitarioNoFansub'] = dirurlslide,
        ['SantosScan'] = dirurlslideU,
        ['Pzykosis666HFansub'] = dirurlonline,
        ['SeinagiFansub'] = dirurlonline,
        ['HelveticaScans'] = dirurlhelvetica,
        ['RavensScans'] = dirurllector,
        ['NoraNoFansub'] = dirurllector
    }    
    if dirs[website] ~= nil then
        return dirs[website]
    else
        return dirurl
    end
end

function getinfo()
    local lurl = MaybeFillHost(module.rooturl, url)
    local result = net_error
    if getWithCookie(lurl) then
        x = TXQuery.Create(http.document)
        mangainfo.coverlink = x.xpathstring('//div[@class="thumbnail"]/img/@src')
        if mangainfo.title == '' then
            if module.website == 'AtelierDuNoir' then
                mangainfo.title = x.xpathstring('//div[@class="section-headline"]//h3')
            else
                mangainfo.title = x.xpathstring('//h1[@class="title"]')
            end
        end
        if Pos('emailprotected', mangainfo.title) > 0 then
            mangainfo.title = Trim(SeparateLeft(x.xpathstring('//title'), '::'))
        end
        mangainfo.authors = string.gsub(
            x.xpathstring('//div[@class="info"]/*[contains(text(),"Author")]/following-sibling::text()[1]'),
            '^[%s:]*', '')
        mangainfo.artists = string.gsub(
            x.xpathstring('//div[@class="info"]/*[contains(text(),"Artist")]/following-sibling::text()[1]'),
            '^[%s:]*', '')
        mangainfo.summary = string.gsub(
            x.xpathstring('//div[@class="info"]/*[contains(text(),"Synopsis")]/following-sibling::text()[1]'),
            '^[%s:]*', '')
        v = x.xpath('//div[@class="list"]//div[@class="title"]/a')
        for i = 1, v.count do
            v1 = v.get(i)
            mangainfo.chapterlinks.add(v1.getattribute('href'))
            if v1.getattribute('title') ~= '' then
                mangainfo.chapternames.add(v1.getattribute('title'))
            else
                mangainfo.chapternames.add(v1.ToString)
            end
        end
        InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
        result = no_error
    end
    return result
end

function taskstart()
    task.pagelinks.clear()
    task.pagenumber = 0
    return true
end

function getpagenumber()
    local result = false
    if getWithCookie(MaybeFillHost(module.rooturl, url)) then
        x = TXQuery.create(http.document)
        task.pagenumber = x.xpath('//div[@class="topbar_right"]//ul[@class="dropdown"]/li').count
        s = x.xpathstring('//script[contains(.,"var pages")]')
        if s ~= '' then
            s = GetBetween('var pages = ', ';', s)
            if Pos('atob("', s) > 0 then
               s = GetBetween('atob("', '")', s)
               s = DecodeBase64(s)  
            end
            x.parsehtml(s)
            v = x.xpath('json(*)()("url")')
            for i = 1, v.count do
                task.pagelinks.add(v.get(i).ToString)
            end
        end
        result = true
    end
    return result
end

function getimageurl()
    local result = false
    local s = url
    if workid > 0 then
        s = AppendURLDelim(s) .. 'page/' .. (workid + 1)
    end
    if getWithCookie(MaybeFillHost(module.rooturl, s)) then
        x = TXQuery.create(http.document)
        task.pagelinks.set(workid, x.XPathString('//div[@id="page"]//img/@src'))
    end
    return result
end

function getdirectorypagenumber()
    local result = net_error
    page = 1
    if getWithCookie(module.rooturl .. getdirurl(module.website)) then
        result = no_error
        x = TXQuery.create(http.document)
        v = x.xpath('//*[@class="next"]/a/@href')
        for i = 1, v.count do
            local s = tonumber(string.match(v.get(i).tostring, '/(%d+)/$'))
            if (s ~= nil) and (s > page) then
                page = s
            end
        end
    end
    return result
end

function getnameandlink()
    local result = net_error
    local s = module.rooturl .. getdirurl(module.website)
    if url ~= '0' then
        s = s .. (tonumber(url) + 1) .. '/'
    end
    if getWithCookie(s) then
        result = no_error
        x = TXQuery.create(http.document)
        if module.website == 'AtelierDuNoir' then
            v = x.xpath('//div[@class="caption"]')
            for i = 1, v.count do
                v1 = v.get(i)
                links.add(x.xpathstring('div/a/@href', v1))
                names.add(x.xpathstring('h4', v1))
            end
        else
            x.XpathHREFAll('//div[@class="list series"]/div/div[@class="title"]/a', links, names)
        end
    end
    return result
end

function AddWebsiteModule(name, url)
    local m = NewModule()
    m.website = name
    m.rooturl = url
    m.lastupdated = 'february, 6 2018'
    m.ongetinfo = 'getinfo'
    m.OnTaskStart = 'taskstart'
    m.OnGetPageNumber = 'getpagenumber'
    m.OnGetImageURL = 'getimageurl'
    m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
    m.OnGetNameAndLink = 'getnameandlink'
    return m
end

function Init()
    AddWebsiteModule('PowerManga', 'http://read.powermanga.org')
    AddWebsiteModule('Shoujosense', 'http://reader.shoujosense.com')
    AddWebsiteModule('GoManga', 'http://gomanga.co')
    AddWebsiteModule('OneTimeScans', 'http://otscans.com')
    AddWebsiteModule('SenseScans', 'http://reader.sensescans.com')
    AddWebsiteModule('Jaiminisbox', 'https://jaiminisbox.com')
    AddWebsiteModule('KireiCake', 'https://reader.kireicake.com')
    AddWebsiteModule('HelveticaScans', 'http://helveticascans.com')
    AddWebsiteModule('WhiteoutScans', 'http://reader.whiteoutscans.com')
    AddWebsiteModule('DokiFansubs', 'https://kobato.hologfx.com')
    AddWebsiteModule('AtelierDuNoir', 'http://atelierdunoir.org')
    AddWebsiteModule('ChampionScans', 'http://reader.championscans.com')
    AddWebsiteModule('WorldThree', 'http://www.slide.world-three.org')
    
    -- es-sc
    AddWebsiteModule('DangoOnlineNoFansub', 'http://lector.dangolinenofansub.com')
    AddWebsiteModule('DejameProbar', 'http://dejameprobar.es')
    AddWebsiteModule('HoshinoFansub', 'http://manga.animefrontline.com')
    AddWebsiteModule('MenudoFansub', 'http://www.menudo-fansub.com')
    AddWebsiteModule('NeoProjectScan', 'http://npscan.mangaea.net')
    AddWebsiteModule('Pzykosis666HFansub', 'http://pzykosis666hfansub.com')
    AddWebsiteModule('SantosScan', 'http://santosfansub.com')
    AddWebsiteModule('SeinagiFansub', 'http://seinagi.org')
    AddWebsiteModule('SeinagiAdultoFansub', 'http://adulto.seinagi.org')
    AddWebsiteModule('SolitarioNoFansub', 'http://snf.mangaea.net')
    AddWebsiteModule('TripleSevenScan', 'http://triplesevenscans.com')
    AddWebsiteModule('RavensScans', 'http://ravens-scans.com')
    AddWebsiteModule('KirishimaFansub', 'http://lector.kirishimafansub.com')
    AddWebsiteModule('NoraNoFansub', 'https://www.noranofansub.com')
    AddWebsiteModule('YamiTenshiNoFansub', 'http://lector.ytnofan.com')
end

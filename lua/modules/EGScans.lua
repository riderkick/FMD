function getinfo()
    local lurl = MaybeFillHost(module.rooturl, url)
    local result = net_problem
    if http.get(lurl) then
        local x = TXQuery.Create(http.document)
        if mangainfo.title == '' then
            mangainfo.title = x.xpathstring('//select[@name="manga"]/option[@selected]')
        end
        local v = x.xpath('//select[@name="chapter"]/option')
        for i = 1, v.count do
            local v1 = v.get(i)
            mangainfo.chapternames.add(v1.ToString)
            mangainfo.chapterlinks.add(url .. '/' .. v1.getattribute('value'))
        end
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
    local s = url .. '&display=webtoon'
    s = MaybeFillHost(module.rooturl, s)
    if http.get(s) then
        local x = TXQuery.create(http.document)
        local v = x.xpath('//td/a/img[@class="picture"]/@src')
        for i = 1, v.count do
            local v1 = v.get(i)
            task.pagelinks.add(MaybeFillHost(module.rooturl, v1.toString))
        end
        result = true
    end
    return result
end

function getdirectorypagenumber()
    page = 1
    return no_error
end

function getnameandlink()
    local result = net_problem
    if http.get(module.rooturl) then
        local x = TXQuery.create(http.document)
        local v = x.xpath('//select[@name="manga"]/option[@value!="0"]')
        for i = 1, v.count do
            local v1 = v.get(i)
            links.add(module.rooturl .. '/' .. v1.getattribute('value'))
            names.add(v1.ToString)
        end
        result = no_error
    end
    return result
end

function Init()
    local m = NewModule()
    m.category = 'English-Scanlation'
    m.website = 'EGScans'
    m.rooturl = 'http://read.egscans.com'    
    m.lastupdated = 'february, 10 2018'
    m.ongetinfo = 'getinfo'
    m.OnTaskStart = 'taskstart'
    m.OnGetPageNumber = 'getpagenumber'
    m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
    m.OnGetNameAndLink = 'getnameandlink'
end
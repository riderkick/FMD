function getinfo()
  local s = ''
  mangainfo.url = MaybeFillHost(module.rooturl, url)
  if http.get(mangainfo.url) then  
    x = TXQuery.Create(http.document)
    mangainfo.coverLink = MaybeFillHost(module.rooturl, x.XPathString('//img[starts-with(@class,"cvr")]/@src'))
    mangainfo.title = x.XPathString('//*[@itemprop="itemreviewed"]')
    mangainfo.authors = x.XPathString('//*[contains(@class,"mng_det")]//*[self::p or self::li][starts-with(.,"Author")]/substring-after(normalize-space(.)," ")')
    mangainfo.artists = x.XPathString('//*[contains(@class,"mng_det")]//*[self::p or self::li][starts-with(.,"Artist")]/substring-after(normalize-space(.)," ")')
    mangainfo.status = MangaInfoStatusIfPos(x.XPathString('//*[contains(@class,"mng_det")]//*[self::p or self::li][starts-with(.,"Status")]/substring-after(normalize-space(.)," ")'))
    mangainfo.summary = x.XPathString('//div[@class="det"]/p[1]')
    if (module.website == 'ReadHentaiManga') then
      mangainfo.genres = x.XPathString('string-join(//*[contains(@class,"mng_det")]//*[self::p or self::li]//a,", ")')
    else
      mangainfo.genres = x.XPathString('//*[contains(@class,"mng_det")]//*[self::p or self::li][starts-with(.,"Category")]/string-join((./*[position()>1]),", ")')
    end
    if module.website == 'MangaOnlineToday' then
      mangainfo.summary = x.XPathString('//div[contains(@class,"mng_det")]/p[1]')
      x.xpathhrefall('//ul[@class="chp_lst"]/li/a', mangainfo.chapterLinks, mangainfo.chapterNames)
    else
      while true do      
        v = x.XPath('//a[@class="lst"]')
        for i = 1, v.count do
          v2 = v.get(i)
          mangainfo.chapterLinks.Add(v2.getAttribute('href'))
          s = v2.getAttribute('title')
          if s == '' then
            s = x.XPathString('*[@class="val"]', v2)
          end
          if s == '' then
            s = x.XPathString('text()[1]', v2)
          end
          mangainfo.chapterNames.Add(s)
        end
        if http.terminated then break end
        s = Trim(x.XPathString('//*[@class="pgg"]//*[./a[@class="sel"]]/following-sibling::*[./a]/a/@href'))
        if s == '' then break end
        if http.GET(MaybeFillHost(module.rooturl, s)) then
          x.ParseHTML(http.Document)
        else
          break
        end
      end
      InvertStrings(mangainfo.chapterLinks, mangainfo.chapterNames)
    end
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  local s = ''
  local allnum = false
  http.Cookies.Values['viewer'] = '1'
  if http.GET(AppendURLDelim(MaybeFillHost(module.RootURL, url)) .. '1') then      
    -- multi page
    x = TXQuery.Create(http.Document)
    s = x.XPathString('//script[contains(.,"imglist")]/substring-after(substring-before(.,"]"),"[")')
    if s ~= '' then
      s = '[' .. s .. ']'
    else
      s = x.XPathString('//script[contains(.,"img_lst")]/substring-after(substring-before(.,"\')"),"(\'")')
      if s ~= '' then
        s = DecodeURL(s)
      end
    end
    x.ParseHTML(s)
    x.XPathStringAll('json(*)()("url")', task.PageLinks)
    if task.PageLinks.Count == 0 then
      x.XPathStringAll('json(*)()', task.PageLinks)
    end
    
    -- single page
    if task.PageLinks.Count == 0 then
      x.ParseHTML(http.Document)
      task.PageNumber = x.XPath('(//select[@class="cbo_wpm_pag"])[1]/option').Count
      if task.PageNumber == 0 then
        task.PageNumber = x.XPath('(//select[@name="page"])[1]/option').Count
      end
      if task.PageNumber == 0 then
        v = x.XPath('//select')
        for i = 1, v.count do
          allnum = true
          v2 = x.XPath('option', v.get(i))
          for i = 1, v2.count do
            if tointeger(v2.toString) == -1 then
              allnum = false
              break
            end
          end
          if allnum then
            task.PageNumber = x.XPath('option', v).Count
            break
          end
        end
      end      
    end
    return true
  else
    return false
  end   
end

function getimageurl()
  local s = ''
  if http.GET(AppendURLDelim(MaybeFillHost(module.RootURL, url)) .. IncStr(workid) .. '/') then
    x = TXQuery.Create(http.Document)        
    if module.Website == 'ReadHentaiManga' then
      s = HTMLDecode(x.XPathString('//img[@id="main_img"]/@src'))
    else
      s = x.XPathString('//*[contains(@class,"mng_rdr")]//img/@src')
    end
    if s == '' then
      s = x.XPathString('//*[@id="reader"]//img[@id="picture"]/@src')
    end
    task.PageLinks[workid] = s
    return true
  else
    return false
  end
end

function getdirurl(website)
  local result = ''
  if (website == 'MangaSpy') or (website == 'MangaIce') then
    result = 'manga_list'
  elseif (website == 'ReadHentaiManga') then
    result = 'hentai-manga-list'
  else
    result = 'manga-list'
  end
  return '/' .. result .. '/all/any/last-added/'
end

function getdirectorypagenumber()
  if http.GET(AppendURLDelim(module.RootURL) .. getdirurl(module.website)) then
    x = TXQuery.Create(http.Document)
    page = tonumber(ReplaceRegExpr('^.*\\/(\\d+)/.*$', x.XPathString('//ul[@class="pgg"]/li[last()]/a/@href'), '$1'))
    if page == nil then
      page = 1
    end
    return true
  else
    return false
  end
end

function getnameandlink()
  local w = {
    ['MangaSpy'] = true,
    ['MangaIce'] = true,
    ['MangaDeep'] = true,
    ['Manga99'] = true
  }
  if http.GET(AppendURLDelim(module.RootURL) .. getdirurl(module.website) .. IncStr(url) .. '/') then
    x = TXQuery.Create(http.Document)
    if w[module.website] then
      x.XPathHREFAll('//*[contains(@id,"content")]//*[@class="det"]/a', links, names)
    elseif module.website == 'MangaOnlineToday' then
      x.XPathHREFAll('//*[contains(@id,"content")]//div[@class="box"]/ul/li/a', links, names)
    else
      x.XPathHREFtitleAll('//*[contains(@id,"content")]//a[./img]', links, names);
    end
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(name, url, category)
  local m = NewModule()
  m.website = name
  m.rooturl = url
  m.category = category
  m.sortedlist = true
  m.OnGetInfo = 'getinfo'
  m.OnGetPageNumber = 'getpagenumber'
  m.OnGetImageURL = 'getimageurl'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
  m.OnGetNameAndLink = 'getnameandlink'
  return m
end

function Init()
  cat = 'H-Sites'
  AddWebsiteModule('ReadHentaiManga', 'http://readhentaimanga.com', cat)
  
  cat = "Arabic-Scanlation"
  AddWebsiteModule('3asq', 'http://www.3asq.info', cat)
end

function getinfo()
  mangainfo.url=MaybeFillHost(module.rooturl,url)
  if http.get(mangainfo.url) then
    x=TXQuery.Create(http.document)
    mangainfo.coverlink=MaybeFillHost(module.rooturl,x.xpathstring('//div[contains(@class,"thumbnail")]/div[1]/img/@src'))
    mangainfo.title=x.xpathstring('//div[contains(@class,"thumbnail")]/div[2]/div[1]/h1')
    mangainfo.genres=x.xpathstring('//div[contains(@class,"thumbnail")]/div[2]/div[1]/string-join(./a,", ")')
    mangainfo.summary=x.xpathstring('//div[contains(@class,"thumbnail")]/div[2]/div[1]/p/substring-after(.,"Sinopsis: ")')
    local lurl=''
    while true do
      x.xpathstringall('//ul[contains(@class,"list-group")]/a/@href',mangainfo.chapterlinks)
      x.xpathstringall('//ul[contains(@class,"list-group")]/a/text()[1]',mangainfo.chapternames)
      lurl=x.xpathstring('//ul[@class="pagination"]/li[@class="active"]/following-sibling::li[not(@class)]/a/@href')
      if http.terminated then break end;
      if lurl~='' then
        if http.get(MaybeFillHost(module.rooturl,lurl)) then
          x.parsehtml(http.document)
        else
          break
        end
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
  if http.get(MaybeFillHost(module.rooturl,url)) then
    x=TXQuery.Create(http.Document)
    task.pagenumber=tonumber(x.xpathstring('//ul[@class="pagination"]/li[not(./a/@rel)][last()]'))
    return true
  else
    return false
  end
end

function getimageurl()
  local lurl=MaybeFillHost(module.rooturl,url)
  if workid~=0 then lurl=lurl..'/?page='..workid+1 end
  if http.get(lurl) then
    x=TXQuery.Create(http.Document)
    task.pagelinks[workid]=MaybeFillHost(module.rooturl,x.xpathstring('//div[@class="card-body"]//img/@src'))
    return true
  else
    return false
  end
end

function getnameandlink()
  if http.get(module.rooturl..'/novelas') then
    x=TXQuery.Create(http.Document)
    x.xpathhrefall('//div[contains(@class,"thumbnail")]/div[2]/div[1]/a[1]', links, names)
    return no_error
  else
    return net_problem
  end
end

function Init()
  m=NewModule()
  m.category='Spanish-Scanlation'
  m.website='SKSubs'
  m.rooturl='http://sksubs.com'
  m.lastupdated='February 28, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.ongetimageurl='getimageurl'
  m.ongetnameandlink='getnameandlink'
end

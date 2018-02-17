local apiurl='https://api.manga.sh/api/v1/'
local cdnurl='https://cdn.manga.sh/'

function getinfo()
  local lid=RegExprGetMatch('/(\\d+)',url,1)
  local lurl=MaybeFillHost(module.RootURL, apiurl..'series?query=Id:'..lid)
  mangainfo.url=MaybeFillHost(module.RootURL, url)
  if http.get(lurl) then
    x=TXQuery.Create(http.document)		
    v=x.xpath('json(*).response(1)')
    mangainfo.title=x.xpathstring('Name', v)
    mangainfo.coverlink=x.xpathstring('CoverImage', v)
    if mangainfo.coverlink ~= '' then
      mangainfo.coverlink=cdnurl..'covers/'..mangainfo.coverlink
    end
    mangainfo.status=MangaInfoStatusIfPos(x.xpathstring('Status/Name', v))
    mangainfo.genres=x.xpathstring('string-join((TypeName,TypeDemonym,SeriesTags/TagName),", ")', v)
    mangainfo.summary=HTMLDecode(x.xpathstring('Description', v))
    if http.get(apiurl..'series_chapters?query=SeriesId.Id:'..lid..'&order=desc&sortby=TimeUploaded&limit=0&offset=0') then
      x.parsehtml(http.document)
      local s='json(.)("response")()'
      local lname=''
      local showalllang=module.getoption('showalllang')
      local showscangroup=module.getoption('showscangroup')      
      if not showalllang then
        s=s..'[ChapterLanguage/Name = "English"]'
      end
      v=x.xpath(s)
      for i=1,v.count do
        v2=v.get(i)
        lname=x.xpathstring('concat("Vol.",VolumeNumber," Ch.",ChapterNumberAbsolute," ",ChapterNumberVolume)', v2)
        if showalllang then
          s=x.xpathstring('ChapterLanguage/Name',v2)
          if s~='' then lname=lname..' ['..s..']' end
        end
        if showscangroup then
          s=x.xpathstring('string-join(SeriesChaptersGroups/GroupName,", ")',v2)
          if s~='' then lname=lname..' ['..s..']' end
        end
        mangainfo.chapterlinks.add(x.xpathstring('Hash', v2))
        mangainfo.chapternames.add(lname)
      end
      return no_error
    else
      return net_problem
    end
  else
    return net_problem
  end
end

function getpagenumber()
  if http.get(apiurl..'series_chapters/'..RemoveURLDelimLeft(url)) then
    TXQuery.Create(http.Document).xpathstringall('json(.)/response/SeriesChaptersFiles/Name',task.pagelinks)
    for i=0,task.pagelinks.count-1 do
      task.pagelinks[i]=cdnurl..task.pagelinks[i]
    end
    return true
  else
    return false
  end
  return true
end

function Init()
  m=NewModule()
  m.website='MangaSh'
  m.rooturl='https://manga.sh'
  m.lastupdated='February 14, 2018'
  m.ongetinfo='getinfo'
  m.ongetpagenumber='getpagenumber'
  m.addoptioncheckbox('showalllang', 'Show all language', false)
  m.addoptioncheckbox('showscangroup', 'Show scanlation group', false)
end

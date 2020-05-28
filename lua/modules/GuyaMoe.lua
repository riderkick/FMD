----------------------------------------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------------------------------------

-- Extract slug from script text
function GetSlug(x)
  local slug = x.XPathString('//script[contains(., "let slug")]')
  slug = GetBetween('let slug', ';', slug)
  slug = slug:gsub('"', ''):gsub('%s*=%s*', '')
  return slug
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local u = MaybeFillHost(module.RootURL, url)
  mangainfo.Url = u

  if not http.Get(u) then return net_problem end

  local x = TXQuery.Create(http.Document)
  local slug = GetSlug(x)

  u = string.format('%s/api/series/%s/', module.rooturl, slug)
  if not http.Get(u) then return net_problem end

  x = TXQuery.Create(http.Document)
  local json = x.xpath('json(*)')

  mangainfo.Title     = x.XPathString('title', json)
  mangainfo.CoverLink = MaybeFillHost(module.RootURL, x.XPathString('cover', json))
  mangainfo.Authors   = x.XPathString('author', json)
  mangainfo.Artists   = x.XPathString('artist', json)
  mangainfo.Summary   = x.XPathString('description', json)

  local chapters = [[
  for $k in jn:keys(chapters)
  return jn:object(object(("chapter_id", $k)), (chapters)($k))
  ]]

  for _, v in ipairs(x.XPathI(chapters, json)) do
    local id = x.XPathString('chapter_id', v)
    for _, w in ipairs(x.XPathI('jn:keys(groups)', v)) do
      local group_id = w.toString
      local group = x.XPathString('(groups)(' .. group_id .. ')', json)
      local link = string.format('/read/manga/%s/%s/#%s', slug, id, group_id)
      local title = string.format('%s - %s [%s]', id, x.XPathString('title', v), group)
      mangainfo.ChapterLinks.Add(link)
      mangainfo.ChapterNames.Add(title)
    end
  end

  return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
  task.PageLinks.Clear()
  task.PageNumber = 0

  local group_id = url:match('/#(%d+)$')
  local u = MaybeFillHost(module.RootURL, url:gsub('/#%d+$', ''))

  if not http.Get(u) then return false end

  local x = TXQuery.Create(http.Document)
  local slug = GetSlug(x)
  local ch = u:match('/(%d+)/?$')

  u = string.format('%s/api/series/%s/', module.RootURL, slug)
  if not http.Get(u) then return false end

  x = TXQuery.Create(http.Document)
  local json = x.XPath('(json(*).chapters)("' .. ch .. '")')
  local folder = x.XPathString('folder', json)

  for _, v in ipairs(x.XPathI('jn:members((groups)(' .. group_id .. '))', json)) do
    local link = string.format('%s/media/manga/%s/chapters/%s/%s/%s',
      module.RootURL, slug, folder, group_id, v.toString)
    task.PageLinks.Add(link)
  end

  return true
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
  if not http.Get(module.RootURL) then return net_problem end

  local x = TXQuery.Create(http.Document)
  x.XPathHREFAll('css("div.dropdown-menu > a")[contains(@href, "read/manga")]', links, names)

  return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------
function Init()
  local m = NewModule()
  m.Category          = 'English'
  m.Website           = 'GuyaMoe'
  m.RootURL           = 'https://guya.moe'
  m.OnGetInfo         = 'GetInfo'
  m.OnGetPageNumber   = 'GetPageNumber'
  m.OnGetNameAndLink  = 'GetNameAndLink'
end

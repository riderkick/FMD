function GetNameAndLink()
	if http.get(module.rooturl..'/titulos/') then
	    TXQuery.create(http.document).xpathhrefall('//div[@class="manga"]/p[1]/a', links, names)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	mangainfo.url = MaybeFillHost(module.rooturl, url)
	if http.get(mangainfo.url) then
		local x = TXQuery.create(http.document)
		
		mangainfo.coverlink = MaybeFillHost(module.rooturl, x.xpathstring('//*[@id="page-mangas"]//*[@class="image"]/img/@src'))
		mangainfo.title     = x.xpathstring('//h1')
		mangainfo.authors   = x.xpathstring('//*[@class="texto-left"]/ul/li[starts-with(.,"História")]/substring-after(.,":")')
		mangainfo.artists   = x.xpathstring('//*[@class="texto-left"]/ul/li[starts-with(.,"Ilustração")]/substring-after(.,":")')
		mangainfo.genres    = x.xpathstring('string-join(//*[@class="generos"]/a,", ")')
		mangainfo.summary   = x.xpathstring('//p[.="Sinopse"]/following-sibling::p')
		mangainfo.status    = MangaInfoStatusIfPos(x.xpathstring('//*[@class="texto-left"]/ul/li[starts-with(.,"Status")]'), 'Em Publicação', '')
		
		local v, w, s
		for _, v in ipairs(x.xpathi('//*[@id="volumes-capitulos"]//*[@class="texto"]')) do
			s = x.xpathstring('p[1]', v)
			for _, w in ipairs(x.xpathi('p[2]//a', v)) do
				mangainfo.chapterlinks.add(w.getattribute('href'))
				mangainfo.chapternames.add(s .. ' Capitulo ' .. w.tostring)
			end
		end
		InvertStrings(mangainfo.chapterlinks, mangainfo.chapternames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	local a, c = url:match('/([^/]+)/capitulo/(%d+)')
	if a and c then
		if http.get(module.rooturl .. '/capitulo.php?act=getImg&anime=' .. EncodeURLElement(a) .. '&capitulo=' .. c .. '&src=1&view=2') then
			local x = TXQuery.Create(http.document)
			local v, i = x.xpath('//*[@id="imgAvancadoVisualizacao"]/img')
			for i = 1, v.count do
				task.pagelinks.add(MaybeFillHost(module.rooturl, v.get(i).getattribute('src')))
			end
			return true
		else
			return false
		end
	end
end

function Init()
	m=NewModule()
	m.Website                    = 'MangaOnlineBR'
	m.RootURL                    = 'http://mangaonline.com.br'
	m.Category                   = 'Portuguese'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

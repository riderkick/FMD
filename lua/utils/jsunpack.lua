local _M = {}

local lookup36 = '0123456789abcdefghijklmnopqrstuvwxyz'

function encode36(code)
  local result = ''
  local i = 0
  repeat
    local digit = math.fmod(math.floor(code / (36^i)), 36)
    result = string.char(string.byte(lookup36, digit+1)) .. result
    code = code - digit * (36^i)
    i = i + 1
  until code <= 0
  return result
end

function getid(c, a)
  local result = ''
  if c < a then
    result = ''
  else
    result = getid(math.floor(c / a), a)
  end
  c = math.fmod(c, a)
  if c > 35 then
    result = result .. string.char(c + 29)
  else
    result = result .. encode36(c)
  end
  return result
end

function _M.unpack36(text, a, c, words)
  if (a == nil) or (c == nil) then return '' end
  if type(words) == 'string' then
    words = splitstr(words, '|')
  end
  local dict = {}
  while c ~= 0 do
    c = c - 1
    dict[getid(c, a)] = words[c+1]
  end
  return text:gsub('%f[%w](%w+)%f[%W]',
    function (w)
      if (dict[w] ~= nil) and (dict[w] ~= '') then
        return dict[w]
      else
        return w
      end
    end)
end

function _M.replacehex(text)
  return text:gsub('(\\x)([%da-f][%da-f]?)',
    function (x, w)
      return string.char(tonumber(w, 16))
    end
  )
end

function _M.splitstr(str, delimiter)
  local result = { }
  local from = 1
  local delim_from, delim_to = string.find(str, delimiter, from)
  while delim_from do
    table.insert(result, string.sub(str, from , delim_from-1))
    from = delim_to + 1
    delim_from, delim_to = string.find(str, delimiter, from)
  end
  table.insert(result, string.sub(str, from))
  return result
end

function Init()
end

return _M

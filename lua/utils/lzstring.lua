local _M = {}

local keyStrBase64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='
local dictBase64 = {}

function filldict()
  for i = 0, string.len(keyStrBase64)-1 do
    dictBase64[string.char(string.byte(keyStrBase64, i+1))] = i
  end
end

function getval64(input, index)
  return dictBase64[string.char(string.byte(input, index + 1))]
end

function _M.decompressFromBase64(input)
  if (not input) or (input == '') then return '' end
  return _decompress(input, 32, getval64)
end

function codepoint_to_utf8(c)
    assert((55296 > c or c > 57343) and c < 1114112, "Bad Unicode code point: "..c..".")
    if c < 128 then
        return string.char(c)
    elseif c < 2048 then
        return  string.char(math.floor(192 + c/64), 128 + c%64)
    elseif c < 55296 or 57343 < c and c < 65536 then
        return string.char(math.floor(224 + c/4096), math.floor(128 + c/64%64), 128 + c%64)
    elseif c < 1114112 then
        return string.char(math.floor(240 + c/262144), math.floor(128 + c/4096%64), math.floor(128 + c/64%64), 128 + c%64)
    end
end

function tochar(i)
  return codepoint_to_utf8(i)
end

function _decompress(input, rst, getval)
  local result, enlargeIn, numBits = '', 4, 3
  local maxpower, power, bits = 2^2, 1, 0
  local dictionary = {[0] = '0', [1] = '1', [2] = '2'}
  local dataval, datapos, dataindex = getval(input, 0), rst, 1
  local w, c, c2, entry, dsize = '\0', '\0', 0, '', 4

  while power ~= maxpower do
    resb = dataval & datapos
      datapos = datapos >> 1
      if datapos == 0 then
        datapos = rst
        dataval = getval(input, dataindex)
        dataindex = dataindex + 1;
      end
      if resb > 0 then bits = bits | power end
      power = power << 1
  end

  if bits == 0 then
    maxpower = 2^8
    power, bits = 1, 0
    while power ~= maxpower do
      resb = dataval & datapos
        datapos = datapos >> 1
        if datapos == 0 then
          datapos = rst
          dataval = getval(input, dataindex)
          dataindex = dataindex + 1;
        end
        if resb > 0 then bits = bits | power end
        power = power << 1
    end
    c = tochar(bits)
  elseif bits == 1 then
    maxpower = 2^16
    power, bits = 1, 0
    while power ~= maxpower do
      resb = dataval & datapos
        datapos = datapos >> 1
        if datapos == 0 then
          datapos = rst
          dataval = getval(input, dataindex)
          dataindex = dataindex + 1;
        end
        if resb > 0 then bits = bits | power end
        power = power << 1
    end
    c = tochar(bits)
  elseif bits == 2 then
    return ''
  end

  dictionary[3] = c
  w = c
  result = result .. c

  while true do
    if dataindex > string.len(input) then
      return ''
    end
    maxpower = 2^numBits
    power, bits = 1, 0
    while power ~= maxpower do
      resb = dataval & datapos
        datapos = datapos >> 1
        if datapos == 0 then
          datapos = rst
          dataval = getval(input, dataindex)
          dataindex = dataindex + 1;
        end
        if resb > 0 then bits = bits | power end
        power = power << 1
    end

    c2 = bits
    if bits == 0 then
      maxpower = 2^8
      power, bits = 1, 0
      while power ~= maxpower do
        resb = dataval & datapos
          datapos = datapos >> 1
          if datapos == 0 then
            datapos = rst
            dataval = getval(input, dataindex)
            dataindex = dataindex + 1;
          end
          if resb > 0 then bits = bits | power end
          power = power << 1
      end
      dictionary[dsize] = tochar(bits)
      c2 = dsize
      dsize = dsize + 1
      enlargeIn = enlargeIn - 1;
    elseif bits == 1 then
      maxpower = 2^16
      power, bits = 1, 0
      while power ~= maxpower do
        resb = dataval & datapos
          datapos = datapos >> 1
          if datapos == 0 then
            datapos = rst
            dataval = getval(input, dataindex)
            dataindex = dataindex + 1;
          end
          if resb > 0 then bits = bits | power end
          power = power << 1
      end
      dictionary[dsize] = tochar(bits)
      c2 = dsize
      dsize = dsize + 1
      enlargeIn = enlargeIn - 1;
    elseif bits == 2 then
      return result
    end

    if enlargeIn == 0 then
      enlargeIn = 2^numBits
      numBits = numBits + 1
    end

    if dsize - 1 >= c2 then
      entry = dictionary[c2]
    elseif c2 == dsize then
      entry = w .. string.char(string.byte(w,1))
    else
      return ''
    end

    result = result .. entry
    dictionary[dsize] = w .. string.char(string.byte(entry,1))
    dsize = dsize + 1
    enlargeIn = enlargeIn - 1
    w = entry
    if enlargeIn == 0 then
      enlargeIn = 2^numBits
      numBits = numBits + 1
    end
  end
end

function Init()
end

filldict()

return _M

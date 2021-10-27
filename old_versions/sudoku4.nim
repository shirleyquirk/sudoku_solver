import sugar
type
  Sudoku = object
    g: string
    empties: set[range[0..80]]
template `[]`(g:Sudoku,x:int,y:int):char = g.g[y*9+x]
proc toSudoku(s:string):Sudoku =
  result.g = s
  for i,c in s:
    if c == '.': result.empties.incl i

proc `$`(g:Sudoku):string =
  for k in 0..2:
    for i in k*3..k*3+2:
      for j in 0..2:
        result.add g.g[i*9+j*3..i*9+j*3+2] & "|"
      result[^1] = '\n'
    if k<2: result.add "---+---+---\n"

iterator square(g:Sudoku,x:int,y:int):char =
  let x = int(x/3) * 3
  let y = int(y/3) * 3
  for j in 0..2:
    for i in 0..2:
      yield g[x+i,y+j]

iterator horiz(g:Sudoku, y:int):char =
  for x in 0..8:
    yield g[x,y]

iterator vertiz(g:Sudoku, x:int): char =
  for y in 0..8:
    yield g[x,y]

template freeset(sg:untyped):set[char] =
  var x: set[char] = {'1'..'9','.'}
  for c in sg: x.excl c
  x

proc interset(g:Sudoku,x:int,y:int): set[char] =
    freeset(horiz(g,y)) * freeset(vertiz(g,x)) * freeset(square(g,x,y))

proc replace(g: var Sudoku, pos:int, car: char) =
  g.g[pos] = car
  if car == '.':
    g.empties.incl pos
  else:
    g.empties.excl pos
proc fill(g:var Sudoku, pos:int, car:char){.inline.} =
  g.g[pos] = car
  g.empties.excl pos
proc empty(g:var Sudoku, pos:int){.inline.} =
  g.g[pos] = '.'
  g.empties.incl pos
import strformat
proc first(s:set[range[0..80]]):cint =
  proc ctz(x:cuint):cint{.importc:"__builtin_ctz".}
  for n,i in cast[array[sizeof(s),uint8]](s):
    if i != 0:
      return ctz(i.cuint) + cint(8*n)
      #[for j in 8*n..8*n+8:
        if j in s:
          let res2 = j
          assert res2==res1,&"{res2}!={res1} for {s}"
          return res2.cint
]#
when defined(useStrChr):
  proc find(s:Sudoku,c:char):int =
    proc strchr(cs:cstring,cc:cint):cstring{.importc:"strchr",header:"<string.h>".}
    cast[ByteAddress](strchr(s.cstring,c.cint)) - cast[ByteAddress](s.cstring)
else:
  #proc find(s:Sudoku,c:char):int{.borrow.}
  proc find(s:Sudoku):int = (if s.empties.card == 0: -1 else: s.empties.first)

proc initSet[T]():set[T] = discard
#proc findAll(s:Sudoku):set[uint8] = collect(initSet,for i,c in s.string: if c=='.':{i})
#best index is the one with the fewest . on the rows and columns (and squares)
var tries = 0
proc resolv(g:var Sudoku):bool =# string =
    inc tries
    let i = g.find#g.find('.')
    if i>=0:
        var f = interset(g,i mod 9,int(i/9))
        for elem in f:
            g.fill(i,elem)#g.replace(i,elem)
            if resolv(g): return true
        g.empty(i)#g.replace(i,'.')
    else:
        return true
var g="""..1....8......45....5.7......273..........2..358.1....2.3.56...9.......1..6.9.7..""".toSudoku

import times
echo g
let
  hsets = collect(for i in 0..8:(i,freeset(horiz(g,i)).len))
  vsets = collect(for i in 0..8:(i,freeset(vertiz(g,i)).len))
  ssets = collect(for x in [0,3,6]:
    for y in [0,3,6]:
      (x div 3 + 3*(y div 3),freeset(square(g,x,y)).len))
echo hsets
echo vsets
echo ssets
let t = getTime()
doAssert resolv(g)
echo g, " took: ", getTime() - t, "in ",tries," tries"

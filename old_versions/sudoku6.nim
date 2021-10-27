import sugar,algorithm,deques
type
  Sudoku = object
    g: string
    when defined(emptySet):
      empties: set[0..127]
    else:
      emptyqueue: seq[int]
    #rows: array[9,int]
    #cols: array[9,int]
    #squares: array[9,int]
    #emptyqueue: seq[int]#seq[tuple[rating:int,idx:int]]#Deque[int]

template `[]`(g:Sudoku,x:int,y:int):char = g.g[y*9+x]

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


#proc rating(g:Sudoku,x,y:int):int = (if (x+9*y) in g.empties: 27 - (g.rows[y] + g.cols[x] + g.squares[x div 3 + 3 * (y div 3)] ) else: ord('.')-ord('a'))
#[
proc init(g:var Sudoku) =
  g.empties = {}
  for i,c in g.g:
    if c == '.': g.empties.incl i
  for y in 0..8:
    g.rows[y] = freeset(horiz(g,y)).card
  for x in 0..8:
    g.cols[x] = freeset(horiz(g,x)).card
  for y in 0..2:
    for x in 0..2:
      g.squares[x+3*y] = freeset(square(g,x*3,y*3)).card
  var ratings = collect(for e in g.empties:(rating:g.rating(e mod 9,e div 9),idx:e.int))
  #sort(ratings)
  g.emptyqueue = ratings# collect(for i in ratings:i.idx).toDeque
]#
proc initSet[T]():set[range[0..127]] = discard
proc toSudoku(s:string):Sudoku =
  result.g = s
  when defined(emptySet):
    result.empties = collect(initSet,for i,c in s:
      if c == '.': {i})
  else:
    result.emptyqueue = collect(for i,c in s:
      if c == '.': i)
proc `$`(g:Sudoku):string =
  for k in 0..2:
    for i in k*3..k*3+2:
      for j in 0..2:
        result.add g.g[i*9+j*3..i*9+j*3+2] & "|"
      result[^1] = '\n'
    if k<2: result.add "---+---+---\n"
  result.add "\n"
#[
  for y in 0..8:
    for x in 0..8:
      result.add chr(g.rating(x,y) + ord('a'))
    result.add '\n'
]#
proc interset(g:Sudoku,x:int,y:int): set[char] =
    freeset(horiz(g,y)) * freeset(vertiz(g,x)) * freeset(square(g,x,y))

#[proc replace(g: var Sudoku, pos:int, car: char) =
  g.g[pos] = car
  if car == '.':
    g.empties.incl pos
  else:
    g.empties.excl pos
]#
proc fill(g:var Sudoku, pos:int, car:char){.inline.} =
  g.g[pos] = car
  #discard g.emptyqueue.pop
  #g.empties.excl pos
import random
proc empty(g:var Sudoku, pos:int){.inline.} =
  g.g[pos] = '.'
  when defined(emptySet):
    g.empties.incl pos
  else:
    g.emptyqueue.add pos#@[(rating:ord('?')-ord('a'),idx:pos)]# & g.emptyqueue
    sort(g.emptyqueue)
proc last(s:set[range[0..127]]):cint =
  proc clz(x:cuint):cint{.importc:"__builtin_clz".}
  proc clzl(x:culong):cint{.importc:"__builtin_clzl".}
  let a = cast[array[2,culong]](s)
  for n in countdown(1,0):
    let i = a[n]
    if i != 0:
      return (63 - clzl(i)) + cint(64*n)

proc first(s:set[range[0..127]]):cint =
  proc ctz(x:cuint):cint{.importc:"__builtin_ctz".}
  proc ctzl(x:culong):cint{.importc:"__builtin_ctzl".}
  when defined(ctzint):
    for n,i in cast[array[4,cuint]](s):
      if i != 0:
        return ctz(i) + cint(32*n)
  else:
    for n,i in cast[array[2,culong]](s):
      if i != 0:
        return ctzl(i) + cint(64*n)
      #[for j in 0..127:
        if j in s:
          let res2 = j.cint
          assert res2==res1,&"{res2}!={res1} for {s}"
          return res2
      ]#
  #3 more bytes
  #[for n,i in cast[array[11,uint8]](s)[8..10]:
    if i != 0:
      let res1 = ctz(i.cuint) + cint(8*n)
      for j in 64..80:
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
  #proc find(s:Sudoku):int = (if s.empties.card == 0: -1 else: s.empties.first)
  #proc find(s:Sudoku):int = (if s.emptyqueue.len == 0: -1 else: s.emptyqueue[^1].idx)
  discard
#proc findAll(s:Sudoku):set[uint8] = collect(initSet,for i,c in s.string: if c=='.':{i})
#best index is the one with the fewest . on the rows and columns (and squares)
var tries = 0
proc resolv(g:var Sudoku):bool =# string =
    inc tries
    #if tries mod 1000 == 0:
      #g.init
      #echo g
      #echo g.emptyqueue
    if (when defined(emptySet): g.empties.len else: g.emptyqueue.len) > 0:
      when defined(emptySet):
        let i = g.empties.last
        g.empties.excl i
      else:
        let i =g.emptyqueue.pop#g.emptyqueue.pop.idx
      var f = interset(g,i mod 9,int(i/9))
      for elem in f:
          g.fill(i,elem)#g.replace(i,elem)
          if resolv(g): return true
      g.empty(i)#g.replace(i,'.')
    else:
        return true
var g="""..1....8......45....5.7......273..........2..358.1....2.3.56...9.......1..6.9.7..""".toSudoku
#extra hard one:
g = "..43..8.78....2.3..3...6.....69.5..2.9..7..4................5.6.........389.5..7.".toSudoku
import times
echo g

#[let
  hsets = collect(for i in 0..8:(i,freeset(horiz(g,i)).len))
  vsets = collect(for i in 0..8:(i,freeset(vertiz(g,i)).len))
  ssets = collect(for x in [0,3,6]:
    for y in [0,3,6]:
      (x div 3 + 3*(y div 3),freeset(square(g,x,y)).len))
echo hsets
echo vsets
echo ssets
]#

when false:
  include sudokus
  import strutils
  var maxtime = getTime() - getTime()
  for ggg in gg.splitLines:
    g = ggg.toSudoku
    tries = 0
    let t = getTime()
    doAssert resolv(g)
    let deltat = getTime()-t
    if deltat > maxtime:
      echo ggg, " took: ", deltat, "in ",tries," tries"
      maxtime = deltat
var t = getTime()
doassert resolv(g)
echo g," took: ", getTime()-t, " in ",tries," tries"

g="..1....8......45....5.7......273..........2..358.1....2.3.56...9.......1..6.9.7..".toSudoku
t = getTime()
doAssert resolv(g)
echo g," took: ", getTime()-t, " in ",tries," tries"

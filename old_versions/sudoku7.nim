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


#rating



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

proc interset(g:Sudoku,x:int,y:int): set[char] =
    freeset(horiz(g,y)) * freeset(vertiz(g,x)) * freeset(square(g,x,y))

proc fill(g:var Sudoku, pos:int, car:char){.inline.} =
  g.g[pos] = car

proc empty(g:var Sudoku, pos:int){.inline.} =
  g.g[pos] = '.'
  when defined(emptySet):
    g.empties.incl pos
  else:
    g.emptyqueue.add pos#@[(rating:ord('?')-ord('a'),idx:pos)]# & g.emptyqueue
    sort(g.emptyqueue)

proc last(s:set[range[0..127]]):int{.inline.} =
  proc clz(x:cuint):cint{.importc:"__builtin_clz".}
  proc clzl(x:culong):cint{.importc:"__builtin_clzl".}
  let a = cast[array[2,culong]](s)
  for n in countdown(1,0):
    let i = a[n]
    if i != 0:
      return (63 - clzl(i)) + (64*n)

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

var tries = 0
proc ratings(g:Sudoku):seq[int] =
  let rows = collect(for y in 0..8: freeset(horiz(g,y)).card)
  let cols = collect(for x in 0..8: freeset(vertiz(g,x)).card)
  let squares = collect(for y in [0,3,6]:
    for x in [0,3,6]:
      freeset(square(g,x,y)).card
    )
  collect(for e in g.empties:
    let x = e mod 9
    let y = e div 9
    rows[y] + cols[x] + squares[x div 3 + 3 * (y div 3)]
    )

type Direction = enum
  dirForward,dirBackward
proc resolv(g:var Sudoku,dir:static Direction):bool =# string =
    inc tries
    if (when defined(emptySet): g.empties.len else: g.emptyqueue.len) > 0:
      when defined(emptySet):
        when dir == dirForward:
          let i = g.empties.first
        else:
          let i = g.empties.last
        g.empties.excl i
      else:
        let i =g.emptyqueue.pop
      for elem in g.interset(i mod 9,int(i/9)):
          g.fill(i,elem)
          if resolv(g,dir): return true
      g.empty(i)
    else:
        return true

proc resolvForwards(g:var Sudoku):bool = g.resolv(dirForward)
proc resolvBackwards(g:var Sudoku):bool = g.resolv(dirBackward)
import math
proc resolv(g:var Sudoku):bool =
  let ratings = g.ratings
  let s = sum(collect(for i in 0..39:ratings[i]-ratings[^(i+1)]))
  #let s = sum(collect(for i in 0..39:cmp(ratings[i],ratings[^(i+1)])))
  if s <= 0:
    g.resolvForwards()
  else:
    g.resolvBackwards()
proc resolvinv(g:var Sudoku):bool =
  let ratings = g.ratings
  let s = sum(collect(for i in 0..39:ratings[i] - ratings[^(i+1)]))
  #let s = sum(collect(for i in 0..39:cmp(ratings[i],ratings[^(i+1)])))
  if s <= 0:
    g.resolvBackwards()
  else:
    g.resolvForwards()
import times
  #(if (x+9*y) in g.empties: 27 - (g.rows[y] + g.cols[x] + g.squares[x div 3 + 3 * (y div 3)] ) else: ord('.')-ord('a'))

var maxtime = getTime() - getTime()
proc attempt(g:string) =
  var t = getTime()
  tries = 0
  var h = g.toSudoku
  #echo h
  #echo h.empties
  let forward = h.ratings

  #echo sum(h.ratings)
  #what's the difference between g and g.reversed? i thought it would be that rating thing but that didn't work
  #how about the balance? like, are more entries in the first half than the second half?
  #maybe the rating worked, we didn't really try
  doAssert resolv(h)
  let forward_time = getTime()-t
  #echo "forward took: ", forward_time, " in ", tries," tries"
  h = cast[string](g.reversed).toSudoku
  #echo h
  #echo h.empties
  let backward = h.ratings
  t = getTime()
  tries = 0
  doAssert resolv(h)
  let backward_time = getTime()-t
  let diff =(if forward_time > backward_time: forward_time.inMicroseconds.float / backward_time.inMicroseconds.float else: -1.0 * backward_time.inMicroseconds.float / forward_time.inMicroseconds.float)
  #if diff > 100 or diff < -100:
  #echo "backward took: ", backward_time, " in ", tries," tries"
  if abs((forward_time - backward_time).inMilliseconds) > 50 :#maxtime:
    echo g," took: ",forward_time, "forward, but ", backward_time," back"
    echo "ratings:",forward
    echo sum(collect(for i in 0..39:forward[i]-forward[^(i+1)]))
  if (forward_time.inMilliseconds > 50):
    echo g," took: ", forward_time.inMilliseconds, " forward and ", backward_time.inMilliseconds," back"
    h = g.toSudoku
    let rat = h.ratings
    t = getTime()
    tries = 0
    doAssert h.resolvinv()
    echo "inversed, took ",getTime()-t
    echo collect(for i in 0..39:rat[i]-rat[^(i+1)])
    echo "\n"
  #  echo forward_time.inMicroseconds.float / 1000.0 ,',',backward_time.inMicroseconds.float / 1000.0,' ',(if forward_time > backward_time: forward_time.inMicroseconds.float / backward_time.inMicroseconds.float else: -1.0 * backward_time.inMicroseconds.float / forward_time.inMicroseconds.float),' ', sum(collect(for (i,j) in zip(forward[0..39],backward[0..39]):i-j))

import sudokus
for s in gg: # ["..43..8.78....2.3..3...6.....69.5..2.9..7..4................5.6.........389.5..7.","..1....8......45....5.7......273..........2..358.1....2.3.56...9.......1..6.9.7.."]:
  s.attempt

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

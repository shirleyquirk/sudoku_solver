#clang vectorizes interset, gcc doesn't
import times,algorithm

type
  Base = uint16
  Sudoku = array[81,Base]#range[0'u8..9'u8]]#distinct string
proc toSudoku(s:string):Sudoku =
  for i,v in s:
    result[i] = (if v=='.': 0 else: ord(v) - ord('0'))

template `[]`(g:Sudoku,x:int,y:int):int8 = g[y*9+x]

proc toString(s:Sudoku):string =
  result = newString(81)
  for i,c in s:
    result[i] = (if c == 0:'.' else: chr(c+ord('0')))

proc `$`(g:Sudoku):string =
  let g = g.toString
  for k in 0..2:
    for i in k*3..k*3+2:
      for j in 0..2:
        result.add (g[i*9+j*3..i*9+j*3+2]) & "|"
      result[^1] = '\n'
    if k<2: result.add "---+---+---\n"

import neo,sugar,std/setutils

const basemask = 0xff
const intersett = (block:
  var seqseq:array[81,array[81,Base]]
  let tmp = collect(
  for y in 0..<9:
    for x in 0..<9:
      let row_start_idx = 9*y
      var res = newSeq[Base](81)#maybe floats are faster dunno
      #horizontal
      for i in row_start_idx..<row_start_idx+9:
        res[i] = basemask
      #vertical
      for j in 0..<9:
        res[9*j+x] = basemask
      #square
      let (sq_x,sq_y) = ((x div 3)*3,(y div 3)*3)
      for j in 0..2:
        for i in 0..2:
          res[sq_x+i + 9*(sq_y + j)] = basemask
      res)
  for y in 0..80:
    for x in 0..80:
      seqseq[x][y] = tmp[x][y]
  seqseq)

#rating
#[
const ratesett = (block:
  var seqseq:array[81,array[81,Base]]
  let tmp = collect(
    for y in 0..<9:
      for x in 0..<9:
        let row_start_idx = 9*y
        var res = newSeq[Base](81)
        #horiz
        for i in row_start_idx..<row_start_idx+9:
          res[i] += 1
        for j in 0..<9:
          res[9*j+x] += 1
        let (sq_x,sq_y) = ((x div 3)*3,(y div 3)*3)
        for j in 0..2:
          for i in 0..2:
            res[sq_x+i + 9*(sq_y + j)] += 1
        res[x + 9*y] = -10
        res)
  for y in 0..80:
    for x in 0..80:
      seqseq[x][y] = tmp[x][y]
  seqseq)
]#
#iterator pairs[N:static int,T](x,y:array[N,T]):auto =
#  for i in 0..<N:
#    yield (x[i],y[i])

iterator `|*|`(x:Sudoku,y:array[81,Base]):range[0.Base..9.Base]=
  for i in 0..<81:
    yield x[i] and y[i]
proc `*`(y:array[81,Base],x:Sudoku):array[81,Base] =
  for i in 0..<81:
    result[i] = x[i] * y[i]

proc interset(g:Sudoku,idx:int): set[range[Base(0)..Base(9)]] =
    #let res1 = freeset(horiz(g,y)) * freeset(vertiz(g,x)) * freeset(square(g,x,y))
  {range[Base(0)..Base(9)](1) .. 9} - (g |*| intersett[idx]).toSet
proc ratings(g:Sudoku): array[81,int] =
  ## size of interset at each index
  ## lower is better. 0 means its already filled in 9 is possible but unlikely
  for i in 0..80:
    result[i] = g.interset(i).len

proc empties(s:Sudoku):array[81,bool] = #its a set of indices set[0..80]
  #var res:array[81,bool]
  for i,m in s:
    result[i] = (m==0)
  #return cast[array[81,uint8]](res)
import math,tables
#[
proc row_rating(s:Sudoku,emptymask:array[81,bool],row:range[0..8]):int =
  let idx = row*9
  sum(collect(for i,x in s[idx..<idx+9]:
    if emptymask[i]:
      rates[i]))
]#
#ok, now we sort our rows. how can we do that. ugh.
import std/decls,sequtils
#proc toRow(s:var Sudoku):array[9,Row]
#proc cmp(a,b:Row):int = cmp(a.row_rating,b.row_rating)
proc swizzle(s:var Sudoku):seq[int]{.discardable.} =
  ## returns indices for unswizzling...somehow
  ## i mean, it's only one of 6 since all swizzles are in three
  type
    Row = array[9,Base]
    Swathe = array[27,Base]

    A = array[81,Base]
    B = array[3,Swathe]
    C = array[3,array[3,Row]]
    D{.union.} = object
      a:A
      b:B
      c:C
  let bySwathes{.byaddr.} = cast[ptr D](s.addr).b
  let byRows{.byaddr.} = cast[ptr D](s.addr).c

  let emptymask = s.empties
  var ratins = block:
    var res:array[81,int]
    for i,r in s.ratings:
      if emptymask[i]:
        res[i] =r
    res

  let swathe_rates = cast[array[3,array[27,int]]](ratins)
  let row_rates = cast[array[9,array[9,int]]](ratins)

  let swathe_rate_sums = swathe_rates.mapIt(sum(it))
  let row_rate_sums = row_rates.mapIt(sum(it))

  let rowTable = collect(for j in 0..2: collect(initTable,for i in 0..2:
      {byRows[j][i] : row_rate_sums[j*3+i]}))
  let indexTable = collect(for j in 0..2:collect(initTable,for i in 0..2:
      {byRows[j][i] : j*3+i}))
  for j in 0..2:
    byRows[j].sort((a,b:Row) => cmp(rowTable[j][a],rowTable[j][b]))
  let swatheIndices = collect(initTable,for j in 0..2:
    {bySwathes[j] : j})
  let swatheTable = collect(initTable,for i in 0..2:
    {bySwathes[i] : swathe_rate_sums[i]})

  bySwathes.sort((a,b:Swathe) => cmp(swatheTable[a],swatheTable[b]))
  let js = collect(for j in 0..2:
    swatheIndices[bySwathes[j]])
  result = collect(for j,jj in js:#j indices have swizzled so
    for i in 0..2:
      indexTable[jj][byRows[j][i]]
    )
proc unswizzle(s:var Sudoku,swizz:seq[int]) =
  type
    Row = array[9,Base]
    Swathe = array[27,Base]

    A = array[81,Base]
    B = array[3,Swathe]
    C = array[9,Row]
    D{.union.} = object
      a:A
      b:B
      c:C
  #let bySwathes{.byaddr.} = cast[ptr D](s.addr).b
  var result:Sudoku = s
  let resbyRows{.byaddr.} = cast[ptr D](result.addr).c
  let sbyRows = cast[D](s).c
  for i,idx in swizz:
    resbyRows[idx] = sbyRows[i]
  s = result
proc replace(g: var Sudoku, pos:int, car: Base){.inline.} = g[pos] = car

proc resolv(g:var Sudoku):bool =# string =
    let i = g.find(Base(0))
    if i>=0:
        var f = interset(g,i)
        for elem in f:
            g.replace(i,elem)
            if resolv(g): return true
        g.replace(i,0)
        #return false
    else:
        return true
import std/bitops
proc pop[T](s:var set[T]):T =
  #proc clzl(x:culong):cint{.importc:"__builtin_clzl".}
  const x = ceil(sizeof(s) / 8.0).int
  let a = cast[array[x,culong]](s)
  for n in countdown(x-1,0):
    let i = a[n]
    if i != 0:
      result = (63 - countLeadingZeroBits(i)) + (64*n)
      s.excl result
#var maxstacklen
proc resolvIterative(g:var Sudoku):bool =
  var stack = newSeqofCap[Sudoku](50)
  stack.add g
  while stack.len > 0:
    g = stack.pop
    let i = g.find(Base(0))
    if i < 0: return true
    for elem in interset(g,i):
      stack.add g.dup(replace(i,elem))
    #if stack.len > maxstacklen: maxstacklen = stack.len
proc resolvTailCall(g:var Sudoku,stack:var seq[Sudoku]):bool =
  if stack.len == 0:
    return false
  g = stack.pop
  let i = g.find(Base(0))
  if i < 0: return true
  for elem in g.interset(i):
    stack.add g.dup(replace(i,elem))
  return resolvTailCall(g,stack)

proc resolvTailCall(g:var Sudoku):bool =
  var stack = @[g]
  return resolvTailCall(g,stack)
var g="""..1....8......45....5.7......273..........2..358.1....2.3.56...9.......1..6.9.7..""".toSudoku
echo g
import times
var t = getTime()
doAssert resolvIterative(g)
echo g, " took: ", getTime() - t

g = "..43..8.78....2.3..3...6.....69.5..2.9..7..4................5.6.........389.5..7.".dup(reverse).toSudoku
t = getTime()
doAssert resolvIterative(g)
echo g, " took: ", getTime() - t
g = "..43..8.78....2.3..3...6.....69.5..2.9..7..4................5.6.........389.5..7.".dup(reverse).toSudoku
echo g
t = getTime()
let swiz = g.swizzle()
echo g
doAssert resolvIterative(g)
g.unswizzle(swiz)
echo g, " took: ", getTime() - t
echo "\n---------------------------------------------------------------------\n\n"
import sudokus
let bigt = getTime()
for x in gg:
  #t = getTime()
  var h = x.toSudoku
  when defined(Swizzling):
    let swiz = h.swizzle()
  doAssert resolvIterative(h)
  when defined(Swizzling):
    h.unswizzle(swiz)
  #let d = getTime() - t
  #if d.inMilliseconds > 10:
  #  echo x.toSudoku, "\n=>\n",h," took ", d, "\n\n"
echo "all 1011 took:",getTime() - bigt

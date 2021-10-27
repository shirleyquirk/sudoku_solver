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

proc sortedness[T](x:openArray[T]):int = discard
  ## edit distance. nlogn
  ## length minus longest increasing subsequence range[0..<x.len]
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
type Row = array[9,Base]
type Swathe = array[27,Base]
#proc toRow(s:var Sudoku):array[9,Row]
#proc cmp(a,b:Row):int = cmp(a.row_rating,b.row_rating)
proc swizzle(s:var Sudoku) =
  ## returns indices for unswizzling
  type
    A = array[81,Base]
    B = array[3,Swathe]
    C{.union.} = object
      a:A
      b:B
  let rr{.byaddr.} = cast[ptr C](s.addr).b
  let emptymask = s.empties
  var ratins = block:
    var res:array[81,int]
    for i,r in s.ratings:
      if emptymask[i]:
        res[i] =r
    res
  let rates = cast[array[3,array[27,int]]](ratins)

  let ratesums = rates.mapIt(sum(it))
  let rowRates = collect(initTable,for i in 0..2:
    {rr[i] : ratesums[i]})
  #echo rowRates

  proc compare(a,b:Swathe):int = cmp(rowRates[a],rowRates[b])
  rr.sort(compare)
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
g.swizzle()
echo g
doAssert resolvIterative(g)
echo g, " took: ", getTime() - t

import sudokus
for x in gg:
  t = getTime()
  var h = x.toSudoku
  h.swizzle()
  doAssert resolvIterative(h)
  let d = getTime() - t
  if d.inMilliseconds > 20:
    echo x.toSudoku, "\n=>\n",h," took ", d, "\n\n"

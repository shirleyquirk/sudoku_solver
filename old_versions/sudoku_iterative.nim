#clang vectorizes interset, gcc doesn't

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

#iterator pairs[N:static int,T](x,y:array[N,T]):auto =
#  for i in 0..<N:
#    yield (x[i],y[i])

iterator `|*|`(x:Sudoku,y:array[81,Base]):range[0.Base..9.Base]=
  for i in 0..<81:
    yield x[i] and y[i]

proc interset(g:Sudoku,x:int,y:int): set[range[Base(0)..Base(9)]] =
    #let res1 = freeset(horiz(g,y)) * freeset(vertiz(g,x)) * freeset(square(g,x,y))
  {range[Base(0)..Base(9)](1) .. 9} - (g |*| intersett[x+9*y]).toSet

proc replace(g: var Sudoku, pos:int, car: Base){.inline.} = g[pos] = car

#when true:#defined(useStrChr):
#  proc find(s:Sudoku,c:char):int =
#    proc strchr(cs:cstring,cc:cint):cstring{.importc:"strchr",header:"<string.h>".}
#    cast[ByteAddress](strchr(s.cstring,c.cint)) - cast[ByteAddress](s.cstring)
#else:
#  proc find(s:Sudoku,c:char):int{.borrow.}
#

proc resolv(g:var Sudoku):bool =# string =
    let i = g.find(Base(0))
    if i>=0:
        var f = interset(g,i mod 9,int(i/9))
        for elem in f:
            g.replace(i,elem)
            #what's on the stack right here
            #i,f,elem,how far through f we are
            if resolv(g): return true #its the early exit that's hard
        g.replace(i,0)
        #return false
    else:
        return true
proc pop[T](s:var set[T]):T =
  proc clzl(x:culong):cint{.importc:"__builtin_clzl".}
  const x = ceil(sizeof(s) / 8.0).int
  let a = cast[array[x,culong]](s)
  for n in countdown(x-1,0):
    let i = a[n]
    if i != 0:
      result = (63 - clzl(i)) + (64*n)
      s.excl result
proc resolvIterative(g:var Sudoku):bool =
  var stack = @[g]
  while stack.len > 0:
    g = stack.pop
    let i = g.find(Base(0))
    if i < 0: return true
    for elem in interset(g,i mod 9,int(i/9)):
      stack.add g.dup(replace(i,elem))


var g="""..1....8......45....5.7......273..........2..358.1....2.3.56...9.......1..6.9.7..""".toSudoku
echo g
import times,algorithm
var t = getTime()
doAssert resolvIterative(g)
echo g, " took: ", getTime() - t

g = "..43..8.78....2.3..3...6.....69.5..2.9..7..4................5.6.........389.5..7.".dup(reverse).toSudoku
t = getTime()
doAssert resolvIterative(g)
echo g, " took: ", getTime() - t

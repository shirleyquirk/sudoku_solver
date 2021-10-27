type
  Sudoku = distinct string

template `[]`(g:Sudoku,x:int,y:int):char = g.string[y*9+x]

proc `$`(g:Sudoku):string =
  for k in 0..2:
    for i in k*3..k*3+2:
      for j in 0..2:
        result.add g.string[i*9+j*3..i*9+j*3+2] & "|"
      result[^1] = '\n'
    if k<2: result.add "---+---+---\n"

iterator square(g:Sudoku,x:int,y:int):char =
  let x = int(x/3)*3
  let y = int(y/3)*3
  for j in 0..2:
    for i in 0..2:
      yield g[x+i,y+j]

iterator horiz(g:Sudoku, y:int):char =
  for x in 0..8:
    yield g[x,y]

iterator vertiz(g:Sudoku, x:int): char =
  for y in 0 .. 8:
    yield g[x,y]

template freeset(sg:untyped):set[char] =
  var x: set[char] = {'1'..'9','.'}
  for c in sg: x.excl c
  x

proc interset(g:Sudoku,x:int,y:int): set[char] =
    freeset(horiz(g,y)) * freeset(vertiz(g,x)) * freeset(square(g,x,y))

proc replace(g: var Sudoku, pos:int, car: char) = g.string[pos] = car

when true:#defined(useStrChr):
  proc find(s:Sudoku,c:char):int =
    proc strchr(cs:cstring,cc:cint):cstring{.importc:"strchr",header:"<string.h>".}
    cast[ByteAddress](strchr(s.cstring,c.cint)) - cast[ByteAddress](s.cstring)
else:
  proc find(s:Sudoku,c:char):int{.borrow.}

proc resolv(g:var Sudoku):bool =# string =
    let i = g.find('.')
    if i>=0:
        var f = interset(g,i mod 9,int(i/9))
        for elem in f:
            g.replace(i,elem)
            if resolv(g): return true
        g.replace(i,'.')
    else:
        return true
var g="""..1....8......45....5.7......273..........2..358.1....2.3.56...9.......1..6.9.7..""".Sudoku

echo Sudoku("").find('x')
import times
let t = getTime()
doAssert resolv(g)
echo g, " took: ", getTime() - t

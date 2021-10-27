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

type
  Parameters = array[81,int]
  Weights = array[81,float]
  Likelihood = float
  Errors = seq
#[
#i want a likelihood function f(parameters:Parameter,weights:Weight):float
#i know the results i want. so i have a seq[Parameter] and a seq[float]
# lets assume that
# proc f(parameters:Parameter,weights:Weights):float = dot_product(parameters,weights) + error(paramters) # minimize squared error
# if you give me an Weights array, i can give you an Errors seq[float] because i have an oracle(Parameter):float for my chosen subset of Parameters.
var oracle:Table[Parameter,Likelihood]
var weights:Weights #find this
proc errors(weights:Weights):
 find weights

]#


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

#gather data. for each of these strings, and their inverse (maybe check if the inverse exists in the dataset)
import sudokus
var likelihoods:seq[float] #1.0 for forward,-1.0 for back
import sets
var suds = gg.toHashSet
for g in gg:
  suds.incl g.dup(reverse)
echo suds.len

#ok.they all seem different, huh. well fine. maybe the generated ones were unique. whats the word, under inversion.
#maybe other rotations would be interesting.  canonical, that's maybe the word.
#flip left right, top bottom. rotate r,2r,3r. i think fliplr + 2r oh it's 4! and then cubies can be swapped about
#swizzle rows,columns,cubies. hmm, but the clues? i dn
#w/evs.
#if flipping or rotating the board before solving could help, that would be another paramter
#but lets start with (0,1)
import tables
var sudokuData:Table[string,tuple[rating:seq[int],forwards_time, backwards_time: Duration; forwards_tries, backwards_tries: int]]
#[for sud in suds:
  var t = getTime()
  tries = 0
  var s = sud.toSudoku()
  let rating = s.ratings
  doAssert resolvForwards(s)
  let forward_time = getTime()-t
  let forward_tries = tries
  t = getTime()
  tries = 0
  s = sud.toSudoku()
  doAssert resolvBackwards(s)
  sudokuData[sud] = (rating,forward_time,getTime()-t,forward_tries,tries)
]#

import marshal,std/streams
var f = newFileStream("sudokuTable",fmRead)
#store(f,sudokuData)
load(f,sudokuData)
echo "done"
f.close()

#now what
#expectation minimization. E(x) = straight up mean.

#ok, well, straight up cmp(forwards_time,backwards_time). hmm. metaparameter here: if they're super close for some definition of close, could be zero.
proc cmp(x,y:Duration):float =
  const epsilon = 0.95
  if x > y:
    if y.inMicroseconds.float / x.inMicroseconds.float > epsilon:
      0.0
    else:
      1.0
  elif x < y:
    if x.inMicroseconds.float / y.inMicroseconds.float > epsilon:
      0.0
    else:
      -1.0
  else:
    0.0
var parameters:seq[seq[float]]
for k,v in sudokuData:
  parameters.add(collect(for i in k: (if i=='.': 0.0 else: 1.0).float))
  #echo v.rating.len#depends on the completeness of the sudoku. sparse matrix. could use 0 values for non-empty cells? but that would change Expectation(y)
  #minimum number of clues is 17. it is possible for a location to have a rating of 0 and not be full,tho.
  #ok, but you can have the 'rating' for a full square, it's just the sum of the full values in that row/col/cubie, and will be >=3
  #hmmm. ok, what about taking the raw (binary) array. [[0,1,0,0,0,0,0,1,0],[0,0,1...]...] thats all we care about anyway. fine.
  #and expectation for that should be [ 17/81,17/81,...] or slightly more, whatever the average number of clues is
  #ok fine. feels like losing information but maybe not.
  likelihoods.add cmp(v.forwards_time,v.backwards_time)
#for k,v in sudokuData:
#  echo "f: ",v.forwards_tries,", b:",v.backwards_tries
echo parameters.len,',',parameters[0].len
import neo
var params = matrix(parameters)
var likes = matrix(@[likelihoods])
iterator chunks(mat:Matrix[float]):Matrix[float] =
  for i in 0..<mat.dim[0] div 81:
    yield mat[81*i..<81*(i+1),All]
import sequtils
#let z = params.chunks.toSeq#zip(toSeq(params.chunks()),toSeq(likes.chunks()))
echo params.dim
#params expectation
let param_expect = vector(collect(for r in 0..80: 20/81))#vector(collect(for r in params.columns: r.foldl(a+b) / r.len.float))
#echo param_expect
let likes_expect = 0.0#likes.t.column(0).foldl(a+b) / likes.t.column(0).len.float
echo likes_expect #this is xbar. it should be zero

#echo likes.dim
#echo likes.t.chunks.toSeq
#xhat = Wy + b  y is parameter vector ok
#Cxy is cross-covariance matrix between x and y.
#cross covariance matrix is matrix E[(X-ux)(Y-uy).t]
const test_data = 500
var all_likes_normalized = vector(collect(for i in likes.t.column(0)[0..test_data]: i - likes_expect))
#echo likes_normalized
var all_params_normalized = matrix(collect(for r in  params.rows: r - param_expect))
var likes_normalized = all_likes_normalized[0..test_data]
var params_normalized = all_params_normalized[0..test_data,All]
#echo params_normalized
let Cxy = (matrix(@[likes_normalized]) * params_normalized)
let Cyy = (params_normalized.t * params_normalized).inv
let W = Cxy * Cyy#echo Cxy.dim,',',Cyy.dim
#echo W*matrix(@[param_expect]).t
let b = likes_expect - (W*matrix(@[param_expect]).t)[0,0]
#echo W * params[0..0,All].t
var wrongs = 0
for i in test_data..<1011:
  let guess = (W*params[i..i,All].t)[0,0] + b
  if (guess < 0.0 and likelihoods[i] > 0.0) or (guess > 0.0 and likelihoods[i] < 0.0):
    inc wrongs
    #echo "wrong guess: ", guess, " for ",likelihoods[i]
echo "wrongs: ", wrongs, "error rate: ", wrongs.float / (1010 - test_data).float
when false:
  for (p,l) in zip(params.chunks.toSeq,likes.T.chunks.toSeq):
    #echo z.type#p.dim
    #echo l.dim
    #echo likes * p.inv
    echo l * p.T.inv
    echo "\n\n\n"
when false:
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

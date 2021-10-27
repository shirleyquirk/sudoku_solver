import sugar
proc initSet[T]():set[T] = discard

#proc freeset(g:openArray[char]):set[char] = {'1'..'9','.'} - collect(initSet,for i in g: {i})
template freeset(g:untyped):set[char] = {'1'..'9','.'} - collect(initSet,g)
#proc square[T](g:T,x,y:int):auto = g[y*9+x..<y*9+x+3] & g[y*9+x+9..<y*9+x+12] & g[y*9+x+18..<y*9+x+21]
template square(g:openArray[char],x,y:int):untyped =
  for j in [0,9,18]:
    for i in y*9+x+j..<y*9+x+3+j:
      g[i]
proc interset(g:openArray[char],x,y:int):auto =
  freeset(for i in 0..8:{g[x+i*9]}) *
  freeset(for i in y*9..<y*9+9:{g[i]}) *
  freeset(square(g,(x div 3)*3,(y div 3)*3))#[block:
    let x = (x div 3)*3;
    let y = (y div 3)*3;
    for j in [0,9,18]:
      for i in 0..2:
        {g[y*9+x+i+j]}
  ) ]#

proc resolv(g:string):string =
    if '.' in g:
        let i=g.find('.')
        for elem in interset(g,i mod 9,i div 9):
            let ng=resolv(g[0..<i] & elem & g[i+1..^1])
            if ng.len != 0: return ng
    else:
        return g

let g="""..1....8......45....5.7......273..........2..358.1....2.3.56...9.......1..6.9.7.."""
echo resolv(g)

proc square(g: string,x:int,y:int): string =
    let x=int(x/3)*3
    let y=int(y/3)*3
    g[y*9+x .. y*9+x+2] & g[y*9+x+9 .. y*9+x+11] & g[y*9+x+18 .. y*9+x+20]

proc horiz(g: string, y:int): string =
    var ligne: int = y * 9
    g[0+ligne .. 8+ligne]

proc vertiz(g: string, x:int): string =
    result = ""
    for y in 0 .. 8:
        var ligne: int = y * 9
        result.add g[x+ligne]

proc freeset(sg: string): set[char] =
  var x: set[char] = {}
  for c in sg: x.incl c
  {'1'..'9', '.'} - x

proc interset(g:string,x:int,y:int): set[char] =
    freeset(horiz(g,y)) * freeset(vertiz(g,x)) * freeset(square(g,x,y))

proc replace(g:string, pos:int, car: char): string =
    g[0..pos-1] & car & g[pos+1..80]

proc resolv(g:string): string =
    let i = g.find('.')
    if i>=0:
        var f = interset(g,i mod 9,int(i/9))
        for elem in f:
            let ng= resolv( replace(g,i,elem) )
            if ng != "": return ng
        return ""
    else:
        return g

var g="""..1....8......45....5.7......273..........2..358.1....2.3.56...9.......1..6.9.7.."""

import times
let t = getTime()
echo resolv(g), " took: ", getTime() - t

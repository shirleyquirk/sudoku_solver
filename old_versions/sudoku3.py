freeset  = lambda n:     set("123456789.") ^ set(n)
square   = lambda g,x,y: g[y*9+x:y*9+x+3] + g[y*9+x+9:y*9+x+12] + g[y*9+x+18:y*9+x+21]
interset = lambda g,x,y: freeset(g[x::9]) & freeset(g[y*9:y*9+9]) & freeset(square(g,(x//3)*3,(y//3)*3))

def resolv(g):
    if "." in g:
        i=g.index(".")
        for elem in interset(g,i%9,i//9):
            ng=resolv( g[:i] + elem + g[i+1:] )
            if ng: return ng
    else:
        return g

g="""..1....8......45....5.7......273..........2..358.1....2.3.56...9.......1..6.9.7.."""

print(resolv(g))

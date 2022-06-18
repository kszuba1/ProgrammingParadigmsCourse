// Krzysztof Szuba - lista 4
// zadanie 3
def composition(f:Double=>Double,g:Double=>Double,dx:Double)={
  val f2 = (x: Double) => (f(x+dx)-f(x)) / dx
  val g2 = (x:Double) => (g(x+dx)-g(x)) / dx
  g2 compose f2
}
composition(x=>3*x,x=>4*x*x,0.0000001)(0)
composition(Math.cos,Math.sin,0.000001)(0)





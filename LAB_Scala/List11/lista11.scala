class Rownanie:
  private def sprawdz(tab:Array[Double]):Boolean=
    val length = tab.length-3
    val tempTab = Array.fill(length)(0)
    tab.startsWith(tempTab,length-1)

  private def stopien2(tab:Array[Double])=
    val delta = (tab(tab.length-2)*tab(tab.length-2))-(4*tab(tab.length-3)*tab(tab.length-1))
    if delta < 0 then println("Brak rozwiazann rownania"+delta)
    else if delta == 0 then {
      val x=(tab(tab.length-2)*(-1))/(tab(tab.length-3)*2)
      println("1 rozwiazanie: x = " + x)
    }
    else {
      val x1 = ((-1)*tab(tab.length-2)-Math.sqrt(delta))/(2*tab(tab.length-3))
      val x2 = ((-1)*tab(tab.length-2)+Math.sqrt(delta))/(2*tab(tab.length-3))
      println("2 rozwiazania: x1 = "+x1+"  x2 = "+x2)
    }

  private def stopien1(tab:Array[Double])=
    val x = (tab(tab.length-1)*(-1))/tab(tab.length-2)
    println("1 rozwiazanie: x = "+x)


  def rozwiaz(row:Array[Double])=
    if row.length >= 3 && row(row.length - 3) != 0 && sprawdz(row) then stopien2(row)
    else if row.length >= 3 && row(row.length - 3) != 0 && !sprawdz(row) then println("Niezaimplementowane rownanie")
    else if row.length == 3 && row(row.length - 3) !=0 then stopien2(row)
    else if row.length >= 3 && row(row.length - 3) == 0 && row(row.length - 2) != 0 && sprawdz(row) then stopien1(row)
    else if row.length >= 3 && row(row.length - 3) == 0 && row(row.length - 2) != 0 && !sprawdz(row) then println("Niezaimplementowane rownanie")
    else if row.length == 3 && row(row.length - 3) == 0 then stopien1(row)
    else if row.length == 2 && row(0) != 0 then stopien1(row)

    else println("nieskonczenie wiele rozwiazan")



object Rownanie:
  def main(args : Array[String]) : Unit =
    val r1=new Array[Double](4)
    r1(0)=0
    r1(1)=(-3)
    r1(2)=2
    r1(3)=1


    val rownanie=new Rownanie
    rownanie.rozwiaz(r1)
    r1(1)=0
    rownanie.rozwiaz(r1)//2x=-1
    r1(0)=5
    rownanie.rozwiaz(r1)

    val r2=new Array[Double](3)
    r2(0) = (-3)
    r2(1) = (2)
    r2(2) = (1)

    rownanie.rozwiaz(r2)

    r2(0)=0

    rownanie.rozwiaz(r2)//2x=-1











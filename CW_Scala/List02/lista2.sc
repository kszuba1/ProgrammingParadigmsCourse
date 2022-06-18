def imply1(pb: (Boolean, Boolean)): Boolean =
  pb match {
    case (false, false) => true
    case (false, true) => true
    case (true, false) => false
    case (true, true) => true
  }

def fib(n: Int): Int = {
  n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 2) + fib(n - 1)
  }
}
def fib2(n: Int): Int = {
  def fib2Helper(fib1: Int, fib2: Int, accum: Int) = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 2) + fib(n - 1)
    }

  }

  n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 2) + fib(n - 1)
  }
}

def fibB(n: Int): Int = {
  def fibIn(n: Int, p: Int, s: Int): Int = {
    n match {
      case 0 => p
      case 1 => s
      case _ => fibIn(n - 1, s, p + s)
    }
  }

  if (n >= 0) fibIn(n, 0, 1)
  else throw new Exception("Ujemny argument")
}
def root3(a: Double): Double = {
  def root3Helper(a: Double,x:Double):Double={
    if math.abs((x*x*x)-a)<=math.pow(10,-15)*math.abs(a) then x else
      root3Helper(a,x+(((a/math.pow(x,2))-x)/3))
    }
  if a>1 then root3Helper(a,a/3) else root3Helper(a,a)
  }
def root3b(a: Double, e: Double): Double = {
  def fabs(a: Double): Double = {
    if (a >= 0) a
    else -a
  }
  def cbrtIn(a: Double, i: Int, s: Double, e: Double): Double = {
    if (fabs(s*s*s-a) < e*fabs(a)) {
      s
    } else {
      i match {
        case 0 if a > 1 => cbrtIn(a, i+1, a/3, e)
        case 0 => cbrtIn(a, i+1, a, e)
        case _ => cbrtIn(a, i+1, s + (a/(s*s)-s)/3, e)
      }
    }
  }
  if (e < 0) throw new Exception("Dokładność musi być nieujemna")
  cbrtIn(a, 0, 0, e)
}
root3b(8,1)

def root33(a:Double) = {
  def root3Helper(x:Double):Double =
    if (math.abs(math.pow(x, 3) - a) <=1.0E-15* Math.abs(a)) x
    else root3Helper(x + (a / math.pow(x, 2) - x) / 3)
  root3Helper(if (a > 1) a/3 else a)
}
//zad 5
def initSegment[A](list:List[A],list2:List[A]):Boolean={
  (list,list2) match{
    case (Nil,_)  => true
    case (_,Nil) => false
    case (_,_)  => if(list.head == list2.head ) then initSegment(list.tail,list2.tail) else false
  }
}
initSegment(List(1,2,3),List(1,2,3,4,5,6))

def replaceNth[A](list:List[A], index:Int, element:A):List[A] =
  (list, index) match {
    case (Nil, _) => Nil
    case (list, 0) => element :: list.tail
    case (list, _) => list.head :: replaceNth(list.tail, index-1, element)
  }

replaceNth(List(1,2,3,4,5,6),2,0)


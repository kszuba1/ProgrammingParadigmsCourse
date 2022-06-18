//Krzysztof Szuba - lista 9

//zadanie 1
class Time(private var hr: Int):
  if hr < 0 then hr = 0

  def hour: Int = hr

  def hour_=(newHr: Int) =
    if newHr < 0 then hr = 0
    else hr = newHr

//zadanie 2
//a
class Time2A(private var hr : Int, private var min : Int):
  require(0 <= hr && hr < 24)
  require(0 <= min && min < 60)

  def hour : Int = hr

  def hour_=(newHr : Int)  =
    require(0 <= newHr && newHr < 24)
    hr = newHr

  def minute : Int = min

  def minute_=(newMin : Int) =
    require(0 <= newMin && newMin < 60)
    min = newMin

  def before(other : Time2A) : Boolean =
    hr < other.hr || (hr == other.hr && min < other.min)

//b

class Time2B(hr : Int,  min : Int):
  require(0 <= hr && hr < 24)
  require(0 <= min && min < 60)

  private var minutes = hr * 60 + min

  def hour : Int = minutes / 60

  def hour_=(newHr : Int) =
    require(0 <= newHr && newHr < 24)
    minutes = newHr * 60 + minute

  def minute : Int = minutes % 60

  def minute_=(newMin : Int) =
    require(0 <= newMin && newMin < 60)
    minutes = hour * 60 + newMin

  def before(other : Time2B) : Boolean =
    minutes < other.minutes

//zadanie 3
class Pojazd(val producent : String, val model : String, val rokProd : Int = -1, var numerRej : String = ""):
  def this(producent : String, model : String, numerRej : String) = this(producent, model, -1, numerRej)

//zadanie 4
def metoda1 = metoda2
def metoda2 = metoda3
def metoda3 = throw new Exception("Wyjątek zgłoszony w metoda3")

object Lista9:
  def main(args : Array[String]) : Unit =
    val t1 = new Time(7)
    println(t1.hour)
    t1.hour=13
    println(t1.hour)

    val t2 = Time2A(13,21)
    val t3 = Time2A(17, 50)
    println(t2.before(t3))
    println(t3.before(t2))
    t2.hour = 17
    println(t2.before(t3))
    t2.minute = 51
    println(t2.before(t3))

    val t4 = Time2B(13,21)
    val t5 = Time2B(17, 50)
    println(t4.before(t5))
    println(t5.before(t4))
    t4.hour = 17
    println(t4.before(t5))
    t4.minute = 51
    println(t4.before(t5))


    val p1: Pojazd = new Pojazd("Ford", "Fiesta")
    var p2: Pojazd = new Pojazd("Ford", "Fiesta",2010)
    var p3: Pojazd = new Pojazd("Ford", "Fiesta", " DW 50A1C")
    var p4: Pojazd = new Pojazd("Ford", "Fiesta", 2010, " DW 50A1C")

    println(p1.rokProd+p1.numerRej)
    p1.numerRej="  FZ 11111"
    println(p1.rokProd+p1.numerRej)
    println(p2.rokProd+p2.numerRej)
    println(p3.rokProd+p3.numerRej)
    println(p4.rokProd+p4.numerRej)


    //zadanie 4
    try
      metoda1
    catch
      case e: Exception =>
        System.err.println(e.getMessage() + "\n")
        e.printStackTrace()


    println("siema")



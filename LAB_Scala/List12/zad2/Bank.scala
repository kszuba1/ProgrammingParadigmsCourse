class Bank(val minStan: Int) {
  private var stan = minStan

  def take(kwota: Int)=this.synchronized{
    while kwota>stan do {println("not enough money in bank, waiting");wait()}
    println("taking " + kwota +"$")
    stan=stan-kwota
    notifyAll()
  }
  def put(kwota: Int) = this.synchronized{
    while stan>=minStan do {println("enough money in bank, waiting");wait()}
    println("putting " + kwota +"$")
    stan=stan+kwota
    notifyAll()
  }

}

object Bank:
  def main(args: Array[String]): Unit =
    val bank = new Bank(1000)
    new Consumer(500,bank).start()
    new Producer(500,bank).start()




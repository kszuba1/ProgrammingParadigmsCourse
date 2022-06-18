class Consumer(val kwotaWyplaty: Int,bank: Bank) extends Thread {
  override def run(): Unit =
    for i <- 1 to 5 do bank.take(kwotaWyplaty)


}

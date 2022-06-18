class Producer(val kwotaWplaty: Int, bank: Bank) extends Thread:
  override def run(): Unit =
    while true do {bank.put(kwotaWplaty); Thread.sleep(500)}



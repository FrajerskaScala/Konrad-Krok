import org.apache.pekko.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import scala.util.Random

def losuj(n: Int, m: Int): Int = {
  val random = new Random()
  n + random.nextInt(m - n + 1)
}

case class Init(liczbaUczestnikow: Int)
case object Start
case object Zaglosuj
case class Glosuje(n: Int)

class Przewodniczacy extends Actor with ActorLogging {
  def receive: Receive = {
    case Init(liczbaUczestnikow) =>
      log.info(s"Tworzenie $liczbaUczestnikow uczestników...")
      val uczestnicy = (1 to liczbaUczestnikow).map { id =>
        context.actorOf(Props[Uczestnik](), s"uczestnik-$id")
      }.toList
      context.become(glosowanie(uczestnicy, 0, 0))
      self ! Start

    case _ => log.warning("Nieznany komunikat")
  }

  def glosowanie(uczestnicy: List[ActorRef], liczbaGlosow: Int, sumaGlosow: Int): Receive = {
    case Start =>
      log.info("Rozpoczynam głosowanie...")
      uczestnicy.foreach(_ ! Zaglosuj)

    case Glosuje(n) =>
      val nowaLiczbaGlosow = liczbaGlosow + 1
      val nowaSumaGlosow = sumaGlosow + n
      log.info(s"Otrzymano głos: $n. Liczba głosów: $nowaLiczbaGlosow, suma głosów: $nowaSumaGlosow")

      if (nowaLiczbaGlosow == uczestnicy.length) {
        log.info(s"Glosowanie zakonczone. Srednia glosow: ${nowaSumaGlosow.toDouble / nowaLiczbaGlosow}")
        context.stop(self)
      } else {
        context.become(glosowanie(uczestnicy, nowaLiczbaGlosow, nowaSumaGlosow))
      }

    case _ => log.warning("Nieznany komunikat podczas glosowania")
  }
}

class Uczestnik extends Actor with ActorLogging {
  def receive: Receive = {
    case Zaglosuj =>
      val glos = losuj(1, 10)
      log.info(s"${self.path.name} głosuje: $glos")
      sender() ! Glosuje(glos)

    case _ => log.warning("Nieznany komunikat")
  }
}

@main def zad3: Unit = {
  val system = ActorSystem("Kolokwium")
  val przewodniczacy = system.actorOf(Props[Przewodniczacy](), "przewodniczacy")

  przewodniczacy ! Init(5)
}
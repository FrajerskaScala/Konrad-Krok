import org.apache.pekko
import pekko.actor._
import scala.util.Random

def losuj(n: Int, m: Int): Int = {
  val random = new Random()
  n + random.nextInt(m - n + 1)
}

case class Init(liczbaUczestnikow: Int)
case object Start
case object Zaglosuj
case class Glosuje(n: Int)

class Przewodniczacy extends Actor {
  import context._

  var uczestnicy: List[ActorRef] = List.empty
  var wyniki: List[Int] = List.empty
  var pozostaliGlosujacy: Int = 0

  def receive: Receive = {
    case Init(liczbaUczestnikow) =>
      uczestnicy = (1 to liczbaUczestnikow).map { i =>
        context.actorOf(Props[Uczestnik], s"uczestnik-$i")
      }.toList
      sender() ! s"Utworzono $liczbaUczestnikow uczestników"
    
    case Start =>
      pozostaliGlosujacy = uczestnicy.size
      uczestnicy.foreach(_ ! Zaglosuj)

    case Glosuje(ocena) =>
      wyniki = ocena :: wyniki
      pozostaliGlosujacy -= 1

      if (pozostaliGlosujacy == 0) {
        val srednia = wyniki.sum.toDouble / wyniki.size
        println(s"Głosowanie zakończone. Średnia ocen: $srednia")
        context.system.terminate()
      }
  }
}

class Uczestnik extends Actor {
  def receive: Receive = {
    case Zaglosuj =>
      val ocena = losuj(1, 10) 
      println(s"${self.path.name} oddaje głos: $ocena")
      sender() ! Glosuje(ocena)
  }
}

@main
def zad3: Unit = {
  val system = ActorSystem("Kolokwium")

  val przewodniczacy = system.actorOf(Props[Przewodniczacy], "przewodniczacy")

  przewodniczacy ! Init(5)
  Thread.sleep(100)
  przewodniczacy ! Start
}

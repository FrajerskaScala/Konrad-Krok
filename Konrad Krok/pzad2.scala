import scala.io.Source

@main
def zad2: Unit = {
  val dane = Source
    .fromResource("dane.txt")
    .getLines()
    .toList

  val convert = dane.map(_.split(";").toList).map {
    case List(nazwisko, imie, siec, wiek) => (nazwisko, imie, siec, wiek.toInt)
    case _ => throw new IllegalArgumentException("Nieprawidłowy format danych w pliku.")
  }

  val groupedBySiec = convert.groupBy(_._3)

  val summary = groupedBySiec.map { case (siec, osoby) =>
    val najstarszaOsoba = osoby.maxBy(_._4)
    (najstarszaOsoba._1, siec, najstarszaOsoba._4)
  }.toList

  val sortedSummary = summary.sortBy(-_._3).zipWithIndex.map {
    case ((nazwisko, siec, maxWiek), index) => (index + 1, siec, maxWiek)
  }

  println(sortedSummary)
}

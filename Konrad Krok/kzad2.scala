import scala.io.Source

@main def zad2: Unit = {
  val dane = Source
    .fromResource("dane.txt")
    .getLines
    .toList

  val convert = dane.map(_.split(";").toList).map {
    case List(nazwisko, imie, siec, wiek) => (nazwisko, imie, siec, wiek.toInt)
    case _ => throw new IllegalArgumentException("Nieprawidłowy format danych")
  }

  val groupedBySiec = convert.groupBy(_._3)

  val result = groupedBySiec
    .map { case (siec, osoby) =>
      val maxWiek = osoby.map(_._4).max
      (siec, maxWiek)
    }
    .toList
    .sortBy(-_._2)
    .zipWithIndex
    .map { case ((siec, maxWiek), index) => (index + 1, siec, maxWiek) }

  println(result)
}
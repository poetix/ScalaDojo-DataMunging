package org.timgroup.dojo

class Spread[T](val index:T,  val spread:Int)  { }

case class MungerFor[T](val lines: List[String], interpreter:Array[String] => Spread[T]) {
  def munge =
      lines.map(_.trim)
           .filter(_.matches("^\\d.*"))
           .map(_.replaceAll("\\*", "").split("\\s+"))
           .map(interpreter)
           .minBy(_.spread)
           .index
}

object Interpreters {
  val toInts = (a:Array[String]) => a.take(3).map(_.toInt)
  val _weather = (a:Array[Int]) => new Spread(a(0), a(1) - a(2))
  val weather = _weather.compose(toInts)
  val football = (a:Array[String]) => new Spread(a(1),(a(6).toInt - a(8).toInt).abs )
}

case class WeatherMunging(val lines: List[String]) {
  def dayWithTheSmallestTemperatureSpread : Int =
    MungerFor(lines,Interpreters.weather).munge
}

case class FootballMunging(val lines: List[String]) {
  def teamWithTheSmallestDifferenceBetweenForAndAgainst:String =
    MungerFor(lines,Interpreters.football).munge
}

package org.timgroup.dojo

abstract class Spread[T] {

  val index:T
  val spread:Int

}

case class Weather(data:Array[String]) extends Spread[Int] {
  val intData = data.take(3).map(_.toInt)
  val index = intData(0)
  val spread = intData(1) - intData(2)
}

case class Football(data:Array[String]) extends Spread[String] {
  val index = data(1)
  val spread = (data(6).toInt - data(8).toInt).abs
}

case class MungerFor[T](val lines: List[String], interpreter:Array[String] => Spread[T]) {
  def munge =
      lines.map(_.trim)
           .filter(_.matches("^\\d.*"))
           .map(_.replaceAll("\\*", "").split("\\s+"))
           .map(interpreter)
           .minBy(_.spread)
           .index
}

case class WeatherMunging(val lines: List[String]) {
  def dayWithTheSmallestTemperatureSpread : Int =
    MungerFor(lines, a => Weather(a)).munge
}

case class FootballMunging(val lines: List[String]) {
  def teamWithTheSmallestDifferenceBetweenForAndAgainst:String =
    MungerFor(lines, a=> Football(a)).munge
}

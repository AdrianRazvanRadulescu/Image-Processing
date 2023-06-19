package util

case class Pixel(red: Integer, green: Integer, blue: Integer) {
  def pixelToString : String = {
    red.toString + " " + green.toString + " " + blue.toString
  }
}

object Pixel {
  def fromStringToPixel(stringPixels: List[String]): Pixel = {
    val intPixel = stringPixels.map(stringPixel => stringPixel.toInt)
    Pixel(intPixel(0), intPixel(1), intPixel(2))
  }
}


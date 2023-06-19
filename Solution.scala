import util.{Pixel, Util}

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val stringImage = image.mkString
    val lines = stringImage.split("\n").toList

    if (lines.length >= 4) { 
      val stringDimensions = lines(1).split(" ")
      val width = stringDimensions(0).toInt

      val allPixels = lines.drop(3)

      val pixelStrings = allPixels.flatMap(line => line.split(" "))
      val groupedPixels = pixelStrings.grouped(3).toList
      val pixelRows = groupedPixels.grouped(width).toList

      pixelRows.map(row => row.map(Pixel.fromStringToPixel))
    } else {
      List.empty
    }
  }

  def toStringPPM(image: Image): List[Char] = {
    val imageWidth = image.head.length
    val imageHeight = image.length
    val ppmHeader = s"P3\n$imageWidth $imageHeight\n255\n"
    val pixelStrings = image.flatMap(row => row.map(p => p.pixelToString))
    val formattedPixels = pixelStrings.mkString("\n")
    (ppmHeader + formattedPixels + "\n").toList
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    val combinedImages = image1 ++ image2
    combinedImages
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    val combinedRows = image1.zip(image2).map { case (row1, row2) =>
      val combinedRow = row1 ::: row2
      combinedRow
    }
    combinedRows
  }

  // ex 3
  def rotate(image: Image, degrees: Int): Image = {
    
    def rotateBy90Degrees(imageData: Image): Image = {
      val reversedColumnIndices = imageData.head.indices.reverse
      
      val rotatedImage = reversedColumnIndices.map { column =>
        imageData.map { row =>
          row(column)
        }
      }.toList

      rotatedImage
    }

    val angleMod360 = degrees % 360

    angleMod360 match {
      case 90  => rotateBy90Degrees(image)
      case 180 =>
        val rotatedFirst = rotateBy90Degrees(image)
        val rotatedSecond = rotateBy90Degrees(rotatedFirst)
        rotatedSecond
      case 270 =>
        val rotatedFirst = rotateBy90Degrees(image)
        val rotatedSecond = rotateBy90Degrees(rotatedFirst)
        val rotatedThird = rotateBy90Degrees(rotatedSecond)
        rotatedThird
      case _ => image
    }
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    val rad = (kernel.length - 1) / 2
    val surroundingPixels = Util.getNeighbors(image, rad)

    surroundingPixels.map { eachRow =>
      eachRow.map { area =>
        (kernel, area).zipped.flatMap((kRow, nRow) => (kRow, nRow).zipped.map(_ * _)).sum
      }
    }
  }

  def edgeDetection(image: Image, threshold: Double): Image = {
    val grayImg = image.map(_.map(Util.toGrayScale))
    val blurImg = applyConvolution(grayImg, gaussianBlurKernel)
    val gradX = applyConvolution(blurImg, Gx)
    val gradY = applyConvolution(blurImg, Gy)

    val mergedImg = gradX.zip(gradY).map { case (xRow, yRow) =>
      xRow.zip(yRow).map { case (pxGradX, pxGradY) =>
        math.abs(pxGradX) + math.abs(pxGradY)
      }
    }

    mergedImg.map { row => 
      row.map { px =>
        if (px >= threshold) Pixel(255, 255, 255)
        else Pixel(0, 0, 0)
      }
    }
  }

  def pickColor(i: Integer): Pixel = {
    if (i == 0) Pixel(255, 0, 0)
    else if (i == 1) Pixel(0, 0, 255)
    else if (i == 2) Pixel(0, 255, 0)
    else if (i == 3) Pixel(255, 255, 255)
    else Pixel(0, 0, 0)
  }

  def moduloPascalMatrix(M: Integer, size: Integer): List[List[Integer]] = {
    def calcElement(i: Integer, j: Integer, memo: Map[(Integer, Integer), Integer]): (Integer, Map[(Integer, Integer), Integer]) = {
      if (j == 0 || i == j) {
        (1, memo)
      } else { 
        memo.get((i, j)) match {
          case Some(value) => (value, memo)
          case None =>
            val (a, memo1) = calcElement(i - 1, j - 1, memo)
            val (b, memo2) = calcElement(i - 1, j, memo1)
            val newValue = (a + b) % M
            (newValue, memo2 + ((i, j) -> newValue))
        }
      }
    }

    def generateRow(i: Integer, memo: Map[(Integer, Integer), Integer]): (List[Integer], Map[(Integer, Integer), Integer]) = {
      (0 to i).toList.foldLeft((List.empty[Integer], memo)) { case ((row, m), j) =>
        val (elem, newMemo) = calcElement(i, j, m)
        (row :+ elem, newMemo)
      }
    }

    def padRow(row: List[Integer]): List[Integer] = {
      row ++ List.fill(size - row.length)(4)
    }

    (0 until size).toList.foldLeft((List.empty[List[Integer]], Map.empty[(Integer, Integer), Integer])) { case ((matrix, memo), i) =>
      val (row, newMemo) = generateRow(i, memo)
      (matrix :+ padRow(row), newMemo)
    }._1
  }

  def moduloPascal(M: Integer, pickColor: Integer => Pixel, size: Integer): Image = {
    moduloPascalMatrix(M, size).map(_.map(pickColor))
  }
}
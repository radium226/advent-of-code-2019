import java.nio.file.{Path, Paths}
import scala.io.Source

type Color = Int

type Coordinates = (Int, Int)

type Size = (Int, Int)

type Row = List[Color]

type Layer = List[Row]

implicit class LayerOps(layer: Layer) {

  def colorAt(coordinates: Coordinates): Color = {
    Layer.colorAt(layer)(coordinates)
  }

  def colorCount(color: Color): Int = {
    Layer.colorCount(layer)(color)
  }

  def width: Int = {
    Layer.width(layer)
  }

  def height: Int = {
    Layer.height(layer)
  }

  def show: String = {
    Layer.show(layer)
  }

  def negate: Layer = {
    Layer.negate(layer)
  }

}

object Layer {

  def colorAt(layer: Layer)(coordinates: Coordinates): Color = {
    val (x, y) = coordinates
    layer(y)(x)
  }

  def colorCount(layer: Layer)(color: Color): Int = {
    (for {
      r <- layer
      c <- r
    } yield if (c == color) 1 else 0).sum
  }

  def show(layer: Layer): String = {
    layer.map({ row =>
      row.map({
        case 1 =>
          " "
        case _ =>
          "x"
      }).mkString
    }).mkString("\n")
  }

  def negate(layer: Layer): Layer = {
    layer.map(_.map({
      case 0 =>
        1
      case 1 =>
        0
    }))
  }

  def merge(layers: List[Layer]): Layer = {
    val width = layers(0).width
    val height = layers(0).height
    layers.foldLeft(Array.fill[Row](height)(Array.fill[Color](width)(2).toList).toList)({ (previousLayer, nextLayer) =>
      nextLayer
        .zipWithIndex.iterator.map({ case (row, y) =>
          row.zipWithIndex.iterator.map({ case (nextColor, x) =>
            val previousColor = previousLayer.colorAt(x, y)
            previousColor match {
              case 0 => // Black
                0

              case 1 => // Whilte
                1

              case 2 =>
                nextColor
            }
          })
              .toList
        })
        .toList
    })
  }

  def width(layer: Layer): Int = {
    layer(0).length
  }

  def height(layer: Layer): Int = {
    layer.length
  }

  def parseText(size: Size)(text: String): List[Layer] = {
    val (width, height) = size
    text
      .toList
      .filter(!Character.isWhitespace(_))
      .map(_.toString.toInt)
      .grouped(width * height)
      .map(_.grouped(width).toList)
      .toList
  }

  def parseFile(size: Size)(filePath: Path): List[Layer] = parseText(size)(Source.fromFile(filePath.toFile).mkString)

}

def test(): Unit = {
  val expectedLayersForStep1 = List(
    List(
      List(1, 2, 3),
      List(4, 5, 6)
    ),
    List(
      List(7, 8, 9),
      List(0, 1, 2)
    )
  )

  val parsedLayersForStep1 = Layer.parseText(3, 2)("123456789012")

  assert(parsedLayersForStep1 == expectedLayersForStep1)
  assert(expectedLayersForStep1(0).colorAt(0, 0) == 1)
  assert(expectedLayersForStep1(0).colorAt(1, 0) == 2)
  assert(expectedLayersForStep1(0).width == 3)
  assert(expectedLayersForStep1(0).height == 2)


  val parsedLayersForStep2 = Layer.parseText(2, 2)("022211222212000000000000")
  println(Layer.merge(parsedLayersForStep2))
  assert(Layer.merge(parsedLayersForStep2) == List(
    List(0, 1),
    List(1, 0)
  ))

}

test()

val layers = Layer.parseFile(25, 6)(Paths.get("input.txt"))
val layer = layers.minBy(_.colorCount(0))
println(layer.colorCount(1) * layer.colorCount(2))

println(Layer.merge(layers).negate.show)

/***
scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0"
)
*/

import java.nio.file.{Path, Paths}
import scala.io.Source

type Code = Int
object Code {

  def parse(text: String): List[Int] = {
    text
      .stripMargin
      .split("\n")
      .flatMap(_.trim.split(","))
      .map(_.trim)
      .filter(!_.isEmpty)
      .map(_.toInt)
      .toList
  }

  def load(filePath: Path): List[Int] = {
    parse(Source.fromFile(filePath.toFile).mkString)
  }

}

type Offset = Int

type Operation = List[Code]
object Operation {

  def isolate(codes: List[Code])(f: PartialFunction[Code, Control]): List[Operation] = {
    val (operations, _, _) = codes
        .foldLeft[(List[Operation], Operation, Control)]((List.empty[Operation], Operation.empty, Jump(0)))({ (operationsAndoperationAndControl, code) =>
          val (operations, operation, control) = operationsAndoperationAndControl
          control match {
            case Jump(0) =>
              val nextControl = f(code).map(_ - 1)
              if (operations.isEmpty && operation.isEmpty) (List.empty[Operation], List(code), nextControl)
              else (operations :+ operation, List(code), nextControl)
            case Jump(offset) =>
              (operations, operation :+ code, Jump(offset - 1))

            case Halt =>
              (operations, Operation.empty, Halt)
          }
        })

    operations
  }

  def execute(codes: List[Code], operations: List[Operation])(f: PartialFunction[Code, (Code, Code) => Code]): List[Code] = {
    operations.foldLeft(codes)({ (codes, operation) =>
      codes.updated(operation(3), f(operation.head)(codes(operation(1)), codes(operation(2))))
    })
  }

  def empty: Operation = List.empty

}

sealed trait Control {

  def map(f : Offset => Offset): Control

}
case class Jump(offset: Offset) extends Control {

  def map(f: Offset => Offset): Control = {
    Jump(f(offset))
  }

}
case object Halt extends Control {

  def map(f: Offset => Offset): Control = Halt

}

val codes = Code.load(Paths.get("input.txt"))

val patchedCode = codes.updated(1, 12).updated(2, 2)

val operations = Operation.isolate(patchedCode)({
  case 1 | 2 =>
    Jump(4)

  case 99 =>
    Halt

})
val executedCode = Operation.execute(patchedCode, operations)({
  case 1 =>
    _ + _

  case 2 =>
    _ * _
})

type Code = Int
  object Code {

    def parse(text: String): List[Int] = {
      text
        .stripMargin
        .split("\n")
        .flatMap(_.trim.split(","))
        .map(_.trim)
        .filter(!_.isEmpty)
        .map(_.toInt)
        .toList
    }

    def load(filePath: Path): List[Int] = {
      parse(Source.fromFile(filePath.toFile).mkString)
    }

  }

  type Offset = Int

  type Operation = List[Code]
  object Operation {

    def isolate(codes: List[Code])(f: PartialFunction[Code, Control]): List[Operation] = {
      val (operations, _, _) = codes
          .foldLeft[(List[Operation], Operation, Control)]((List.empty[Operation], Operation.empty, Jump(0)))({ (operationsAndoperationAndControl, code) =>
            val (operations, operation, control) = operationsAndoperationAndControl
            control match {
              case Jump(0) =>
                val nextControl = f(code).map(_ - 1)
                if (operations.isEmpty && operation.isEmpty) (List.empty[Operation], List(code), nextControl)
                else (operations :+ operation, List(code), nextControl)
              case Jump(offset) =>
                (operations, operation :+ code, Jump(offset - 1))

              case Halt =>
                (operations, Operation.empty, Halt)
            }
          })

      operations
    }

    def execute(codes: List[Code], operations: List[Operation])(f: PartialFunction[Code, (Code, Code) => Code]): List[Code] = {
      operations.foldLeft(codes)({ (codes, operation) =>
        codes.updated(operation(3), f(operation.head)(codes(operation(1)), codes(operation(2))))
      })
    }

    def empty: Operation = List.empty

  }

  sealed trait Control {

    def map(f : Offset => Offset): Control

  }

  case class Jump(offset: Offset) extends Control {

    def map(f: Offset => Offset): Control = {
      Jump(f(offset))
    }

  }

  case object Halt extends Control {

    def map(f: Offset => Offset): Control = Halt

  }

  val codes = Code.load(Paths.get("input.txt"))

  def compute(codes: List[Code], noun: Code, verb: Code): Code = {
    val patchedCode = codes.updated(1, noun).updated(2, verb)

    val operations = Operation.isolate(patchedCode)({
      case 1 | 2 =>
        Jump(4)

      case 99 =>
        Halt

    })
    val executedCode = Operation.execute(patchedCode, operations)({
      case 1 =>
        _ + _

      case 2 =>
        _ * _
    })

    executedCode(0)
  }



  val (verb, noun) = (for {
    verb <- 0 to 99
    noun <- 0 to 99
  } yield (verb, noun)).map({ case (noun, verb) =>

    val r = compute(codes, noun, verb)
    println(r)
    if (r == 19690720) Some((noun, verb))
    else None
  }).collect({
    case Some((noun, verb)) =>
      (noun, verb)
  }).head

  println(s"noun=${noun} / verb=${verb}")

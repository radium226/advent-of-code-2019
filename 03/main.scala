import scala.io.Source


type Offset = Int

type Length = Int

sealed trait Direction

case object Start extends Direction

case object Up extends Direction

case object Down extends Direction

case object Left extends Direction

case object Right extends Direction

case class Instruction(direction: Direction, length: Length)

case class Position(x: Int, y: Int) {

  def distance: Int = x.abs + y.abs

}

case class Step(instruction: Instruction, positions: List[Position])

object Step {

  def positions(steps: List[Step]): List[Position] = {
    steps.flatMap(_.positions).distinct
  }

}

object Instruction {

  def parse(text: String): Instruction = {
    val length = text.tail.toInt
    text.head match {
      case 'U' =>
        Instruction(Up, length)

      case 'D' =>
        Instruction(Down, length)

      case 'L' =>
        Instruction(Left, length)

      case 'R' =>
        Instruction(Right, length)
    }
  }

}

def parseInstructions(text: String): List[Instruction] = {
  text.split(",").map(_.trim).filter(!_.isEmpty).map(Instruction.parse).toList
}

def gatherSteps(instructions: List[Instruction]): List[Step] = {
  instructions.foldLeft(List(Step(Instruction(Start, 0), List(Position(0, 0)))))({ (steps, instruction) =>
    val position = steps.last.positions.last
    val range = (1 to instruction.length).toList
    val newStep = Step(instruction, instruction.direction match {
      case Up =>
        range.map({ i =>
          position.copy(y = position.y + i)
        })

      case Down =>
        range.map({ i =>
          position.copy(y = position.y - i)
        })

      case Left =>
        range.map({ i =>
          position.copy(x = position.x - i)
        })

      case Right =>
        range.map({ i =>
          position.copy(x = position.x + i)
        })
    })

    steps :+ newStep
  }).drop(1)
}

val lines = Source.fromFile("input.txt").getLines().toList

val instructions1 = parseInstructions(lines(0))
val steps1 = gatherSteps(instructions1)
val positions1 = Step.positions(steps1)

val instructions2 = parseInstructions(lines(1))
val steps2 = gatherSteps(instructions2)
val positions2 = Step.positions(steps2)

val allPositions = (positions1 ++ positions2).distinct

val crossedPositions = positions1.intersect(positions2).filter(Position(0, 0) != _)
println(crossedPositions)
val bestCrossedPosition = crossedPositions.minBy(_.distance)
println(bestCrossedPosition.distance)


def countPositions(steps: List[Step], position: Position): Int = {
  //println(s"position=${position}")
  val lastIndex = steps.zipWithIndex.takeWhile({ case (step, _) => !step.positions.contains(position) }).map({ case (_, index) => index }).last
  val s = steps.take(lastIndex + 2)
  //println(s"s.size=${s.size}")
  s.foreach(println)
  val k = s.foldLeft(0)(_ + _.positions.takeWhile(_ != position).size) + 1
  //println(s"k=${k}")
  k
}

println(crossedPositions.map({ position => countPositions(steps1, position) + countPositions(steps2, position) }).min)

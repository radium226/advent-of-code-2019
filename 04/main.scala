type Password = String

type Check = Password => Boolean

def check(password: Password): Boolean = {
  val digits: Check = _.matches("[0-9]{6}")
  val adjacent: Check = _.toList.sliding(2).exists({ letters => letters(0) == letters(1) })
  val increase: Check = _.toList.map(_.toInt).sliding(2).forall({ digits => digits(0) <= digits(1) })
  val larger: Check = { password =>
    val letters = password.toList
    val countByLetter = letters.groupBy({ letter => letter }).map({ case (letter, letters) => letter -> letters.length })
    countByLetter.forall({ case (_, length) => length == 1 || length % 2 == 0 })
  }

  val checks = List(digits, adjacent, increase, larger)
  println(s" --> ${password}")
  checks.forall(_.apply(password))
}

def test(): Unit = {
  //assert(check("111111")) 1196
  assert(!check("223450"))
  assert(!check("123789"))
  assert(check("112233"))
  assert(!check("123444"))
  assert(check("111122"))
}

test()

val passwords = (152085 to 670283).toList.map(_.toString).filter(check)
println(passwords.size)

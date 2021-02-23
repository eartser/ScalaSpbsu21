package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException {
    def handleRepeatingDigitsException(): Unit = println("Characters can't repeat. Try again!")
  }
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException {
    def handleWrongNumberLengthException(): Unit = println("Unsuitable length of the string: " +
      s"expected $expected characters, got $got characters. Try again!")
  }

  def generateNumberString(length: Int): String = {
    var str = ""
    var symbs: Set[Char] = Set.empty
    val anStream = Random.alphanumeric
    var i = 0
    while (symbs.size < length) {
      val sym = anStream(i)
      if (!symbs.contains(sym)) {
        str = str + sym
        symbs = symbs + sym
      }
      i += 1
    }
    str
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    val len = userInput.length
    val userSym = userInput.toSet

    if (len != secret.length)
      throw new WrongNumberLengthException(secret.length, userInput.length)
    if (len != userSym.size)
      throw new RepeatingDigitsException

    var bulls = 0
    var cows = 0
    val secretSym = secret.toSet
    for (i <- 0 until len) {
      if (userInput(i) == secret(i))
        bulls += 1
      else if (secretSym.contains(userInput(i)))
        cows += 1
    }

    if (bulls == len) Correct(numTries)
    else Incorrect(bulls, cows)
  }

  def main(args: Array[String]): Unit = {
    print("Enter length: ")
    val length = readLine().toInt
    val secret = generateNumberString(length)
    println(s"Ok, I made a $length-character word from 'a-z', 'A-Z', '0-9'. Try to guess it!")

    var res: GuessResult = Incorrect(0, 0)
    var numTries = 0
    while (res.isInstanceOf[Incorrect]) {
      numTries += 1
      print("Your input: ")
      val userInput = readLine()
      try {
        res = validate(secret, userInput, numTries)
        res match {
          case Correct(numTries) => println(s"You won! Number of tries: $numTries.")
          case Incorrect(bulls, cows) => println(s"Bulls: $bulls, cows: $cows.")
        }
      } catch {
        case e: WrongNumberLengthException => e.handleWrongNumberLengthException(); numTries -= 1
        case e: RepeatingDigitsException => e.handleRepeatingDigitsException(); numTries -= 1
        case _ => println("Something went wrong!")
      }
    }
  }
}
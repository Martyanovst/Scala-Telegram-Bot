package main.scala

import scala.io.Source

object CommandLineBot extends App{
  val fileName = "test.txt"
  val  commands = Source.fromFile(fileName).getLines.foreach(_ => CommandReader.ReadCommand(_))
}

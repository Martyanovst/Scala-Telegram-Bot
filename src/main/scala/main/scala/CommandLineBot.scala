package main.scala

import scala.io.Source

object CommandLineBot extends App{
  val fileName = "test.txt"
  var commands = List[Command]()
  for(line <- Source.fromFile(fileName).getLines)
    commands ::= CommandReader.ReadCommand(line)
}

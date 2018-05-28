package main.scala

import scala.io.Source

object CommandLineBot extends App {
  val parser = new CommandParser()
  Source.fromFile("MyTimeTest.txt").getLines
    .map(parser.parse(parser.command, _).get.execute)
    .foldLeft(PollRepo())(
      (context, executor) => {
        val (message, ctx) = executor(context)
        println(message)
        ctx
      })
}

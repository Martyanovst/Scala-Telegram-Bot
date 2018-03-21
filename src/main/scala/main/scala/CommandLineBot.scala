package main.scala

import Parsers._

import scala.io.Source

object CommandLineBot extends App {
  val parser = new CommandParser()
  var context = PollRepo(Map[Int, Poll]())
  Source.fromFile("test.txt").getLines
    .map(parser.parse(parser.command, _).get.execute)
    .foldLeft(context)((context, executor) => {
      val (message, ctx) = executor(context)
      println(message)
      ctx
    })
}
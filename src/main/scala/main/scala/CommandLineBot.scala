package main.scala

import Parsers._

import scala.io.Source

object CommandLineBot extends App {
  val parser = new CommandParser()
  Source.fromFile("test.txt").getLines
    .map(parser.parse(parser.command, _).get.execute)
    .foldLeft(PollRepo(Map[Int, Poll]()))(
      (context, executor) => {
      val (message, ctx) = executor(context)
      println(message)
      ctx
    })
}
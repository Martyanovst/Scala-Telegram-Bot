package main.scala
import info.mukel.telegrambot4s.api.TelegramBot
import info.mukel.telegrambot4s.api.Polling
import info.mukel.telegrambot4s.api.declarative.{Callbacks,Commands}
import scala.io.Source


//class MyBot extends TelegramBot with Polling with Commands{
//  override def token = "499150688:AAHcHDrlzM_MGLEm5s9p5JrsdQy2stqd3TY"
//  onCommand("help"){
//  }
//  onCommand("delete"){}
//  onCommand("")
//  onCommand("begin"){}
//}
object Main extends App {
  val parser = new CommandParser()
  Source.fromFile("MyTimeTest.txt").getLines
    .map(parser.parse(parser.command, _).get.execute)
    .foldLeft(PollRepo(Map[Int, Poll]()))(
      (context, executor) => {
        val (message, ctx) = executor(context)
        println(message)
        ctx
      })
}

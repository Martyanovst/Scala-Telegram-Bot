package main.scala

import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.methods.SendMessage
import info.mukel.telegrambot4s.models.Message


object MyBot extends TelegramBot with Polling with Commands {
  lazy val token = "499150688:AAHcHDrlzM_MGLEm5s9p5JrsdQy2stqd3TY"
  val parser = new CommandParser()
  var context = PollRepo()

  override def receiveMessage(msg: Message): Unit = {
    for (text <- msg.text) {
      val command = parser.parse(parser.command, text).get
      val commandWithAuth = command.auth(msg.from)
      val (message, ctx) = commandWithAuth execute context
      context = ctx
      request(SendMessage(msg.source, message))
    }
  }
}
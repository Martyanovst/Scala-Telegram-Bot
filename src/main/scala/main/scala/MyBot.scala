package main.scala

import info.mukel.telegrambot4s.api.declarative.{Args, Commands}
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.models.User


object MyBot extends TelegramBot with Polling with Commands {
  lazy val token = "499150688:AAHcHDrlzM_MGLEm5s9p5JrsdQy2stqd3TY"
  val parser = new CommandParser()
  var context = PollRepo()


  onCommand('create_poll) { implicit msg =>
    withArgs {
      args => {
        val command = parser.parse(parser.createPoll, join("/create_poll", args))
        if (command.isEmpty) reply(processCommand(IncorrectCommand("incorrect command")))
        else {
          val (message, repo) = command.get execute context
          context = repo
          reply(message)
        }
      }
    }
  }

  onCommand('list) { implicit msg =>
    reply(processRequest("/list", msg.from))
  }

  onCommand('delete_poll) { implicit msg =>
    withArgs(args => reply(processRequest(join("/delete_poll", args), msg.from)))

  }

  onCommand('start_poll) { implicit msg =>
    withArgs(args => reply(processRequest(join("/start_poll", args), msg.from)))
  }

  onCommand('stop_poll) { implicit msg =>
    withArgs(args => reply(processRequest(join("/stop_poll", args), msg.from)))
  }


  onCommand('result) { implicit msg =>
    withArgs(args => reply(processRequest("/result " + args.mkString(" "), msg.from)))
  }

  onCommand('begin) { implicit msg =>
    withArgs(args => reply(processRequest(join("/begin", args), msg.from)))
  }

  onCommand('end) { implicit msg =>
    withArgs(args => reply(processRequest(join("/end", args), msg.from)))
  }

  onCommand('view) { implicit msg =>
    withArgs(args => reply(processRequest("/view " + args.mkString(" "), msg.from)))
  }

  onCommand('add_question) { implicit msg =>
    withArgs(args => reply(processRequest(join("/add_question", args), msg.from)))
  }

  onCommand('delete_question) { implicit msg =>
    withArgs(args => reply(processRequest(join("/delete_question", args), msg.from)))
  }

  onCommand('answer) { implicit msg =>
    withArgs(args => reply(processRequest(join("/answer", args), msg.from)))
  }

  def processRequest(msg: String, user: Option[User]): String = processCommand(parseMessage(msg).auth(user))

  def processCommand(command: Command): String = {
    val (message, ctx) = command execute context
    context = ctx
    message
  }

  def parseMessage(msg: String): Command =
    parser.parse(parser.command, msg).getOrElse(IncorrectCommand("incorrect command!"))

  def join(command: String, args: Args): String =
    Array(command, args).mkString(" ")
}
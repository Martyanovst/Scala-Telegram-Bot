package Commands

import main.scala.PollRepo

case class DeletePoll(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => ("Success!", PollRepo(context.polls - id)) }
}

case class StartPoll(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => ("Success!", context) }

}

case class Result(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => ("Success!", context) }

}

case class StopPoll(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => ("Success!", context) }

}

case class Begin(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => ("Success!", context) }

}

case class DeleteQuestion(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => ("Success!", context) }

}

class End extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => ("Success!", context) }

}

class Listing extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context =>
    (context.polls.values.map(poll => poll.toString).mkString, context)
  }
}

class View extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => ("Success!", context) }

}

case class IncorrectCommand(arg: String) extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => ("Success!", context) }

}
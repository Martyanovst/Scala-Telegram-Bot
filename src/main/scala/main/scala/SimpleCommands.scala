package main.scala

case class DeletePoll(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context => {
    if (!context.polls.contains(id))
      ("Error: This poll doesn't exists", context)
    else if (context.polls(id).running.getOrElse(false))
      ("Error: Can't delete poll while it's running", context)
    else ("Ok: Poll has been deleted", PollRepo(context.polls - id))
  }
}

case class Listing() extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    (context.polls.values.map(poll => poll.toString).mkString, context)
}

case class StartPoll(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    (tryExecute(context.polls, id, _.start), context)
}

case class Result(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    (tryExecute(context.polls, id, _.showResult), context)
}

case class StopPoll(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    (tryExecute(context.polls, id, _.stop), context)
}

case class IncorrectCommand(text: String) extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => (text, context) }
}

package main.scala

case class DeletePoll(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context => ("Success!", PollRepo(context.polls - id))
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

case class IncorrectCommand(arg: String) extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => ("The operation is incorrect", context) }
}

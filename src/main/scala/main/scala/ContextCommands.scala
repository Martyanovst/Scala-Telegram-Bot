package main.scala

case class Begin(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    if (!context.polls.contains(id))
      ("Error: This poll doesn't exist", context)
    else
      (s"Ok: switch to poll with number $id", PollRepo(context.polls, id))
}

case class End() extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    if (context.currentContextPoll == -1)
      ("Error: The context is already switched off", context)
    else
      (s"Ok: switch off the context", PollRepo(context.polls, -1))
}


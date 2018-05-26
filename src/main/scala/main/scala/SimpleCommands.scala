package main.scala

import Poll.now


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
  override def execute: PollRepo => (String, PollRepo) = context => {
    if (!context.polls.contains(id))
      ("Error: This poll doesn't exist", context)
    else {
      val poll = context.polls(id)
      if ((for {x <- poll.end} yield now after x).getOrElse(false))
        ("Error: Poll is finished", context)
      else if (poll.running.getOrElse(false))
        ("Error: The poll is already on", context)
      else if (poll.begin.isDefined)
        ("Error: Start time is already defined", context)
      else ("Ok: The poll was launched", context % poll.start)
    }
  }
}

case class Result(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context => {
    if (!context.polls.contains(id))
      ("Error: This poll doesn't exist", context)
    else (context.polls(id).showResult, context)
  }
}

case class StopPoll(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    if (!context.polls.contains(id))
      ("Error: This poll doesn't exist", context)
    else {
      val poll = context.polls(id)
      if (poll.end.isDefined) ("Error: Stop time is already defined", context)
      else {
        val isCompleted = for (x <- poll.end) yield now after x
        if (poll.running.getOrElse(false) && isCompleted.getOrElse(true))
          ("Ok: The poll is over", context % poll.stop)
        else ("Error: Poll is already off", context)
      }
    }
}

case class IncorrectCommand(text: String) extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => (text, context) }
}

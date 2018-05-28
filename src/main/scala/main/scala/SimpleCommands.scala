package main.scala

import Poll.now
import info.mukel.telegrambot4s.models.User


case class DeletePoll(id: Int, override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context => {
    if (!context.polls.contains(id))
      ("Error: This poll doesn't exists", context)
    else if (!context.validation(user, id))
      ("Error: permission denied!", context)
    else if (context.polls(id).running.getOrElse(false))
      ("Error: Can't delete poll while it's running", context)
    else ("Ok: Poll has been deleted", PollRepo(context.polls - id))
  }

  override def auth(user: Option[User]) = DeletePoll(id, user)
}

case class Listing(override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    (context.polls.values.map(poll => poll.toString).mkString, context)

  override def auth(user: Option[User]) = Listing(user)
}

case class StartPoll(id: Int, override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context => {
    if (!context.polls.contains(id))
      ("Error: This poll doesn't exist", context)
    else {
      val poll = context.polls(id)
      if (!context.validation(user, id))
        ("Error: permission denied!", context)
      else if ((for {x <- poll.end} yield now after x).getOrElse(false))
        ("Error: Poll is finished", context)
      else if (poll.running.getOrElse(false))
        ("Error: The poll is already on", context)
      else if (poll.begin.isDefined)
        ("Error: Start time is already defined", context)
      else ("Ok: The poll was launched", context % poll.start)
    }
  }

  override def auth(user: Option[User]) = StartPoll(id, user)
}

case class Result(id: Int, override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context => {
    if (!context.polls.contains(id))
      ("Error: This poll doesn't exist", context)
    else (context.polls(id).showResult, context)
  }

  override def auth(user: Option[User]) = Result(id, user)
}

case class StopPoll(id: Int, override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    if (!context.polls.contains(id))
      ("Error: This poll doesn't exist", context)
    else {
      val poll = context.polls(id)
      if (!context.validation(user, id))
        ("Error: permission denied!", context)
      else if (poll.end.isDefined) ("Error: Stop time is already defined", context)
      else {
        val isCompleted = for (x <- poll.end) yield now after x
        if (poll.running.getOrElse(false) && isCompleted.getOrElse(true))
          ("Ok: The poll is over", context % poll.stop)
        else ("Error: Poll is already off", context)
      }
    }

  override def auth(user: Option[User]) = StopPoll(id, user)
}

case class IncorrectCommand(text: String, override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => (text, context) }

  override def auth(user: Option[User]) = IncorrectCommand(text, user)
}

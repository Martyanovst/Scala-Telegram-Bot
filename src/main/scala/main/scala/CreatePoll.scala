package main.scala

import java.util.Date

case class CreatePoll(name: String, isAnonymous: Boolean = false, isAfterStop: Boolean = false,
                      dateStart: Option[Date] = None, dateEnd: Option[Date] = None) extends Command {
  def execute: PollRepo => (String, PollRepo) = { context =>
    val id = IdGenerator.GetId
    val pair = id -> new Poll(id, name, isAnonymous, isAfterStop, dateStart, dateEnd)
    (s"polls id: $id", PollRepo(context.polls + pair))
  }
}

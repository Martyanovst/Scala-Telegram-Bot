package Commands

import java.util.Date

import Parsers.CommandExecutor
import main.scala._

case class CreatePoll(name: String, isAnonymous: Boolean, isAfterStop: Boolean,
                      dateStart: Option[Date], dateEnd: Option[Date]) extends Command {
  def execute: PollRepo => (String, PollRepo) = { context =>
    val id = CommandExecutor.GetId
    val pair = id -> new Poll(id, name, isAnonymous, isAfterStop, dateStart, dateEnd)
    (s"polls id: $id", PollRepo(context.polls + pair))
  }
}

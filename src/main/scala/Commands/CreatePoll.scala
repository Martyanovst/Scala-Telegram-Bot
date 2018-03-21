package Commands

import Old.CommandExecutor
import main.scala._

case class CreatePoll(name: String, isAnonymous: Boolean, isAfterStop: Boolean, dateStart: String, dateEnd: String)
  extends Command {

  def execute: PollRepo => (String, PollRepo) = { context =>
    val id = CommandExecutor.GetId
    val pair = id -> new Poll(id, name, isAnonymous, isAfterStop, dateStart, dateEnd)
    (s"polls id: $id", PollRepo(context.polls + pair))
  }
}

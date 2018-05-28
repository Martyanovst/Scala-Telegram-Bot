package main.scala

import java.util.Date

import info.mukel.telegrambot4s.models.User

case class CreatePoll(name: String,
                      isAnonymous: Boolean = false,
                      isAfterStop: Boolean = false,
                      dateStart: Option[Date] = None,
                      dateEnd: Option[Date] = None,
                      override val user: Option[User] = None
                     ) extends Command {
  def execute: PollRepo => (String, PollRepo) = { context =>
    val id = IdGenerator.GetId
    val pair = id -> new Poll(user, id, name, isAnonymous, isAfterStop, dateStart, dateEnd)
    (s"polls id: $id", PollRepo(context.polls + pair, lastPoll = id))
  }

  override def auth(user: Option[User]) = CreatePoll(name,isAnonymous,isAfterStop,dateStart,dateEnd,user)
}

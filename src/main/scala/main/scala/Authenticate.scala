package main.scala

import info.mukel.telegrambot4s.models.User

case class Authenticate(id: Int, override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    if (!context.polls.contains(id))
      ("Error: This poll doesn't exist", context)
    else
      (s"Ok: authenticate to poll with number $id", context % context.polls(id).auth(user))

  override def auth(user: Option[User]) = Authenticate(id, user)
}

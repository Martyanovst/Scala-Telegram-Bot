package main.scala

import info.mukel.telegrambot4s.models.User


trait Command{
  def execute: PollRepo => (String, PollRepo)

  def user: Option[User] = None

  def auth(user: Option[User]): Command
}

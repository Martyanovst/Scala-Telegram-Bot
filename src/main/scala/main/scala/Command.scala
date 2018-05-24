package main.scala

import scala.util.Try

trait Command {
  def execute: PollRepo => (String, PollRepo)

  def tryExecute(polls: Map[Int, Poll], id: Int, f: Poll => String): String =
    Try(f(polls(id))).getOrElse("This poll doesnt exists")
}

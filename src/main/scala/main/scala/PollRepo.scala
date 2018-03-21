package main.scala

import java.util.Date
import java.util.Calendar

import Commands.Question

import scala.util.Try

case class PollRepo(polls: Map[Int, Poll], context: Int = 0)


class Poll(id: Int, name: String, isAnonymous: Boolean, isAfterstop: Boolean, started: Option[Date], end: Option[Date]) {
  def now: Date = dateParser.parse(dateParser.format(calendar.getTime))

  var running: Boolean = Try((now after started.get) && (now before end.get)).getOrElse(false)

  val calendar = Calendar.getInstance()
  val dateParser = new java.text.SimpleDateFormat("hh:mm:ss yy:MM:dd")
  val questions = Map[Int, Question]()

  def timeIsNotOver: Boolean = {
    if (started.isEmpty) return true
    if ((now after started.get) && (now before end.get))
      return true
    false
  }

  def start: String = {
    running = started.isEmpty
    if (running)
      "The poll started"
    else
      "Start time is already defined"
  }

  def showResult: String = {
    if (running && timeIsNotOver && isAfterstop)
      "the poll result is not available until the end"
    else
      this.toString + questions.mkString("\n")
  }

  def stop: String = {
    running = end.isEmpty
    if (running) {
      running = !running
      "The poll is over"
    }
    else
      "Stop time is already defined"
  }

  override def toString = {
    s"""Poll name : $name
        Poll id : $id
        Anonymous : $isAnonymous
        Show result : $isAfterstop
        Start : ${started.getOrElse("indefined")}
        End : ${end.getOrElse("indefined")}
        Is running : ${if (running) "Yes" else "No"}
      """
  }
}
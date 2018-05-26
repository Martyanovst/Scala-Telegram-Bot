package main.scala

import java.util.Date
import java.util.Calendar

case class PollRepo(polls: Map[Int, Poll] = Map[Int, Poll](), currentContextPoll: Int = -1)

class Poll(val id: Int, name: String, val isAnonymous: Boolean, val isAfterstop: Boolean,
           val begin: Option[Date], val end: Option[Date],
           val questions: Map[Int, Question] = Map[Int, Question]()) {
  val calendar = Calendar.getInstance()
  val getName = name
  val getId = id
  val dateParser = new java.text.SimpleDateFormat("hh:mm:ss yy:MM:dd")
  val questionNumberGenerator = Stream.from(1).toIterator

  def now: Date = dateParser.parse(dateParser.format(calendar.getTime))

  var running: Option[Boolean] = if (end.isDefined) for {x <- begin; y <- end} yield (now after x) && (now before y)
  else for (x <- begin) yield now after x

  def timeIsNotOver: Option[Boolean] = {
    for (x <- begin; y <- end)
      yield (now after x) && (now before y)
  }

  def start: String = {
    if ((for {x <- end} yield now after x).getOrElse(false))
      "Error: Poll is finished"
    else {
      if (running.getOrElse(false))
        "Error: The poll is already on"
      else {
        if (begin.isDefined)
          "Error: Start time is already defined"
        else {
          running = Some(true)
          "Ok: The poll was launched"
        }
      }
    }
  }

  def showResult: String = {
    if (running.getOrElse(false) && timeIsNotOver.getOrElse(false) && isAfterstop)
      "Error: Poll's result isn't available until the end"
    else
      "Ok\n" + this.toString + questions.mkString("\n")
  }

  def stop: String = {
    if (end.isDefined) "Error: Stop time is already defined"
    else {
      val isCompleted = for (x <- end) yield now after x
      if (running.getOrElse(false) && isCompleted.getOrElse(true)) {
        running = Some(false)
        "Ok: The poll is over"
      }
      else "Error: Poll is already off"
    }
  }

  def addQuestion(question: Question) = new Poll(id, name, isAnonymous, isAfterstop, begin, end,
    questions + (questionNumberGenerator.next() -> question))

  def deleteQuestion(id: Integer) = new Poll(id, name, isAnonymous, isAfterstop, begin, end,
    questions - id)
  
  override def toString = {
    s"""Poll name : $name
        Poll id : $id
        Anonymous : $isAnonymous
        Show result : $isAfterstop
        Start : ${
      begin.getOrElse("indefined")
    }
        End : ${
      end.getOrElse("indefined")
    }
        Is running : ${
      if (running.getOrElse(false)) "Yes" else "No"
    }
      """
  }
}
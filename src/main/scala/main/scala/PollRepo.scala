package main.scala

import java.util.Date
import java.util.Calendar
import Poll.now

case class PollRepo(polls: Map[Int, Poll] = Map[Int, Poll](), currentContextPoll: Int = -1) {

  def %(poll: Poll): PollRepo = PollRepo(polls + (poll.id -> poll), currentContextPoll)
}

class Poll(val id: Int, name: String, val isAnonymous: Boolean, val isAfterstop: Boolean,
           val begin: Option[Date], val end: Option[Date],
           val questions: Map[Int, Question] = Map[Int, Question]()) {

  val getName = name
  val getId = id

  val questionNumberGenerator = Stream.from(1).toIterator

  val running: Option[Boolean] = {
    if (end.isDefined)
      for {x <- begin; y <- end} yield (now after x) && (now before y)
    else for (x <- begin) yield (now after x) || (now equals x)
  }

  def timeIsNotOver: Option[Boolean] = {
    for (x <- begin; y <- end)
      yield (now after x) && (now before y)
  }

  def start = new Poll(id, name, isAnonymous, isAfterstop, Some(now), end, questions)


  def showResult: String = {
    if (running.getOrElse(false) && timeIsNotOver.getOrElse(false) && isAfterstop)
      "Error: Poll's result isn't available until the end"
    else
      "Ok\n" + this.toString + questions.mkString("\n")
  }

  def stop = new Poll(id, name, isAnonymous, isAfterstop, begin, Some(now), questions)

  def deleteQuestion(idQ: Integer): Poll = new Poll(id, name, isAnonymous, isAfterstop, begin, end, questions - idQ)

  def addQuestion(question: Question): Poll = new Poll(id, name, isAnonymous, isAfterstop, begin, end,
    questions + (questionNumberGenerator.next() -> question))


  def updateQuestions(question: Question, idq: Int) = new Poll(id, name, isAnonymous, isAfterstop, begin, end,
    questions + (idq -> question))


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

object Poll {
  val calendar = Calendar.getInstance()
  val dateParser = new java.text.SimpleDateFormat("hh:mm:ss yy:MM:dd")

  def now: Date = dateParser.parse(dateParser.format(calendar.getTime))
}
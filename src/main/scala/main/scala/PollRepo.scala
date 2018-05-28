package main.scala

import java.util.Date
import java.util.Calendar

import Poll.now
import info.mukel.telegrambot4s.models.User

import scala.util.Try

case class PollRepo(polls: Map[Int, Poll] = Map[Int, Poll](), currentContextPoll: Int = -1, lastPoll: Int = -11) {

  def %(poll: Poll): PollRepo = PollRepo(polls + (poll.id -> poll), currentContextPoll)

  def validation(user: Option[User], pollId: Int): Boolean = Try(polls(pollId).creator == user).getOrElse(false)
}

class Poll(
            val creator: Option[User],
            val id: Int,
            val name: String,
            val isAnonymous: Boolean,
            val isAfterstop: Boolean,
            val begin: Option[Date],
            val end: Option[Date],
            val questions: Map[Int, Question] = Map[Int, Question]()
          ) {


  val questionNumberGenerator = Stream.from(1).toIterator

  val running: Option[Boolean] = {
    if (end.isDefined)
      for {x <- begin; y <- end} yield (now after x) && (now before y)
    else for (x <- begin) yield (now after x) || (now equals x)
  }

  def isAdmin(user: Option[User]): Boolean = user == creator

  def timeIsNotOver: Option[Boolean] = {
    for (x <- begin; y <- end)
      yield (now after x) && (now before y)
  }

  def start = new Poll(creator, id, name, isAnonymous, isAfterstop, Some(now), end, questions)

  def showResult: String = {
    if (running.getOrElse(false) && timeIsNotOver.getOrElse(false) && isAfterstop)
      "Error: Poll's result isn't available until the end"
    else
      "Ok\n" + this.toString + questions.mkString("\n")
  }

  def stop = new Poll(creator, id, name, isAnonymous, isAfterstop, begin, Some(now), questions)

  def deleteQuestion(idQ: Integer): Poll = new Poll(creator, id, name, isAnonymous, isAfterstop, begin, end, questions - idQ)

  def addQuestion(question: Question): Poll = new Poll(creator, id, name, isAnonymous, isAfterstop, begin, end,
    questions + (questionNumberGenerator.next() -> question))


  def updateQuestions(question: Question, idq: Int) = new Poll(creator, id, name, isAnonymous, isAfterstop, begin, end,
    questions + (idq -> question))

  def auth(user: Option[User]): Poll = new Poll(user, id, name, isAnonymous, isAfterstop, begin, end, questions)

  override def toString = {
    s"""Poll name : $name
        Created by : $creator
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
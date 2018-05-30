package main.scala

import java.util.Date
import java.util.Calendar

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


  def running: Option[Boolean] = {
    if (end.isDefined)
      for {x <- begin; y <- end} yield (Poll.now() after x) && (Poll.now() before y)
    else for (x <- begin) yield (Poll.now() after x) || (Poll.now() equals x)
  }

  def isAdmin(user: Option[User]): Boolean = user == creator

  def timeIsNotOver: Option[Boolean] = {
    for (x <- begin; y <- end)
      yield (Poll.now() after x) && (Poll.now() before y)
  }

  def start = new Poll(creator, id, name, isAnonymous, isAfterstop, Some(Poll.now()), end, questions)

  def showResult: String = {
    if (running.getOrElse(false) && timeIsNotOver.getOrElse(false) && isAfterstop)
      "Error: Poll's result isn't available until the end"
    else
      "Ok\n" + this.toString + questions.mkString("\n")
  }

  def stop = new Poll(creator, id, name, isAnonymous, isAfterstop, begin, Some(Poll.now()), questions)

  def deleteQuestion(idQ: Integer): Poll = new Poll(creator, id, name, isAnonymous, isAfterstop, begin, end, questions - idQ)

  def addQuestion(question: Question): Poll = new Poll(creator, id, name, isAnonymous, isAfterstop, begin, end,
    questions + (questions.size + 1 -> question))


  def updateQuestions(question: Question, idq: Int) = new Poll(creator, id, name, isAnonymous, isAfterstop, begin, end,
    questions + (idq -> question))

  def auth(user: Option[User]): Poll = new Poll(user, id, name, isAnonymous, isAfterstop, begin, end, questions)

  override def toString = {
    s"""Poll name : $name
        Created by : ${creator.getOrElse(User(1, isBot = false, "user")).firstName}
        Poll id : $id
        Anonymous : $isAnonymous
        Show result : ${!isAfterstop}
        Start : ${
      begin.getOrElse("indefined")
    }
        End : ${
      end.getOrElse("indefined")
    }
        Is running : ${
      if (running.getOrElse(false)) "Yes" else "No"
    }
        Questions :\n $getQuestions
      """
  }

  def getQuestions: String = {
    if (isAfterstop && running.getOrElse(true))
      "Result's will be available after the end"
    else questions.map(x => x._1.toString + ": " + x._2.toString(isAnonymous)).mkString("\n")
  }
}

object Poll {
  val dateParser = new java.text.SimpleDateFormat("hh:mm:ss yy:MM:dd")

  def now(): Date = dateParser.parse(dateParser.format(Calendar.getInstance().getTime))
}
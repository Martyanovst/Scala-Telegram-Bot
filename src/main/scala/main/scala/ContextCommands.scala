package main.scala

import info.mukel.telegrambot4s.models.User

import scala.util.Try

case class Begin(id: Int, override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    if (!context.polls.contains(id))
      ("Error: This poll doesn't exist", context)
    else if (!context.validation(user, id))
    ("Error: permission denied!", context)
    else
      (s"Ok: switch to poll with number $id", PollRepo(context.polls, id))

  override def auth(user: Option[User]) = Begin(id, user)
}

case class End(override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    if (context.currentContextPoll == -1)
      ("Error: The context is already switched off", context)
    else if (!context.validation(user, context.currentContextPoll))
    ("Error: permission denied!", context)
    else
      (s"Ok: switch off the context", PollRepo(context.polls, -1))

  override def auth(user: Option[User]) = End(user)
}

case class View(override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    if (context.currentContextPoll == -1)
      ("Error: You should select poll to watch the results", context)
    else
      ("Ok\n" + context.polls(context.currentContextPoll).toString, context)

  override def auth(user: Option[User]) = View(user)
}

case class AddQuestion(name: String, questionType: Question.Value, answers: Array[String],
                       override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context => {
    if (context.currentContextPoll == -1) ("Error: you should select poll to add questions", context)
    else if (!context.validation(user, context.currentContextPoll))
      (s"Error: Permission denied!", context)
    else if (context.polls(context.currentContextPoll).running.getOrElse(false))
      ("Error: Can't change poll when it's running", context)
    else {
      questionType match {
        case Question.open => if (answers.nonEmpty)
          ("Error: Can't create poll with answers in choice mode", context)
        else {
          val contextPoll = context.polls(context.currentContextPoll).addQuestion(Question(name, questionType, answers))
          (s"Ok: Poll number ${context.currentContextPoll} add new question", context % contextPoll)
        }
        case Question.multi | Question.choice => if (answers.isEmpty)
          ("Error: Can't create poll without answers in multi or choice mode", context)
        else {
          val contextPoll = context.polls(context.currentContextPoll).addQuestion(Question(name, questionType, answers))
          (s"Ok: Poll number ${context.currentContextPoll} add new question", context % contextPoll)
        }
      }
    }
  }

  override def auth(user: Option[User]) = AddQuestion(name, questionType, answers, user)
}

case class DeleteQuestion(id: Int, override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context => {
    if (context.currentContextPoll == -1)
      ("Error: Context mode turned off", context)
    else if (!context.validation(user, context.currentContextPoll))
      (s"Error: Permission denied!", context)
    else if (context.polls(context.currentContextPoll).running.getOrElse(false))
      ("Error: can't change poll when it's running", context)
    else if (!context.polls(context.currentContextPoll).questions.contains(id))
      ("Error: Question id wasn't found", context)
    else {
      val newContext = context.polls(context.currentContextPoll).deleteQuestion(id)
      (s"Ok: ${context.currentContextPoll} delete question", context % newContext)
    }
  }

  override def auth(user: Option[User]) = DeleteQuestion(id, user)
}

case class AnswerTheQuestion(id: Int, answer: String,
                             override val user: Option[User] = None) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context => {
    val username = user.getOrElse(defaultUser).firstName
    if (context.currentContextPoll == -1)
      ("Error: Context mode turned off", context)
    else {
      if (!context.polls(context.currentContextPoll).questions.contains(id))
        (s"Error: Question №$id doesn't exist", context)
      else {
        val question = context.polls(context.currentContextPoll).questions(id)
        if (!context.polls(context.currentContextPoll).running.getOrElse(false))
          ("Error: can't answer the question if poll wasn't started", context)
        else if (question.usersAnswers.contains(username))
          (s"Error: User already answered the question №$id", context)
        else if (question.questionType == Question.choice && Try(answer.toInt).isFailure)
          ("Error: Question type is choice but answer isn't digit", context)
        else if (question.questionType == Question.multi) {
          val arr = answer.split(" ")
          if (arr.exists(x => Try(x.toInt).isFailure))
            ("Error: Question type is multi but answer isn't sequence of digits", context)
          else if (arr.distinct.length != arr.length)
            ("Error: Question type is multi but some digits in answer are the same", context)
          else {
            val newUserAnswers = question.usersAnswers + (username -> answer)
            val newQuestion = Question(question.text, question.questionType, question.answers, newUserAnswers)
            val newContext = context.polls(context.currentContextPoll).updateQuestions(newQuestion, id)
            (s"Ok: answer in poll №${context.currentContextPoll} on question №$id accepted", context % newContext)
          }
        }
        else {
          val newUserAnswers = question.usersAnswers + (username -> answer)
          val newQuestion = Question(question.text, question.questionType, question.answers, newUserAnswers)
          val newContext = context.polls(context.currentContextPoll).updateQuestions(newQuestion, id)
          (s"Ok: answer in poll №${context.currentContextPoll} on question №$id accepted", context % newContext)
        }
      }
    }
  }

  override def auth(user: Option[User]) = AnswerTheQuestion(id, answer, user)

  val defaultUser: User = User(-1, isBot = false, "default")
}


package main.scala

case class Begin(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    if (!context.polls.contains(id))
      ("Error: This poll doesn't exist", context)
    else
      (s"Ok: switch to poll with number $id", PollRepo(context.polls, id))
}

case class End() extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    if (context.currentContextPoll == -1)
      ("Error: The context is already switched off", context)
    else
      (s"Ok: switch off the context", PollRepo(context.polls, -1))
}

case class View() extends Command {
  override def execute: PollRepo => (String, PollRepo) = context =>
    if (context.currentContextPoll == -1)
      ("Error: You should select poll to watch the results", context)
    else
      ("Ok\n" + context.polls(context.currentContextPoll).toString, context)
}

case class AddQuestion(name: String, questionType: Question.Value, answers: Array[String]) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context => {
    if (context.currentContextPoll == -1) ("Error: you should select poll to add questions", context)
    else if (context.polls(context.currentContextPoll).running.getOrElse(false))
      ("Error: Can't change poll when it's running", context)
    else {
      questionType match {
        case Question.open => if (answers.nonEmpty)
          ("Error: Can't create poll with answers in choice mode", context)
        else {
          val contextPoll = context.polls(context.currentContextPoll).addQuestion(Question(name, questionType, answers))
          (s"Ok: Poll number ${context.currentContextPoll} add new question",
            PollRepo(context.polls + (context.currentContextPoll -> contextPoll), context.currentContextPoll))
        }
        case Question.multi | Question.choice => if (answers.isEmpty)
          ("Error: Can't create poll without answers in multi or choice mode", context)
        else {
          val contextPoll = context.polls(context.currentContextPoll).addQuestion(Question(name, questionType, answers))
          (s"Ok: Poll number ${context.currentContextPoll} add new question",
            PollRepo(context.polls + (context.currentContextPoll -> contextPoll), context.currentContextPoll))
        }
      }
    }
  }
}

case class DeleteQuestion(id: Int) extends Command {
  override def execute: PollRepo => (String, PollRepo) = context => {
    if (context.currentContextPoll == -1)
      ("Error: Context mode turned off", context)
    else if (context.polls(context.currentContextPoll).running.getOrElse(false))
      ("Error: can't change poll when it's running", context)
    else if (!context.polls(context.currentContextPoll).questions.contains(id))
      ("Error: Question id wasn't found", context)
    else {
      val newContext = context.polls(context.currentContextPoll).deleteQuestion(id)
      (s"Ok: ${context.currentContextPoll} delete question",
        PollRepo(context.polls + (context.currentContextPoll -> newContext), context.currentContextPoll))
    }
  }
}

case class AnswerOnQuestion(username: String,id: Int, answer: String) extends Command{
  override def execute: PollRepo => (String, PollRepo) = context => {
      if (context.currentContextPoll == -1)
        ("Error: Context mode turned off", context)
      else if (!context.polls(context.currentContextPoll).running.getOrElse(false))
        ("Error: can't answer the question if poll wasn't started", context)
      else if (!context.polls(context.currentContextPoll).questions.contains(id))
        (s"Error: Question №$id doesn't exist", context)
      else if (context.polls(context.currentContextPoll).questions(id).usersAnswers.contains(username))
        (s"Error: User already answered the question №$id", context)
      else{
        val question = context.polls(context.currentContextPoll).questions(id)
        val newUserAnswers = question.usersAnswers + (username -> answer)
        val newQuestion = Question(question.text, question.questionType, question.answers, newUserAnswers)
        val newContext = context.polls(context.currentContextPoll).updateQuestions(newQuestion, id)
        (s"Ok: answer in poll №${context.currentContextPoll} on question №$id accepted",
          PollRepo(context.polls + (context.currentContextPoll -> newContext), context.currentContextPoll))
      }
    }
}

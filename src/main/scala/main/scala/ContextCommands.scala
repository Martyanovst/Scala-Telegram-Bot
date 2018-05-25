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

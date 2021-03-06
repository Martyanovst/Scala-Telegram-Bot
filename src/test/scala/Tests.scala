import info.mukel.telegrambot4s.models.User
import main.scala._
import org.scalatest.{FlatSpec, Matchers}

class Tests extends FlatSpec with Matchers {
  val dateParser = new java.text.SimpleDateFormat("hh:mm:ss yy:MM:dd")

  "After initialization poll repo's map" should "be empty" in assert(PollRepo().polls.isEmpty)

  "After Initialization poll repo's context id" should "be zero" in assertResult(-1)(PollRepo().currentContextPoll)


  "Poll repo" should "contains new poll after create poll" in {
    val (_, actual) = CreatePoll("test") execute PollRepo()
    assert(actual.polls.nonEmpty)
    assertResult("test")(actual.polls(1).name)
  }

  "When poll repo is empty Listing" should "be empty" in {
    val (message, _) = Listing() execute PollRepo()
    assert(message.isEmpty)
  }

  "Delete Poll" should "delete poll if it contains" in {
    val (_, ctx) = CreatePoll("test") execute PollRepo()
    val id = ctx.polls.keys.head
    val (message, actual) = DeletePoll(id) execute ctx
    message shouldBe "Ok: Poll has been deleted"
    assert(actual.polls.isEmpty)
  }

  "Delete Poll" should "not delete anything when delete incorrect poll id" in {
    val (_, ctx) = CreatePoll("test") execute PollRepo()
    val (message, actual) = DeletePoll(1000) execute ctx
    message shouldBe "Error: This poll doesn't exists"
    assert(actual.polls.nonEmpty)
  }

  "Delete Poll" should "not delete anything when poll is running" in {
    val (_, ctx) = CreatePoll("test") execute PollRepo()
    val id = ctx.polls.keys.head
    val (_, repo) = StartPoll(id) execute ctx
    val (message, actual) = DeletePoll(id) execute repo
    message shouldBe "Error: Can't delete poll while it's running"
    assert(actual.polls.nonEmpty)
  }

  "Start Poll" should "not start poll, when it's running" in {
    val (msg, ctx) = CreatePoll("HI", dateStart = Some(dateParser.parse("12:05:30 18:03:16"))) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (message, _) = StartPoll(id) execute ctx
    assertResult("Error: The poll is already on")(message)
  }

  "Start Poll" should "start poll, when start time is undefined" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (message, _) = StartPoll(id) execute ctx
    assertResult("Ok: The poll was launched")(message)
  }

  "Start Poll" should "not start poll, when start time is defined" in {
    val (msg, ctx) = CreatePoll("HI", dateStart =
      Some(dateParser.parse("12:05:30 19:03:16"))) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (message, _) = StartPoll(id) execute ctx
    assertResult("Error: Start time is already defined")(message)
  }

  "Start Poll" should "not start poll, when it's finished" in {
    val (msg, ctx) = CreatePoll("HI", dateStart = Some(dateParser.parse("12:05:30 16:03:16")),
      dateEnd = Some(dateParser.parse("12:05:30 18:05:16"))) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (message, _) = StartPoll(id) execute ctx
    assertResult("Error: Poll is finished")(message)
  }

  "Start Poll" should "stop poll after start" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (messageStart, context) = StartPoll(id) execute ctx
    messageStart shouldBe "Ok: The poll was launched"
    context.polls(id).running shouldBe Some(true)
    val (messageStop, repo) = StopPoll(id) execute context
    messageStop shouldBe "Ok: The poll is over"
    repo.polls(id).running shouldBe Some(false)
  }

  "Stop Poll" should "not stop poll, when it doesn't running" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val (message, _) = StopPoll(msg.split(":")(1).trim.toInt) execute ctx
    assertResult("Error: Poll is already off")(message)
  }

  "Stop Poll" should "not stop poll, when stop time is defined" in {
    val (msg, ctx) = CreatePoll("HI", dateStart = Some(dateParser.parse("12:05:30 18:03:16")),
      dateEnd = Some(dateParser.parse("12:05:30 18:05:16"))) execute PollRepo()
    val (message, _) = StopPoll(msg.split(":")(1).trim.toInt) execute ctx
    assertResult("Error: Stop time is already defined")(message)
  }

  "Stop Poll" should "stop poll, when poll is running and stop time isn't defined" in {
    val (msg, ctx) = CreatePoll("HI", dateStart = Some(dateParser.parse("12:05:30 18:03:16"))) execute PollRepo()
    val (message, _) = StopPoll(msg.split(":")(1).trim.toInt) execute ctx
    assertResult("Ok: The poll is over")(message)
  }

  "Poll Repo" should "show poll's result, when poll is completed" in {
    val (msg, ctx) = CreatePoll("HI", dateStart = Some(dateParser.parse("12:05:30 16:03:16")),
      dateEnd = Some(dateParser.parse("12:05:30 17:03:16"))) execute PollRepo()
    val (message, _) = Result(msg.split(":")(1).trim.toInt) execute ctx
    assertResult("Ok")(message.split("\n")(0))
  }

  "Poll Repo" should "show poll's result, when poll's running with continuous mode on" in {
    val (msg, ctx) = CreatePoll("HI", dateStart = Some(dateParser.parse("12:05:30 16:03:16")),
      dateEnd = Some(dateParser.parse("12:05:30 20:03:16"))) execute PollRepo()
    val (message, _) = Result(msg.split(":")(1).trim.toInt) execute ctx
    assertResult("Ok")(message.split("\n")(0))
  }

  "Poll Repo" should "not show poll's result, when poll's running with afterstop mode on" in {
    val (msg, ctx) = CreatePoll("HI", isAfterStop = true, dateStart = Some(dateParser.parse("12:05:30 16:03:16")),
      dateEnd = Some(dateParser.parse("12:05:30 20:03:16"))) execute PollRepo()
    val (message, _) = Result(msg.split(":")(1).trim.toInt) execute ctx
    assertResult("Error: Poll's result isn't available until the end")(message)
  }

  "Poll Repo" should "switch context, when repo exists this id" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (message, repo) = Begin(id) execute ctx
    assertResult(s"Ok: switch to poll with number $id")(message)
    assertResult(repo.currentContextPoll)(id)
  }

  "Poll Repo" should "not switch context, when repo doesn't exist this id" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt + 1
    val (message, _) = Begin(id) execute ctx
    assertResult("Error: This poll doesn't exist")(message)
  }

  "End" should "switch off the context, when repo exists this id" in {
    val (msg, ctx1) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, ctx2) = Begin(id) execute ctx1
    val (message, repo) = End() execute ctx2
    assertResult("Ok: switch off the context")(message)
    assertResult(repo.currentContextPoll)(-1)
  }

  "End" should "not switch off the context, when repo isn't in context mode" in {
    val repo = PollRepo()
    assertResult(repo.currentContextPoll)(-1)
    val (message, actualRepo) = End() execute repo
    assertResult("Error: The context is already switched off")(message)
    assertResult(actualRepo.currentContextPoll)(-1)
  }

  "View" should "show poll information, when repo is in context mode" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, repo) = Begin(id) execute ctx
    val (message, _) = View() execute repo
    assertResult("Ok")(message.split("\n")(0))
  }

  "Poll Repo" should "not show poll information, when repo isn't in context mode" in {
    val repo = PollRepo()
    assertResult(repo.currentContextPoll)(-1)
    val (message, _) = View() execute repo
    assertResult("Error: You should select poll to watch the results")(message.split("\n")(0))
  }

  "Poll Repo" should "add question to poll, when repo is in context mode" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id) execute ctx
    val (message, repo) = AddQuestion("Who are you?", Question.choice, Array("Tom", "John")) execute context
    assertResult(s"Ok: Poll number ${repo.currentContextPoll} add new question")(message)
    val actualQuestions = repo.polls(repo.currentContextPoll).questions
    actualQuestions should have size 1
    actualQuestions should contain key 1
    val question = actualQuestions(1)
    question.text shouldBe "Who are you?"
    question.questionType shouldBe Question.choice
    question.answers shouldBe Array("Tom", "John")
  }

  "Poll Repo" should "not add question to poll, when repo isn't in context mode" in {
    val (message, _) = AddQuestion("Who are you?", Question.choice, Array("Tom", "John")) execute PollRepo()
    assertResult("Error: you should select poll to add questions")(message)
  }

  "Poll Repo" should "not add question to poll, when question have multi or choice type and haven't any answers" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id) execute ctx
    for (questionType <- Array(Question.choice, Question.multi)) {
      val (message, repo) = AddQuestion("Who are you?", questionType, Array.empty) execute context
      assertResult("Error: Can't create poll without answers in multi or choice mode")(message)
      val actualQuestions = repo.polls(repo.currentContextPoll).questions
      actualQuestions should have size 0
    }
  }

  "Poll Repo" should "not add question to poll, when question have open type and have answers" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id) execute ctx
    val (message, repo) = AddQuestion("Who are you?", Question.open, Array("Tom", "John")) execute context
    assertResult("Error: Can't create poll with answers in choice mode")(message)
    val actualQuestions = repo.polls(repo.currentContextPoll).questions
    actualQuestions should have size 0
  }

  "Poll Repo" should "not add question to poll, when poll is running" in {
    val (msg, ctx) = CreatePoll("HI", dateStart = Some(dateParser.parse("12:05:30 18:03:16"))) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id) execute ctx
    val (message, repo) = AddQuestion("Who are you?", Question.choice, Array("Tom", "John")) execute context
    assertResult("Error: Can't change poll when it's running")(message)
    val actualQuestions = repo.polls(repo.currentContextPoll).questions
    actualQuestions should have size 0
  }

  "Delete question" should "delete question, if it exist and context mode is on" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id) execute ctx
    val (_, repo) = AddQuestion("Who are you?", Question.open, Array.empty) execute context
    val (message, newRepo) = DeleteQuestion(1) execute repo
    assertResult(s"Ok: $id delete question")(message)
    val actualQuestions = newRepo.polls(newRepo.currentContextPoll).questions
    actualQuestions should have size 0
  }

  "Delete question" should "return error, when question id is wrong" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id) execute ctx
    val (message, newRepo) = DeleteQuestion(1) execute context
    assertResult("Error: Question id wasn't found")(message)
    assertResult(newRepo)(context)
  }

  "Delete question" should "return error, if context mode wasn't started" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (message, newRepo) = DeleteQuestion(1) execute ctx
    assertResult("Error: Context mode turned off")(message)
    assertResult(newRepo)(ctx)
  }

  "Delete question" should "return error, if poll was started" in {
    val (msg, ctx) = CreatePoll("HI") execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id) execute ctx
    val (_, repo) = AddQuestion("Who are you?", Question.open, Array.empty) execute context
    val (_, rep) = StartPoll(id) execute repo
    val (message, newRepo) = DeleteQuestion(1) execute rep
    assertResult(s"Error: can't change poll when it's running")(message)
    val actualQuestions = newRepo.polls(newRepo.currentContextPoll).questions
    actualQuestions should have size 1
  }

  "AnswerOnQuestion" should "return error, context mode hasn't started" in {
    val (_, ctx) = CreatePoll("HI") execute PollRepo()
    val username = User(1, isBot = false, "Orochimaru")
    val (message, _) = AnswerTheQuestion(1, "smt", user = Some(username)) execute ctx
    assertResult("Error: Context mode turned off")(message)
  }

  "AnswerOnQuestion" should "return error, if poll wasn't started" in {
    val username = User(1, isBot = false, "Sakura")
    val (msg, ctx) = CreatePoll("HI", user = Some(username)) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id, user = Some(username)) execute ctx
    val (_, repo) = AddQuestion("Who are you?", Question.open, Array.empty, user = Some(username)) execute context
    val (message, r) = AnswerTheQuestion(1, "smt", Some(username)) execute repo
    assertResult("Error: can't answer the question if poll wasn't started")(message)
    r.polls(r.currentContextPoll).questions(1).usersAnswers shouldBe Map()
  }

  "AnswerOnQuestion" should "return error, if user answered the question before" in {
    val username = User(1, isBot = false, "Sasuke")
    val (msg, ctx) = CreatePoll("HI", user = Some(username)) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id,user = Some(username)) execute ctx
    val (_, repo) = AddQuestion("Who are you?", Question.open, Array.empty, user = Some(username)) execute context
    val (_, rep) = StartPoll(id, user = Some(username)) execute repo
    val (ms, re) = AnswerTheQuestion(1, "smt", Some(username)) execute rep
    val (message, newRepo) = AnswerTheQuestion(1, "smt", Some(username)) execute re
    newRepo.polls(newRepo.currentContextPoll).questions(1).usersAnswers shouldBe Map("Sasuke" -> "smt")
    assertResult(s"Ok: answer in poll №$id on question №1 accepted")(ms)
    assertResult("Error: User already answered the question №1")(message)
  }

  "AnswerOnQuestion" should "return error, if question id doesn't exist" in {
    val username = User(1, isBot = false, "Sakura")
    val (msg, ctx) = CreatePoll("HI", user = Some(username)) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id,user = Some(username)) execute ctx
    val (_, re) = AddQuestion("Who are you?", Question.open, Array.empty, user = Some(username)) execute context
    val (_, rep) = StartPoll(id,user = Some(username)) execute re
    val (message, repo) = AnswerTheQuestion(2, "smt", Some(username)) execute rep
    assertResult("Error: Question №2 doesn't exist")(message)
    repo.polls(repo.currentContextPoll).questions(1).usersAnswers should have size 0
    repo.polls(repo.currentContextPoll).questions should have size 1
  }

  "AnswerOnQuestion" should "accept answer, if question exist and user didn't aswer on it before" in {
    val username = User(1, isBot = false, "Sakura")
    val (msg, ctx) = CreatePoll("HI", user = Some(username)) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id,user = Some(username)) execute ctx
    val (_, repo) = AddQuestion("Who are you?", Question.open, Array.empty, Some(username)) execute context
    val (_, rep) = StartPoll(id, Some(username)) execute repo
    val (message, _) = AnswerTheQuestion(1, "smt", Some(username)) execute rep
    assertResult(s"Ok: answer in poll №$id on question №1 accepted")(message)
  }

  "AnswerOnQuestion" should "return error, if question type is choice and answer is not digit" in {
    val username = User(1, isBot = false, "Sakura")
    val (msg, ctx) = CreatePoll("HI", user = Some(username)) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id,user = Some(username)) execute ctx
    val (_, repo) = AddQuestion("Who are you?", Question.choice, Array("1", "2", "3"), Some(username)) execute context
    val (_, rep) = StartPoll(id, Some(username)) execute repo
    val (message, _) = AnswerTheQuestion(1, "smt", Some(username)) execute rep
    assertResult("Error: Question type is choice but answer isn't digit")(message)
  }

  "AnswerOnQuestion" should "return error, if question type is multi and answer is not sequence of digits" in {
    val username = User(1, isBot = false, "Sakura")
    val (msg, ctx) = CreatePoll("HI", user = Some(username)) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id,user = Some(username)) execute ctx
    val (_, repo) = AddQuestion("Who are you?", Question.multi, Array("adf", "2", "3"), Some(username)) execute context
    val (_, rep) = StartPoll(id, Some(username)) execute repo
    val (message, _) = AnswerTheQuestion(1, "smt", Some(username)) execute rep
    assertResult("Error: Question type is multi but answer isn't sequence of digits")(message)
  }

  "AnswerOnQuestion" should "return error, if question type is multi and some digits in answer are the same" in {
    val username = User(1, isBot = false, "Sakura")
    val (msg, ctx) = CreatePoll("HI", user = Some(username)) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id,user = Some(username)) execute ctx
    val (_, repo) = AddQuestion("Who are you?", Question.multi, Array("1", "2", "3"), Some(username)) execute context
    val (_, rep) = StartPoll(id, Some(username)) execute repo
    val (message, _) = AnswerTheQuestion(1, "1 1 2", Some(username)) execute rep
    assertResult("Error: Question type is multi but some digits in answer are the same")(message)
  }

  "AnswerOnQuestion" should "accept answer, if question type is choice and answer is digit" in {
    val username = User(1, isBot = false, "Sakura")
    val (msg, ctx) = CreatePoll("HI", user = Some(username)) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id,user = Some(username)) execute ctx
    val (_, repo) = AddQuestion("Who are you?", Question.multi, Array("1", "2", "3"), Some(username)) execute context
    val (_, rep) = StartPoll(id, Some(username)) execute repo
    val (message, _) = AnswerTheQuestion(1, "1 1 2", Some(username)) execute rep
    assertResult("Error: Question type is multi but some digits in answer are the same")(message)
  }

  "AnswerOnQuestion" should "accept answer, if question type is multi and answer is sequence of different digts" in {
    val username = User(1, isBot = false, "Sakura")
    val (msg, ctx) = CreatePoll("HI", user = Some(username)) execute PollRepo()
    val id = msg.split(":")(1).trim.toInt
    val (_, context) = Begin(id,user = Some(username)) execute ctx
    val (_, repo) = AddQuestion("Who are you?", Question.multi, Array("1", "2", "3"), Some(username)) execute context
    val (_, rep) = StartPoll(id, Some(username)) execute repo
    val (message, _) = AnswerTheQuestion(1, "1 2 3", Some(username)) execute rep
    assertResult(s"Ok: answer in poll №$id on question №1 accepted")(message)
  }
}

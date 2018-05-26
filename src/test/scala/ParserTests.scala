import java.text._

import main.scala._
import org.scalatest._

import scala.util.Try

class ParserTests extends FunSpec with Matchers {

  val dateParser = new SimpleDateFormat("hh:mm:ss yy:MM:dd")
  val parser = new CommandParser()

  describe("CommandParser") {
    it("should parse create poll command with specified time") {
      val query = "/create_poll (CreatePoll)(yes)(afterstop)(22.22.22 1.1.18) (22.22.23 1.1.18)"
      val startTime = "22.22.22 1.1.18"
      val endTime = "22.22.23 1.1.18"
      val start = Try(dateParser.parse(startTime)).toOption
      val end = Try(dateParser.parse(endTime)).toOption
      val command = CreatePoll("CreatePoll", isAnonymous = true, isAfterStop = true, start, end)
      assertResult(command)(parser.parse(parser.command, query).get)
    }

    it("should parse create poll without time") {
      val query = "/create_poll (SomeName) (no) (afterstop)"
      val start = Try(dateParser.parse("")).toOption
      val end = Try(dateParser.parse("")).toOption
      val command = CreatePoll("SomeName", isAnonymous = false, isAfterStop = true, start, end)
      assertResult(command)(parser.parse(parser.command, query).get)
    }

    it("should parse appending question with multiple choices") {
      val query = "/add_question (Question)(multi)\none\ntwo\nthree\n"
      val answers = Array("one", "two", "three")
      val command = AddQuestion("Question", Question.GetValue("multi"), answers)
      val actual = parser.parse(parser.command, query).get.asInstanceOf[AddQuestion]
      assertResult(command.name)(actual.name)
      assertResult(command.questionType)(actual.questionType)
      assertResult(command.answers)(actual.answers)
    }
  }

  it("should parse appending question without answers") {
    val query = "/add_question (Question)(multi)"
    val command = AddQuestion("Question", Question.GetValue("multi"), Array())
    val actual = parser.parse(parser.command, query).get.asInstanceOf[AddQuestion]
    assertResult(command.name)(actual.name)
    assertResult(command.questionType)(actual.questionType)
    assertResult(command.answers)(actual.answers)
  }

  it("should parse simple command") {
    val queryDel = "/delete_poll (1)"
    val queryStart = "/start_poll (2)"
    val queryResult = "/result (3)"
    val queryBegin = "/begin (4)"
    val queryStop = "/stop_poll (5)"
    val queryDeleteQuestion = "/delete_question (6)"
    parser.parse(parser.command, queryDel).get shouldBe DeletePoll(1)
    parser.parse(parser.command, queryStart).get shouldBe StartPoll(2)
    parser.parse(parser.command, queryResult).get shouldBe Result(3)
    parser.parse(parser.command, queryBegin).get shouldBe Begin(4)
    parser.parse(parser.command, queryStop).get shouldBe StopPoll(5)
    parser.parse(parser.command, queryDeleteQuestion).get shouldBe DeleteQuestion(6)
  }

  it("should parse commands without args") {
    val list = "/list"
    val end = "/end"
    val view = "/view"
    assert(parser.parse(parser.command, list).get.execute.equals(Listing().execute))
    assert(parser.parse(parser.command, end).get.execute.equals(End().execute))
    assert(parser.parse(parser.command, view).get.execute.equals(View().execute))
  }

  it("shouldn't parse wrong command") {
    val badCommandName = "/bad_command (1)"
    val incorrectParams1 = "/create_poll (bla-bla-bla)(12:05:30 18:03:16)"
    val incorrectParams2 = "/create_poll (bla-bla-bla)(yes)(12:05:30 18:03:16)"
    val incorrectParams3 = "/create_poll (bla-bla-bla)(yes)(afterstop)(12!05!30 18!03!16)(12:05:30 18:03:16)"
    assertResult(IncorrectCommand("incorrect command"))(parser.parse(parser.command, badCommandName).get)
    assertResult(IncorrectCommand("bad params"))(parser.parse(parser.command, incorrectParams1).get)
    assertResult(IncorrectCommand("bad params"))(parser.parse(parser.command, incorrectParams2).get)
    assertResult(IncorrectCommand("bad params"))(parser.parse(parser.command, incorrectParams3).get)
  }

  it("should parse add question") {
    val open = "/add_question (bla-bla-bla)(open)"
    val choice = "/add_question (bla-bla-bla)(choice)\nfirst\nsecond\n"
    val multi = "/add_question (bla-bla-bla)(multi)\nfirst\nsecond\n"
    val parsedOpen = parser.parse(parser.command, open).get.asInstanceOf[AddQuestion]
    val parsedChoice = parser.parse(parser.command, choice).get.asInstanceOf[AddQuestion]
    val parsedMulti = parser.parse(parser.command, multi).get.asInstanceOf[AddQuestion]
    parsedOpen.name shouldBe "bla-bla-bla"

    parsedOpen.questionType shouldBe Question.open
    parsedChoice.questionType shouldBe Question.choice
    parsedMulti.questionType shouldBe Question.multi

    parsedOpen.answers should have length 0
    parsedChoice.answers shouldBe Array("first", "second")
    parsedMulti.answers shouldBe Array("first", "second")
  }

  it("shouldn't parse add question with bad question type") {
    val badQuestionType = "/add_question (bla-bla-bla)(multipopen)\nfirst\nsecond\n"
    parser.parse(parser.command, badQuestionType).get shouldBe
      IncorrectCommand("Please, select one of {open;choice,multi} question type")
  }

  it("shouldn't parse delete question with bad parameters") {
    val badQuestionId = "/delete_question (ha-ha)"
    val withoutId = "/delete_question"
    parser.parse(parser.command, badQuestionId).get shouldBe IncorrectCommand("incorrect command")
    parser.parse(parser.command, withoutId).get shouldBe IncorrectCommand("incorrect command")
  }
}

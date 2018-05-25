import main.scala._
import org.scalatest._

import scala.util.Try

class ParserTests extends FunSpec with Matchers {

  val dateParser = new java.text.SimpleDateFormat("hh:mm:ss yy:MM:dd")
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

  it("should parse simple command") {
    val query_del = "/delete_poll (1)"
    val query_start = "/start_poll (2)"
    val query_result = "/result (3)"
    val query_begin = "/begin (4)"
    val query_stop = "/stop_poll (5)"

    assert(parser.parse(parser.command, query_del).get.equals(DeletePoll(1)))
    assert(parser.parse(parser.command, query_start).get.equals(StartPoll(2)))
    assert(parser.parse(parser.command, query_result).get.equals(Result(3)))
    assert(parser.parse(parser.command, query_begin).get.equals(Begin(4)))
    assert(parser.parse(parser.command, query_stop).get.equals(StopPoll(5)))
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
}


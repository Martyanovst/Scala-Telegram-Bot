import Parsers.CommandParser
import Commands._
import org.scalatest._
import scala.util.Try

class ParserTests extends FunSpec {
  val dateParser = new java.text.SimpleDateFormat("hh:mm:ss yy:MM:dd")
  describe("CommandParser"){
    it ("should parse create poll command with specified time"){
      val parser = new CommandParser()
      val query = "/create_poll (CreatePoll) (open) 22.22.22 1.1.18 22.22.23 1.1.18"
      val startTime = "22.22.22 1.1.18"
      val endTime = "22.22.23 1.1.18"
      val start = Try(dateParser.parse(startTime)).toOption
      val end = Try(dateParser.parse(endTime)).toOption
      val command = CreatePoll("CreatePoll", isAnonymous = true, isAfterStop = true, start, end)
      assert(parser.parse(parser.command, query).get.equals(command))
    }
    it ("should parse create poll without time"){
      val parser = new CommandParser()
      val query = "/create_poll (SomeName) (no) (open)"
      val start = Try(dateParser.parse("")).toOption
      val end = Try(dateParser.parse("")).toOption
      val command = CreatePoll("SomeName", isAnonymous = false, isAfterStop = true, start, end)
      assert(parser.parse(parser.command, query).get.equals(command))
    }
    it ("should parse appending question with multiple choices"){
      val parser = new CommandParser()
      val query = "/add_question (Question) (multi) (one)(two)(three)"
      val answers = Array("one", "two", "three")
      val command = Commands.AddQuestion("Question", Commands.Question.GetValue("multi"), answers)
      val one = parser.parse(parser.command, query).get
      assert(one.equals(command))
    }
    it ("should parse simple command")
    {
      val parser = new CommandParser()
      val query_del = "/delete_poll (1)"
      val query_start = "/start_poll (2)"
      val query_result = "/result (3)"
      val query_begin = "/begin (4)"
      val query_stop = "/stop_poll (5)"

      assert(parser.parse(parser.command, query_del).get.equals(Commands.DeletePoll(1)))
      assert(parser.parse(parser.command, query_start).get.equals(Commands.StartPoll(2)))
      assert(parser.parse(parser.command, query_result).get.equals(Commands.Result(3)))
      assert(parser.parse(parser.command, query_begin).get.equals(Commands.Begin(4)))
      assert(parser.parse(parser.command, query_stop).get.equals(Commands.StopPoll(5)))
    }
    it("should parse commands without args")
    {
      val parser = new CommandParser()
      val list = "/list"
      val end = "/end"
      val view = "/view"
      assert(parser.parse(parser.command, list).get.execute.equals(new Commands.Listing().execute))
      assert(parser.parse(parser.command, end).get.execute.equals(new Commands.End().execute))
      assert(parser.parse(parser.command, view).get.execute.equals(new Commands.View().execute))
    }
    it("shouldn't parse wrong command")
    {
      val parser = new CommandParser()
      val bad_command_name = "/bad_command (1)"
      val bad_argument1 = "/create_poll (Poll) (bad) 1.1.1 1.1.1"
      val bad_argument2 = "/create_poll (Poll) (no) (bad)"
      val bad_syntax = "/add_question (Create) multi one two"
      val otladka = parser.parse(parser.command, bad_argument1).get
      assert(parser.parse(parser.command, bad_command_name).get.equals(Commands.IncorrectCommand("incorrect command")))
      assert(parser.parse(parser.command, bad_argument1).get.equals(Commands.IncorrectCommand("incorrect command")))
      assert(parser.parse(parser.command, bad_argument2).get.equals(Commands.IncorrectCommand("incorrect command")))
      assert(parser.parse(parser.command, bad_syntax).get.equals(Commands.IncorrectCommand("incorrect command")))
    }
  }
}

package Parsers

import Commands._
import Old.Question

import scala.util.parsing.combinator.RegexParsers

class CommandParser extends RegexParsers {
  val dateParser = new java.text.SimpleDateFormat("hh:mm:ss yy:MM:dd")

  def command: Parser[Command] = createPoll | simpleCommand | commandsWithoutArgs | add_question |
    success(IncorrectCommand("incorrrect command"))

  def createPoll: Parser[Command] = "/create_poll" ~> anyWord ~ (anonymous | success(true)) ~
    (visibility | success(true)) ~ (date | "") ~ (date | "") ^^ {
    case poll ~ anon ~ vis ~ start ~ stop => CreatePoll(poll, anon, vis, start, stop)
  }

  def simpleCommand: Parser[Command] = "/" ~> ("delete_poll" | "start_poll" |
    "stop_poll" | "result" | "begin" | "delete_question") ~ id ^^ { case commandType ~ id => commandType match {
    case "delete_poll" => DeletePoll(id)
    case "start_poll" => StartPoll(id)
    case "stop_poll" => StopPoll(id)
    case "begin" => Begin(id)
    case "delete_question" => DeleteQuestion(id)
  }
  }

  def commandsWithoutArgs: Parser[Command] = "/" ~> ("list" | "end" | "view") ^^ {
    case "list" => new Listing()
    case "end" => new End()
    case "view" => new View()
  }

  def add_question: Parser[Command] = "/add_question" ~ anyWord ~ ("(" ~> ("open" | "choice" | "multi") <~ ")") ~
    answers ^^ { case _ ~ name ~ questionType ~ answers => AddQuestion(name, Question.GetValue(questionType), answers) }

  def answers: Parser[Array[String]] = ("(" ~> anyWord <~ ")").* ^^ {
    _.toArray
  }

  def id: Parser[Int] = "(" ~> "\\d+".r <~ ")" ^^ {
    _.toInt
  }

  def anonymous: Parser[Boolean] = "(" ~> ("yes" | "no") <~ ")" ^^ {
    _.toString == "yes"
  }

  def date: Parser[String] = "(" ~> "\\d{2}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}".r <~ ")" ^^ (_.toString)

  def visibility: Parser[Boolean] = "(" ~> ("afterstop" | "continuous") <~ ")" ^^ {
    _.toString == "afterstop"
  }

  def anyWord: Parser[String] = "(" ~> "[^)]*".r <~ ")" ^^ {
    _.toString
  }
}

package main.scala

import java.util.Date

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.parsing.combinator.RegexParsers

class CommandParser extends RegexParsers {
  val dateParser = new java.text.SimpleDateFormat("hh:mm:ss yy:MM:dd")

  def command: Parser[Command] = createPoll | simpleCommand | commandsWithoutArgs | add_question |
    success(IncorrectCommand("incorrect command"))

  def createPoll: Parser[Command] = "/create_poll" ~> anyWord ~ anonymous.? ~
    visibility.? ~ date.? ~ date.? ^^ {
    case name ~ None ~ vis ~ start ~ stop =>
      if (vis.isDefined || start.isDefined || stop.isDefined) IncorrectCommand("bad params")
      else CreatePoll(name, isAnonymous = true, vis.getOrElse(true), None, None)

    case name ~ Some(anon) ~ None ~ start ~ stop =>
      if (start.isDefined || stop.isDefined) IncorrectCommand("bad params")
      else CreatePoll(name, anon, isAfterStop = true, None, None)

    case _ ~ Some(_) ~ Some(_) ~ Some(scala.util.Failure(_)) ~ Some(scala.util.Success(_)) => IncorrectCommand("bad params")

    case poll ~ Some(anon) ~ Some(vis) ~ None ~ None => CreatePoll(poll, anon, vis, None, None)

    case poll ~ anon ~ vis ~ start ~ stop =>
      CreatePoll(poll, anon.getOrElse(true), vis.getOrElse(true), start.get.toOption, stop.get.toOption)
  }

  def simpleCommand: Parser[Command] = "/" ~> ("delete_poll" | "start_poll" |
    "stop_poll" | "result" | "begin" | "delete_question") ~ id ^^ { case commandType ~ id => commandType match {
    case "delete_poll" => DeletePoll(id)
    case "start_poll" => StartPoll(id)
    case "result" => Result(id)
    case "stop_poll" => StopPoll(id)
    case "begin" => Begin(id)
    case "delete_question" => DeleteQuestion(id)
  }
  }

  def commandsWithoutArgs: Parser[Command] = "/" ~> ("list" | "end" | "view") ^^ {
    case "list" => Listing()
    case "end" => End()
    case "view" => View()
  }

  def add_question: Parser[Command] = "/add_question" ~> anyWord ~ ("(" ~> ("open" | "choice" | "multi") <~ ")").? ~
    answers ^^ {
    case name ~ questionType ~ answers => AddQuestion(name, Question.GetValue(questionType.getOrElse("open")), answers)
  }

  def answers: Parser[Array[String]] = ".+".r.* ^^ {
    _.toArray
  }

  def id: Parser[Int] = "(" ~> "\\d+".r <~ ")" ^^ {
    _.toInt
  }

  def anonymous: Parser[Boolean] = "(" ~> ("yes" | "no") <~ ")" ^^ {
    _.toString == "yes"
  }

  def date: Parser[Try[Date]] = (("(" ~> "\\d{2}:\\d{2}:\\d{2} \\d{2}:\\d{2}:\\d{2}".r <~ ")") | anyWord) ^^
    (date => Try(dateParser.parse(date)))

  def visibility: Parser[Boolean] = "(" ~> ("afterstop" | "continuous") <~ ")" ^^ (_.toString == "afterstop")

  def anyWord: Parser[String] = "(" ~> "[^)]*".r <~ ")" ^^ {
    _.toString
  }
}

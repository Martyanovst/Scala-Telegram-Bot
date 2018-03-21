package Commands

import Old.Question
import main.scala.PollRepo

case class AddQuestion(name: String, questionType: Question.Value, answers: Array[String]) extends Command {
  override def execute: PollRepo => (String, PollRepo) = { context => ("Success!", context) }
}

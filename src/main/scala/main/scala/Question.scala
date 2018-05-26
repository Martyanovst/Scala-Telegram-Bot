package main.scala

case class Question(text: String, questionType: Question.Value, answers: Array[String],
                    usersAnswers: Map[String, String] = Map[String, String]()) {
}

object Question extends Enumeration {
  val open, choice, multi = Value

  def GetValue(text: String): Question.Value = {
    if (text == "choice") Question.choice
    if (text == "multi") Question.multi
    else
      Question.open
  }
}
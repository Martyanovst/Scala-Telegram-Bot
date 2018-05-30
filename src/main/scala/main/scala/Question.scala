package main.scala

case class Question(text: String, questionType: Question.Value, answers: Array[String],
                    usersAnswers: Map[String, String] = Map[String, String]()) {
  def toString(isAnonymous: Boolean): String = {
    val builder = new StringBuilder()
    builder.append("\nQuestion text: ")
    builder.append(text + "\n")
    builder.append("Question type: ")
    builder.append(questionType + "\n")
    if (isAnonymous) {
      if(questionType == Question.multi){
        builder.append(
          answers.map(x => x + ": " + usersAnswers.values.count
          (y => y.split(" ").contains(answers.indexOf(x) + 1))).mkString("\n"))
      }
      else if (questionType == Question.open)
        builder.append(usersAnswers.values.mkString("\n"))
      else
        builder.append(
          answers.map(x => x + ": " + usersAnswers.values.count
          (y => y.toInt == answers.indexOf(x) + 1)).mkString("\n"))
      builder.toString
    }
    else {
      if (questionType == Question.open)
        builder.append(usersAnswers.mkString("\n"))
      else
      if(questionType == Question.multi){
        builder.append(
          usersAnswers.map(x => "\n" + x._1 + ": " + x._2.split(" ").map(y=>answers(y.toInt - 1)).mkString(","))
            .mkString("\n"))
      }
      else
        builder.append(
          usersAnswers.map(x => "\n" + x._1 + ": " + answers(x._2.toInt - 1)).mkString("\n"))
      builder.toString
    }
  }
}

object Question extends Enumeration {
  val open, choice, multi = Value

  def GetValue(text: String): Question.Value = {
    if (text == "choice") Question.choice
    else if (text == "multi") Question.multi
    else Question.open
  }
}
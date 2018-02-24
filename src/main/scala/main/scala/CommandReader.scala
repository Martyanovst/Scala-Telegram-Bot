package main.scala

object CommandReader {
  def ReadCommand(rawString: String): Command ={
    Command(new Array[String](0),new Array[String](0),CommandType.Answer,isAdmin = true)
  }
}

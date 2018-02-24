package main.scala

class Command(val args: Array[String],val kwargs: Array[String], commandType: CommandType.Value, isAdmin: Boolean){
  val Args = args
  val Kwargs = kwargs
  val CommandType = commandType
  val IsAdmin = isAdmin


}

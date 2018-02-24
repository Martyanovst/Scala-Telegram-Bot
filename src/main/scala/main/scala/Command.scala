package main.scala

case class Command(args: Array[String], kwargs: Array[String], commandType: CommandType.Value, isAdmin: Boolean){
}

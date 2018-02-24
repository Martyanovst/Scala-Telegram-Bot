package main.scala

object CommandType extends Enumeration {
  val CreatePoll,List,DeletePoll,
  StartPoll,StopPoll,Result,
  Begin,End,View,AddQuestion,
  DeleteQuestion,Answer = Value
}

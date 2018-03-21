package Old
//Это старый код исполнителя комманд, вся логика перешла в классы-наследники трейта Command
//class CommandExecutor {
//  var Polls: Map[Int, Poll] = Map.empty
//  val calendar = Calendar.getInstance()
//  val now = calendar.getTime
//
//  def Run(command: Command): String = command match {
//    case _: CreatePoll => CreatePoll(command)
//    case _: DeletePoll => DeletePoll(command)
//    case _: Listing => List(command)
//    case _: Result => ShowPollingResult(command)
//    case _: StartPoll => StartPoll(command)
//    case _: StopPoll => StopPoll(command)
//  }
//
//  def CreatePoll(command: Command): String = {
//    val id = CommandExecutor.GetId
//    Polls += (id -> new Poll(Array(id.toString) ++ command.args ++ command.kwargs))
//    s"Poll id: $id"
//  }
//
//  def List(command: Command): String = Polls.values.map(poll => poll.toString).mkString
//
//  def DeletePoll(command: Command): String = Try({
//    Polls -= command.args(1).toInt
//    "Success!"
//  }).getOrElse("Failure")
//
//  def StartPoll(command: Command): String = Try({
//    val id = command.args(0).toInt
//    if (Polls(id).isHasTime)
//      return "Poll is running!!!!"
//    Polls(id).Start()
//    "Success!"
//  }).getOrElse("Incorrect Id")
//
//  def StopPoll(command: Command) = Try({
//    val id = command.args(0).toInt
//    Polls(id).Stop()
//    "Success"
//  }).getOrElse("Incorrect id")
//
//  def ShowPollingResult(command: Command) = Try({
//      val id = command.args(1).toInt
//      Polls(id).toString
//    }).getOrElse("This poll doesnt exists")
//}

object CommandExecutor {
  private var IdGenerator = 0

  def GetId = {
    IdGenerator += 1
    IdGenerator
  }
}

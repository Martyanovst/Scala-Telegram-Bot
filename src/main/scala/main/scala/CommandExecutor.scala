package main.scala

class CommandExecutor {
  val state = new State(new Array[Poll](0))
  val idGenerator = 0
  val dateParser = new java.text.SimpleDateFormat("dd-MM-yyyy")
  def execute(command: Command){}

  def executeCreation(command:Command)= {
    val name = command.args.head
    val isAnonimous = command.kwargs.head == "yes"
    val isAfterStop = command.kwargs(1) == "afterstop"
    val start = dateParser parse command.kwargs(2)
    val stop = dateParser parse command.kwargs(3)
    val variants = command.args.tail
    state.Polls :+ new Poll(name, isAnonimous, isAfterStop, start, stop, variants)
    idGenerator + 1
  }

  def executeList(command: Command)={

  }
}

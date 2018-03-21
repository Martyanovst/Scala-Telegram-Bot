package Parsers

object CommandExecutor {
  private var IdGenerator = 0

  def GetId = {
    IdGenerator += 1
    IdGenerator
  }
}

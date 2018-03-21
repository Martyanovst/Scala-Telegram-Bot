package Parsers

object IdGenerator {
  private var IdGenerator = 0

  def GetId = {
    IdGenerator += 1
    IdGenerator
  }
}

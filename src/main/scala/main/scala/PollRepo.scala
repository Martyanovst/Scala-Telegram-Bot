package main.scala

import Old.Question

case class PollRepo(polls: Map[Int,Poll], context: Int = 0)


class Poll(id: Int, name: String,isAnonymous: Boolean, isAfterstop: Boolean,start: String, end : String) {
  var started = false
  var ended = false
  var Questions = Map[Int, Question]()

  def Start() = started = true

  def Stop() = ended = true

  override def toString = {
    s"""Poll name : $name
        Poll id : $id
        Anonymous : $isAnonymous
        Show result : $isAfterstop
        Progress : ${if (started) "Started" else "Not Started"}
        Is over : ${if (ended) "Yes" else "No"}
        Questions : ${Questions.toString}
      """
  }
}
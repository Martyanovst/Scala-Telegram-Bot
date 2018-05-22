import Commands.{CreatePoll, DeletePoll, Listing}
import main.scala.PollRepo
import org.scalatest.FlatSpec

class Tests extends FlatSpec {

  "After initialization poll repo's map" should "be empty" in {
    assert(PollRepo().polls.isEmpty)
  }

  "After Initialization poll repo's context id" should "be zero" in {
    assertResult(0) {
      PollRepo().context
    }
  }
/*
  "Poll repo" should "contains new poll after create poll" in {
    val (_, actual) = CreatePoll("test") execute PollRepo()
    assert(actual.polls.nonEmpty)
    assertResult("test") {
      actual.polls(1).getName
    }
  }
*/
  "When poll repo is empty Listing" should "be empty" in {
    val (message, _) = Listing() execute PollRepo()
    assert(message.isEmpty)
  }

  "Poll repo" should "delete poll if it contains" in {
    val (_, ctx) = CreatePoll("test") execute PollRepo()
    val id = ctx.polls.keys.head
    val (_, actual) = DeletePoll(id) execute ctx
    assert(actual.polls.isEmpty)
  }

  "Poll repo" should "not delete anything when delete incorrect poll id" in {
    val (_, ctx) = CreatePoll("test") execute PollRepo()
    val (_, actual) = DeletePoll(1000) execute ctx
    assert(actual.polls.nonEmpty)
  }
  //TODO
  // On the Future
  // Not implemented
  //  "Context" should "update after begin command" in {
  //    val (_, actual) = Begin(2) execute PollRepo()
  //    assertResult(2) {
  //      actual.context
  //    }
  //  }
}

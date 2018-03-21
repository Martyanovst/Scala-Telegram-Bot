package Commands

import main.scala.PollRepo

trait Command {
  def execute: PollRepo => (String, PollRepo)
}

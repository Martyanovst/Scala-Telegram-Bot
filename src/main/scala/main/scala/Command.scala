package main.scala


trait Command {
  def execute: PollRepo => (String, PollRepo)
}

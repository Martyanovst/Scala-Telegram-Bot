package main.scala
import java.util.Date

class Poll(name: String, isAnonimous: Boolean = true, isafterstop: Boolean = true,
           start: Date, stop: Date, variants: Array[String]){
  val Name = name
  val IsAnonimous = isAnonimous
  val IsAfterStop = isafterstop
  val Start = start
  val Stop = stop
  val Variants = variants

  def toString(): String ={

  }
}

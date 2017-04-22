package chapter4

/**
  * Created by KOHEI on 2017/04/22.
  */
object result {
  def main(args: Array[String]): Unit = {
    val test = null
    println(Option(test).getOrElse(0))
  }
}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

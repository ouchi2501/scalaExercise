package chapter3

/**
  * Created by KOHEI on 2017/03/04.
  */
object result {
  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println(x)
    println(List.tail(List(1, 2, 3, 4, 5)))
    println(List.setHead(0, List(1, 2, 3, 4, 5)))
    println(List.drop(List(1, 2, 3, 4, 5), 2))
    println(List.dropWhile(List(1, 2, 3, 4, 5))(a => a < 4))
    println(List.append(List(1, 2, 3, 4), List(5, 6, 7, 8, 9)))
    println(List.init(List(1, 2, 3, 4)))
    println(List.sum2(List(1, 2, 3)))
    println(List.product2(List(2.3, 1.0)))
    println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
    println(List.length(List(1, 2, 3, 4, 5)))
    println(List.foldLeft(List(1, 2, 3, 4), 0)(_ + _))
    println(List.sum3(List(1, 2, 3)))
    println(List.product3(List(1, 2, 3)))
    println(List.length2(List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)))
    println(List.reverse(List(1, 2, 3, 4)))
    println(List.foldRightViaFoldLeft(List(1, 2, 3), 0)(_ + _))
    println(List.foldLeftViaFoldRight(List(1, 2, 3), 0)(_ + _))
    println(List.appendViaFoldRight(List(1, 2, 3), List(4, 5, 6)))
    println(List.concat(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))
  }
}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil => sys.error("setHead of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = if (n <= 0) l else l match {
    case Nil => Nil
    case Cons(_, t) => drop(t, n - 1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((b, _) => b + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((h, t) => Cons(t, h))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldRightViaFoldLeft(l, Nil: List[A])(append)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
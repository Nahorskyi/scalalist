import scala.annotation.tailrec
import scala.collection.mutable

import List._
import List.*

enum List[+A]:
  case Nil
  case Cons(hd: A, tl: List[A])
  
  def foldLeft[B](x: B)(f: (B, A) => B): B = {
    def inner(xs: List[A], acc: B): B = xs match {
      case Nil => acc
      case Cons(xh, xt) => inner(xt, f(acc, xh))
    }
    inner(this, x)
  }

  def foldRight[B](x: B)(f: (A, B) => B): B = {
    this match {
      case Nil => x
      case Cons(xh, xt) => f(xh, xt.foldRight(x)(f))
    }
  }

  def length(xs: List[Int]): Int = xs match {
    case Nil => 0 
    case Cons(xh,xt) => 1 + length(xt)
  }



  def isEmpty: Boolean =
    this match {
      case Nil        => true
      case Cons(_, _) => false
  }



  override def toString: String = {
    def go(sb: StringBuilder, xs: List[A]): String =
      {
        xs match
          case Nil => sb.append("]").result
          case Cons(xh,xt) => 
            go(sb.append(xh).append(if xt == Nil then "" else ", "),xt)
      }
    go(new StringBuilder("["),this)
  }


object List:
  def empty[A]: List[A] = Nil
  def apply[A](xs: A*): List[A] = of(xs*)
  def of[A](xs: A*): List[A] = xs.foldRight(Nil: List[A]) { case (x, acc) => Cons(x, acc) }
  
  def drop[A](xs: List[A], n: Int): List[A] ={
    if (n == 0) xs
    else xs match {
      case Nil => Nil
      case Cons(_, xt) => drop(xt, n - 1)
    }
  }

  def take[A]( xs: List[A], n: Int, acc: List[A] = Nil): List[A] = {
    xs match {
      case Nil => acc
      case Cons(xh, xt) => {
        if (n <= 0) acc 
        else 
          Cons(xh, take(xt,n-1,acc))
      }
    }
  }  

  def forall[A](xs: List[A], p: A => Boolean): Boolean = {
    if (xs.isEmpty) true
    else xs match {
      case Nil => xs==Nil
      case Cons(xh, xt) =>
        if (p(xh))
          forall(xt, p) && true
        else
          false
    }
  }


@main def main(): Unit = {
  println(forall(List(1,3,5,7), x => x % 2 == 1))
  var l1 = List(1,2,3,4,5,7)
  println(take(l1,3))
  println(drop(l1,3))
}

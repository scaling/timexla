package timexla

import scala.math._

// This little module was provided by Jason Baldridge, presumably under a WTFPL

object QuickMath {
  def euclid(x: List[Double], y: List[Double]): Double = sqrt((for ((a,b) <- x.zip(y)) yield pow(a-b,2)) sum)
  def dot(x: List[Double], y: List[Double]): Double = (for ((a,b) <- x.zip(y)) yield a*b) sum
  def norm(x: List[Double]): Double = sqrt(x map(s => s*s) sum)
  def cosine(x: List[Double], y: List[Double]): Double = dot(x,y)/(norm(x)*norm(y))
  def kl(x: List[Double], y: List[Double]): Double = (for ((a,b) <- x.zip(y)) yield a * log(a/b)) sum
  def avg(x: List[Double], y: List[Double]): List[Double] = for ((a,b) <-x.zip(y)) yield (a+b)/2
  def avg(x: Iterable[Double]): Double = x.sum / x.size
  // def avg[A](x: Iterable[A])(implicit numeric: Iterable[Numeric[A]]): A = x.sum / x.size
  def js(x: List[Double], y: List[Double]): Double = kl(x,avg(x,y)) + kl(y,avg(x,y))
}

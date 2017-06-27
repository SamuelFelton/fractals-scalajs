package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.{document, html}

import scala.scalajs.js.JSApp
import scala.util.Random

case class Vector(x: Double, y: Double) {
  def *(v: Vector) = Vector(x * v.x, y * v.y)

  def *(l: Double) = Vector(l * x, l * y)

  def magnitude = Math.sqrt(x * x + y * y)

  def unary_~ = {
    val m = magnitude
    Vector(x / m, y / m)
  }

  def -(v: Vector) = Vector(x - v.x, y - v.y)

  def +(v: Vector) = Vector(x + v.x, y + v.y)

  def +(d: Double) = Vector(x + d, y + d)

}

case class Complex(x: Double, y: Double) {
  def +(o: Complex) = Complex(x + o.x, y + o.y)

  def +(d: Double) = Complex(x + d, y)

  def -(o: Complex) = Complex(x - o.x, y - o.y)

  def unary_- = Complex(-x, -y)

  def *(o: Complex) = Complex(x * o.x - y * o.y, x * o.y + y * o.x)

  def *(d: Double) = Complex(x * d, y * d)


  def modulus = if (x != 0.0 || y != 0.0) Math.sqrt(x * x + y * y) else 0.0

  def conjugate = Complex(x, -y)

  def /(c: Complex) = (this, c) match {
    case (Complex(a: Double, b: Double), Complex(c: Double, d: Double)) => Complex((a * c + b * d) / (c * c + d * d), (b * c - a * d) / (c * c + d * d))
  }

  def /(d: Double) = Complex(x / d, y / d)

  def argument = Math.atan2(y, x)
}

object Complex {
  def sqrt(c: Complex) = {
    val r = Math.sqrt(c.modulus)
    val theta = c.argument / 2.0
    Complex(Math.cos(theta), Math.sin(theta)) * r
  }


  def sqrts(c: Complex) = {
    //println(c)
    val r = Math.sqrt(c.modulus)
    val theta = c.argument / 2.0
    val primaryRoot = Complex(Math.cos(theta), Math.sin(theta)) * r
    List(primaryRoot)
  }
}

case class Circle(radius: Double, pos: Vector) {
  val curvature = 1.0 / radius

  def bend(p: Vector) = if ((p - pos).magnitude < radius) -curvature else curvature


  def draw(color:String = "black")(implicit ctx: dom.CanvasRenderingContext2D) = {
    ctx.beginPath()
    ctx.fillStyle = color
    ctx.arc(pos.x, pos.y, radius, 2 * Math.PI, 0)

    ctx.fill()

  }
}

object Util {
  def approxEqual(d1: Double, d2: Double) = Math.abs(Math.abs(d1) - Math.abs(d2)) < 0.001

}

object Circle {
  def areEqual(c1: Circle, c2: Circle) = {
    Util.approxEqual(c1.pos.x, c2.pos.x) && Util.approxEqual(c1.pos.y, c2.pos.y) && Util.approxEqual(c1.radius, c2.radius)
  }

  def areTangent(c1: Circle, c2: Circle) = {
    val diff = c1.pos - c2.pos
    Util.approxEqual(diff.x * diff.x + diff.y * diff.y, Math.pow(c1.radius + c2.radius, 2)) ||
      Util.approxEqual(diff.x * diff.x + diff.y * diff.y, Math.pow(c1.radius - c2.radius, 2))
  }
}

object TutorialApp extends JSApp {


  def curvatureNextCircle(c1: Double, c2: Double, c3: Double): List[Double] = {
    val a = c1 + c2 + c3
    val root = 2.0 * Math.sqrt(c1 * c2 + c1 * c3 + c2 * c3)
    List(a + root, a - root)
  }

  def positionsNextCircle(z1: Complex, z2: Complex, z3: Complex, k1: Double, k2: Double, k3: Double): List[Complex] = {
    val bz1 = z1 * k1
    val bz2 = z2 * k2
    val bz3 = z3 * k3

    val addition = bz1 + bz2 + bz3
    val root = Complex.sqrt(bz1 * bz2 + bz1 * bz3 + bz2 * bz3) * 2.0
    List(addition + root, addition - root)
  }

  def kissingCircles(c1: Circle, c2: Circle, c3: Circle): List[Circle] = {
    implicit def toComplex(v: Vector): Complex = Complex(v.x, v.y)
    implicit def toVector(c: Complex): Vector = Vector(c.x, c.y)

    val curvatures: List[Double] = curvatureNextCircle(c1.bend(c2.pos), c2.bend(c1.pos), c3.bend(c2.pos))
    val positions: List[Complex] = positionsNextCircle(c1.pos, c2.pos, c3.pos, c1.bend(c2.pos), c2.bend(c1.pos), c3.bend(c2.pos))
    val parentCircles = List(c1, c2, c3)
    for {
      curve <- curvatures
      pos <- positions
      c: Circle = Circle(1.0 / curve, pos / curve)
      if curve > 0.0 && parentCircles.forall(Circle.areTangent(_, c)) && c.radius < parentCircles.map(_.radius).min
    } yield Circle(1.0 / curve, pos / curve)
  }

  val colors = List.fill(15)(s"rgb(${Random.nextInt(250)},${Random.nextInt(250)},${Random.nextInt(250)})")
  def main(): Unit = {
    def startingCircles(outer: Circle) = {
      (Circle(outer.radius / 2.0, Vector(outer.pos.x - outer.radius / 2.0, outer.pos.y)), Circle(outer.radius / 2.0, Vector(outer.pos.x + outer.radius / 2.0, outer.pos.y)))
    }
    val size = 1000.0
    val offset = 500.0
    val n = 9
    val cvs = document.getElementById("canvas") match {
      case e: html.Canvas => Some(e)
      case _ => None
    }
    implicit val ctx = cvs.map(e => e.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]).getOrElse(throw new Error())
    val c1 = Circle(size / 2.0, Vector(size / 2, size / 2) + offset)
    val (c2, c3) = startingCircles(c1)

    ctx.lineWidth = 1
    c1.draw(colors(n))
    c2.draw(colors(n))
    c3.draw(colors(n))
    draw(c1, c2, c3, n)

    val (c22, c23) = startingCircles(c2)
    c22.draw(colors(n - 1))
    c23.draw(colors(n - 1))

    draw(c2, c22, c23, n)
    val (c32, c33) = startingCircles(c3)
    c32.draw(colors(n - 1))
    c33.draw(colors(n - 1))

    draw(c3, c32, c33, n)
    //draw(c1,c2,c4, 3)
    //draw(c1, c2, c3, c4, 0)

    //draw(c1, c2, c3, 2)

  }

  def draw(c1: Circle, c2: Circle, c3: Circle, n: Int)(implicit ctx: dom.CanvasRenderingContext2D): Unit = {
    if (n <= 0) Unit
    else {
      val ks = kissingCircles(c1, c2, c3)
      ks.foreach(_.draw(colors(n)))
      //ks.foreach(k => println(s"${(c1,c2,c3)} = ${k}"))
      ks.flatMap(k => List((c1, c2, k), (c1, c3, k), (c2, c3, k))).foreach(o => draw(o._1, o._2, o._3, n - 1))
    }
  }
}
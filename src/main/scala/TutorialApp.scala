package tutorial.webapp

import org.scalajs.dom
import dom.document
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLCanvasElement

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

  def *(o: Complex) = Complex(x * o.x - y * o.y, x * o.y + y * o.x)

  def *(d: Double) = Complex(x * d, y * d)

  def modulus = Math.sqrt(x * x + y * y)

  def conjugate = Complex(x, -y)

  def /(c: Complex) = (this, c) match {
    case (Complex(a: Double, b: Double), Complex(c: Double, d: Double)) => Complex((a * c + b * d) / (c * c + d * d), (b * c - a * d) / (c * c + d * d))
  }

  def /(d: Double) = Complex(x / d, y / d)

}

object Complex {
  def sqrt(c: Complex) = {
    val r = c.modulus
    val modZR = (c + r).modulus
    ((c + r) / modZR) * Math.sqrt(r)
  }
}

case class Circle(radius: Double, pos: Vector) {
  val curvature = 1.0 / radius

  def bend(p: Vector) = if ((p - pos).magnitude < radius) -curvature else curvature


  def draw(color:String = "black")(implicit ctx: dom.CanvasRenderingContext2D) = {
    ctx.beginPath()
    ctx.strokeStyle = color
    ctx.arc(pos.x, pos.y, radius, 2 * Math.PI, 0)
    ctx.stroke()
  }
}

object TutorialApp extends JSApp {


  def curvatureNextCircle(c1: Double, c2: Double, c3: Double): Double = c1 + c2 + c3 + 2 * Math.sqrt(c1 * c2 + c1 * c3 + c2 * c3)

  def positionsNextCircle(z1: Complex, z2: Complex, z3: Complex, k1: Double, k2: Double, k3: Double): List[Complex] = {
    val bz1 = z1 * k1
    val bz2 = z2 * k2
    val bz3 = z3 * k3
    println(s"z3 = ${z3}, k3 = ${k3}")
    println(bz1,bz2,bz3)

    val addition = bz1 + bz2 + bz3
    val root = Complex.sqrt(bz1 * bz2 + bz1 * bz3 + bz2 * bz3) * 2
    List(addition - root, addition + root)
  }

  def kissingCircles(c1: Circle, c2: Circle, c3: Circle): List[Circle] = {
    implicit def toComplex(v: Vector): Complex = Complex(v.x, v.y)
    val curvature = curvatureNextCircle(c1.bend(c2.pos), c2.bend(c1.pos), c3.bend(c2.pos))
    positionsNextCircle(c1.pos, c2.pos, c3.pos, c1.bend(c2.pos), c2.bend(c1.pos), c3.bend(c2.pos)).map(c => Vector(c.x / curvature, c.y / curvature)).map(Circle(1 / curvature, _))
  }

  def main(): Unit = {
    val size = 500
    val offset = 250.0
    val cvs = document.getElementById("canvas") match {
      case e: html.Canvas => Some(e)
      case _ => None
    }
    implicit val ctx = cvs.map(e => e.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]).getOrElse(throw new Error())
    val c1 = Circle(size / 2.0, Vector(size / 2, size / 2) + offset)
    val c2 = Circle(size / 4.0, Vector(size / 4, size / 2) + offset)
    val c3 = Circle(size / 4.0, Vector(3 * size / 4, size / 2) + offset)

    c1.draw()
    c2.draw()
    c3.draw()
    val c4 = kissingCircles(c1, c2, c3)(0)
    c4.draw()
    //kissingCircles(c2,c3,c4).foreach(c => {println(c);c.draw("red")})
    //kissingCircles(c1, c2, c4).map(_.draw("red"))
    //draw(c1,c2,c3, 2)
    //draw(c1, c2, c3, c4, 0)

    //draw(c1, c2, c3, 2)

  }

  def draw(c1: Circle, c2: Circle, c3: Circle, n: Int)(implicit ctx: dom.CanvasRenderingContext2D): Unit = {
    if (n == 0) Unit
    else {
      val k = kissingCircles(c1,c2,c3).head
      k.draw("red")
      List((c1,c2,k),(c1,c3,k),(c2,c3,k)).foreach(o => draw(o._1,o._2,o._3,n-1))

    }
  }
}
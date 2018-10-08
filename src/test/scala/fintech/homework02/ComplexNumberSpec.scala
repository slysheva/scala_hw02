package fintech.homework02
import org.scalatest.{FlatSpec, Matchers}

class ComplexNumberSpec extends FlatSpec with Matchers {
  val eps = 1e-7

  "Sum of complex numbers" should "be correct" in {
    val a = new ComplexNumber(2, 7)
    val b = new ComplexNumber(5, 4)
    val actual = a + b
    val expected = new ComplexNumber(7, 11)
    math.abs(actual.real - expected.real) should be <= eps
    math.abs(actual.imaginary - expected.imaginary) should be <= eps
  }

  "Complex numbers with equal real and imaginary parts" should "be equal" in{
    val a = new ComplexNumber(2, 7)
    val b = new ComplexNumber(2, 7)
    a == b should be (true)
  }

  "Complex numbers with different real and imaginary parts" should "not be equal" in{
    val a = new ComplexNumber(2, 7)
    val b = new ComplexNumber(3, 7)
    a == b should be (false)
  }

  "Equal complex numbers" should "have equal hash code" in {
    val a = new ComplexNumber(2, 7)
    val b = new ComplexNumber(2, 7)
    a.hashCode() == b.hashCode() should be(true)
  }

  "Power function for complex number" should "be correct" in {
    val a = new ComplexNumber(2, 7)
    val actual = a ~ 3
    val expected = new ComplexNumber(-286, -259)
    math.abs(actual.real - expected.real) should be <= eps
    math.abs(actual.imaginary - expected.imaginary) should be <= eps
  }

  "Multiply function for complex numbers" should "be correct" in {
    val a = new ComplexNumber(2, 7)
    val b = new ComplexNumber(5, 4)
    val actual = a * b
    val expected = new ComplexNumber(-18, 43)
    math.abs(actual.real - expected.real) should be <= eps
    math.abs(actual.imaginary - expected.imaginary) should be <= eps
  }

  "Complex number in zero power" should "be 1" in {
    val a = new ComplexNumber(2, 7)
    val actual = a ~ 0
    val expected = new ComplexNumber(1,0)
    math.abs(actual.real - expected.real) should be <= eps
    math.abs(actual.imaginary - expected.imaginary) should be <= eps
  }

  "Complex number in negative power" should "be correct" in {
    val a = new ComplexNumber(2, 7)
    val actual = a ~ -3
    val expected = new ComplexNumber(- 0.00192104,0.00173969)
    math.abs(actual.real - expected.real) should be <= eps
    math.abs(actual.imaginary - expected.imaginary) should be <= eps
  }

  "String version of complex number" should "be in format \"a + bi\"" in {
    val a = new ComplexNumber(2, 7)
    val actual = a.toString
    val expected = "2.0 + 7.0i"
    actual == expected  should be (true)
  }

  "String version of complex number with negative imaginary part" should "be in format \"a - bi\"" in {
    val a = new ComplexNumber(2, -7)
    val actual = a.toString
    val expected = "2.0 - 7.0i"
    actual == expected should be (true)
  }

  "String version of complex number with zero real part" should "be in format \"bi\"" in {
    val a = new ComplexNumber(0, 7)
    val actual = a.toString
    val expected = "7.0i"
    actual == expected should be (true)
  }

  "String version of complex number with zero imaginary part" should "be in format \"a\"" in {
    val a = new ComplexNumber(2, 0)
    val actual = a.toString
    val expected = "2.0"
    actual == expected should be (true)
  }
}
package fintech.homework02

class ComplexNumber(val real: Double, val imaginary: Double) {
  def +(other: ComplexNumber): ComplexNumber =
    new ComplexNumber(real + other.real, imaginary + other.imaginary)

  def *(other: ComplexNumber): ComplexNumber = {
    val realPart = real * other.real - imaginary * other.imaginary
    val imaginaryPart = real * other.imaginary + imaginary * other.real
    new ComplexNumber(realPart, imaginaryPart)
  }

  def ~(pow: Int): ComplexNumber = {
    val absoluteValue = math.sqrt(real * real + imaginary * imaginary)
    val argument = math.atan(imaginary / real)
    val absoluteValueInPow = math.pow(absoluteValue, pow)
    new ComplexNumber(math.cos(pow * argument) * absoluteValueInPow,
                      math.sin(pow * argument) * absoluteValueInPow)
  }

  override def hashCode(): Int = real.hashCode() ^ imaginary.hashCode()

  override def equals(other: Any): Boolean =
    other match {
      case that : ComplexNumber => this.real.equals(that.real) && this.imaginary.equals(that.imaginary)
      case _ => false
    }

  override def toString: String = {
    if (imaginary == 0)
      real.toString
    else if (real == 0)
      imaginary.toString + 'i'
    else if (imaginary > 0)
      "%s + %si".format(real.toString, imaginary.toString)
    else
      "%s - %si".format(real.toString, math.abs(imaginary).toString)
  }

  // Написать класс описывающий комплексные числа.
  // Реализовать проверку на равенство, умножение и сложение, toString.
  // Реализовать оператор возведения в целую степень: "~".
  // Реализовать тесты в ComplexNumberSpec
}

import org.specs2.mutable.Specification

class ExpressionTest extends Specification {
  "Calculator" >> {
    "Evaluates a complex expression" >> {
      Subtraction(Number(10), Addition(Number(5), Number(4))).eval === Success(1)
    }

    "Failure divide by zero" >> {
      Subtraction(Number(10), Division(Number(5), Number(0))).eval === Failure("Divide by zero")
    }

    "Failure square root of negative number" >> {
      SquareRoot(Number(-4)).eval === Failure("Square root of negative number")
    }
  }
}

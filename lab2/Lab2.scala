package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * Nolan Cretney
   *
   * Partner: Kelsey Dowd
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with  your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   *
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(b) => if (b) 1.0 else 0.0
      case S(s) =>
        try{s.toDouble} // think converting "23" to 23
        catch{case _ : NumberFormatException => Double.NaN}// think converting "hello" to a number, it shouldnt happen
      case Undefined => Double.NaN
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case S(s) => if (!s.isEmpty) true else false
      case N(n) => n match {
        case Double.NaN => false
        case 0.0 | -0.0 => false
        case _ => true
      }
      case Undefined => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => prettyNumber(n)
      case S(s) => s
      case B(b) => if(b) "true" else "false"
      case Undefined => "undefined"
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(n) => N(n)
      case S(s) => S(s)
      case B(b) => B(b)
      case Undefined => Undefined

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      case Var(x) => lookup(env,x)

      case ConstDecl(x, e1, e2) => {
        // map x to new binding, get a new env
        // recursivley call eval on e1 to get to bottom most layer
        val newEnv = extend(env,x,eval(env,e1))

        //eval the the env with the 2nd expression
        eval(newEnv,e2)
      }

      case Binary(Plus,e1,e2) => (eval(env, e1), eval(env, e2)) match {
        case(S(x),e2) => S(x + toStr(e2))
        case(e1,S(y)) => S(toStr(e1) + y)
        case(e1,e2) => N(toNumber(e1) + toNumber(e2))
      }

      case Binary(Minus,e1,e2) => (eval(env, e1), eval(env, e2)) match {
        case(e1,e2) => N(toNumber(e1) - toNumber(e2))
      }

      case Binary(And,e1,e2) => (eval(env, e1), eval(env, e2)) match {
        case(S(x),e2) => if(x.isEmpty) S(x) else e2
        case(e1,e2) => if(toBoolean(e1)) e2 else e1
      }

      case Binary(Or,e1,e2) => (eval(env, e1), eval(env, e2)) match {
        case(S(x),e2) => if(x.isEmpty) e2 else S(x)
        case(e1,e2) => if(toBoolean(e1)) e1 else e2

      }

      case Binary(Times,e1,e2) => (eval(env, e1), eval(env,e2)) match {
        case(e1, e2) => N(toNumber(e1) * toNumber(e2))
      }

      case Binary(Div,e1,e2) => (eval(env,e1), eval(env,e2)) match {
        case(N(x),N(0.0)) => if(x > 0) N(Double.PositiveInfinity) else if(x < 0) N(Double.NegativeInfinity) else N(Double.NaN)
        case(N(x),N(-0.0)) => if(x > 0) N(Double.NegativeInfinity) else if(x < 0) N(Double.PositiveInfinity) else N(Double.NaN)
        case(B(x),N(0.0)) => if(x) N(Double.PositiveInfinity) else N(Double.NaN)
        case(B(x),N(-0.0)) => if(x) N(Double.NegativeInfinity) else N(Double.NaN)
        case(S(x), N(0.0)) =>
          val num = toNumber(S(x))
          if(num != Double.NaN){
            if(num > 0) N(Double.PositiveInfinity)
            else if(num < 0) N(Double.NegativeInfinity)
            else N(Double.NaN)
          }
          else N(Double.NaN)
        case(S(x), N(-0.0)) =>
          val num = toNumber(S(x))
          if(num != Double.NaN){
            if(num > 0) N(Double.NegativeInfinity)
            else if(num < 0) N(Double.PositiveInfinity)
            else N(Double.NaN)
          }
          else N(Double.NaN)
        case(e1,e2) => N(toNumber(e1)/toNumber(e2))
      }

      case Binary(Eq,e1,e2) => (eval(env,e1), eval(env,e2)) match {
        case (S(x), N(y)) => B(false)
        case (N(x), S(y)) => B(false)
        case (e1,e2) => B(toNumber(e1) == toNumber(e2))
      }

      case Binary(Ne,e1,e2) => (eval(env,e1), eval(env,e2)) match {
        case(S(x),N(y)) => if(toNumber(S(x)) == y) B(false) else B(true)
        case(N(x),S(y)) => if(x == toNumber(S(y))) B(false) else B(true)
        case(e1,e2) => B(toNumber(e1) != toNumber(e2))
      }

      case Binary(Gt,e1,e2) => (eval(env,e1), eval(env,e2)) match {
        case (S(x),S(y)) =>
          val compare = x.compareTo(y)
          if (compare > 0) B(true)
          else (B(false))
        case (e1,e2) => B(toNumber(e1) > toNumber(e2))
      }

      case Binary(Ge,e1,e2) => (eval(env,e1), eval(env,e2)) match {
        case (S(x),S(y)) =>
          val compare = x.compareTo(y)
          if (compare >= 0) B(true)
          else (B(false))
        case (e1,e2) => B(toNumber(e1) >= toNumber(e2))

      }

      case Binary(Le,e1,e2) => (eval(env,e1), eval(env,e2)) match {
        case (S(x),S(y)) =>
          val compare = x.compareTo(y)
          if (compare <= 0) B(true)
          else (B(false))
        case (e1,e2) => B(toNumber(e1) <= toNumber(e2))
      }

      case Binary(Lt,e1,e2) => (eval(env,e1), eval(env,e2)) match {
        case (S(x),S(y)) =>
          val compare = x.compareTo(y)
          if (compare < 0) B(true)
          else (B(false))
        case (e1,e2) => B(toNumber(e1) < toNumber(e2))
      }

      case Unary(Neg,e1) => eval(env,e1) match {
        case (e1) => N(-toNumber(e1))

      }

      case Unary(Not,e1) => eval(env, e1) match {
        case e1 => B(!toBoolean(e1))
      }

      case If(e1,e2,e3) => eval(env, e1) match {
        case e1 => if(toBoolean(e1)) eval(env,e2) else eval(env,e3)
      }

      case Binary(Seq,e1,e2) => (eval(env, e1), eval(env, e2)) match {
        case (e1, e2) => e2
      }

      //case _ => ???
    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}

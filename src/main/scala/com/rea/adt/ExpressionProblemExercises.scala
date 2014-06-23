package com.rea.adt


sealed trait Expr
case class Const(value: Boolean) extends Expr
case class And(a: Expr, b: Expr) extends Expr
case class Or(a: Expr, b: Expr) extends Expr
case class Not(expr: Expr) extends Expr

/**
 * Use pattern matching and recursion.  No vars, no loops, no overriding.
 */
object Expr {
  
  /**
   * Evaluate the expression.
   */
  def eval(expr: Expr): Boolean = expr match {
    case Const(value) => value
    case And(a, b)    => eval(a) && eval(b)
    case Or(a, b)     => eval(a) || eval(b)
    case Not(expr)    => !eval(expr)
  }
  
  /**
   * Normalise the expression, such that: 
   * !!a     ==> a
   * !a & !b ==> !(a | b)
   * !a | !b ==> !(a & b)
   */
  def normalise(expr: Expr): Expr = expr match {
    case Not(Not(a)) => normalise(a)
    case And(Not(a), Not(b)) => Not(Or(normalise(a), normalise(b)))
    case Or(Not(a), Not(b)) => Not(And(normalise(a), normalise(b)))
    case _ => expr
  }
  
  /**
   * Show, using English lower-case words "and", "or", "not", "true", "false"
   */
  def show(expr: Expr): String = expr match {
    case Const(value) => if (value) "true" else "false"
    case And(a, b)    => show(a) + " and " + show(b)
    case Or(a, b)     => show(a) + " or "  + show(b)
    case Not(expr)    => "not " + show(expr)
  }

}

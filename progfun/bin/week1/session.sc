package week1

object session {
  def abs(x:Double) = if (x < 0) -x else x        //> abs: (x: Double)Double

def sqrtIter(guess: Double, x: Double): Double =
   if (isGoodEnough(guess, x)) guess
   else sqrtIter(improve(guess, x), x)            //> sqrtIter: (guess: Double, x: Double)Double

def isGoodEnough(guess: Double, x: Double) = ???  //> isGoodEnough: (guess: Double, x: Double)Nothing

def improve(guess: Double, x: Double) =
   (guess + x / guess) / 2                        //> improve: (guess: Double, x: Double)Double

def sqrt(x: Double) = sqrtIter(1.0, x)            //> sqrt: (x: Double)Double
}
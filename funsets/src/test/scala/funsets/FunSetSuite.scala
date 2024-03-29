package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }
  
  test("intersect should contain one element"){
    new TestSets {
      val s = union(s1, s2)
      val tested  = intersect(s1, s)
      assert(contains(tested, 1), "intersect 1")
    }
  }
  
  test("diff should contain one element"){
    new TestSets {
      val s = union(s1, s2)
      val tested  = diff(s, s1)
      assert(contains(tested, 2), "intersect 1")
    }
  }
  
  test("diff of {1,3,4,5,7,1000} and {1,2,3,4} should be {5,7,1000}"){
    val left = union(_==1,union(_==3,union(_==4,union(_==5,union(_==7,_==1000)))))
    val right = union(_==1,union(_==2,union(_==3,_==4)))
    val test = diff(left, right)
    assert(contains(test, 5))
    assert(contains(test, 7))
    assert(contains(test, 1000))
    assert(!contains(test, 1))
    assert(!contains(test, 2))
    assert(!contains(test, 3))
    assert(!contains(test, 4))
  }
  
  test("forall for set 1,2,3,4,5 by function _>0 && _<=5 with predicate _<= 10 say OK"){
    val tested: Set = e => e>0 && e<=5
    assert(forall(tested, _<=10), "forall")
  }
  
  test("exists for set 1,2,3,4,5 by function _>0 && _<=5 with predicate _==4 say OK and with pred _==10 say NO"){
    val tested: Set = e => e>0 && e<=5
    assert(exists(tested, _==4), "exists")
    assert(!exists(tested, _==10), "exists")
  }

  test("map for set 1,2,3,4,5 by function _>0 && _<=5 with map function _+1000 should contain 1001"){
    val tested: Set = map(e => e>0 && e<=5, _+1000)
    assert(contains(tested, 1001), "contains")
  }
  
  
  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
}

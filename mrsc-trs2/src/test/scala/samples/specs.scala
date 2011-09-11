package samples

import org.junit.runner.RunWith
import org.specs2._
import org.specs2.matcher._
//import org.specs2.runner.{ JUnitSuiteRunner, JUnit }

import org.specs2.runner.JUnitRunner

//import org.scalacheck.Gen

/**
 * Sample specification.
 * 
 * This specification can be executed with: scala -cp <your classpath=""> ${package}.SpecsTest
 * Or using maven: mvn test
 *
 * For more information on how to write or run specifications, please visit: http://code.google.com/p/specs.
 *
 */
@RunWith(classOf[JUnitRunner])
class MySpecTest extends mutable.Specification {
  args(sequential = true)

  "My" should {
    "allow " in {
      1 must_== 1
      //0
    }
    "deny " in {
      1 must_!= 0
      //0
    }
  }
  
  "A List" should {
    "have a size method returning the number of elements in the list" in {
      List(1, 2, 3).size must_== 3
    }
    // add more examples here
    // ...
  }

}

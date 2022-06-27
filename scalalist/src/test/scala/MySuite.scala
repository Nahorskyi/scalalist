import List._
import List.*

class MySuite extends munit.FunSuite {
  
  test("string builder test 1"){
    val expected = "[1, 2, 3, 4, 5, 6]"
    val actual = List.of(1,2,3,4,5,6).toString
    assertEquals(expected,actual)
  }

  test("string builder test 2"){
    val expected = "[a, b, c, d]"
    val actual = List.of("a","b","c","d").toString
    assertEquals(expected,actual)
  } 

  test("drop test 1"){
    val expected = List(4)
    val actual = (drop(List(1,2,3,4),3))
    assertEquals(expected , actual)
  }

  test("drop test 2"){
    val expected = List(2,3,4)
    val actual = (drop(List(1,2,3,4),1))
    assertEquals(expected , actual)
  }  

  test("drop test 3"){
    val expected = List("c","d")
    val actual = (drop(List("a","b","c","d"),2))
    assertEquals(expected , actual)
  }     

  test("drop test 4"){
    val expected = List(3,4,5,6,7)
    val actual = (drop(List(1,2,3,4,5,6,7),2))
    assertEquals(expected , actual)
  }

  test("take test 1"){
    val expected = List(1,2,3,4,5,6,7)
    val actual = (take(List(1,2,3,4,5,6,7),100))
    assertEquals(expected , actual)
  }

  test("take test 2"){
    val expected = List(1,2,3)
    val actual = (take(List(1,2,3,4,5,6,7),3))
    assertEquals(expected , actual)
  }  

  test("take test 3"){
    val expected = List("a","b",2,3)
    val actual = (take(List("a","b",2,3,"f","g",7),4))
    assertEquals(expected , actual)
  }

  test("take test 4"){
    val expected = List(1)
    val actual = (take(List(1,2,3,4,5,6,7),1))
    assertEquals(expected , actual)
   }    

  test("foldLeft test1") {
    val obtained = 6
    val expected = List(1,2,3).foldLeft(1)(_*_)
    assertEquals(obtained, expected)
  }

  test("foldLeft test2") {
    val obtained = 10
    val expected = List(1,2,3,4).foldLeft(0)(_+_)
    assertEquals(obtained, expected)
  }
  
  test("foldLeft test3") {
    val obtained = 0
    val expected = List(0,1,2,3,4,5,6,7).foldLeft(1)(_*_)
    assertEquals(obtained, expected)
  }
  
  test("foldLeft test4") {
    val obtained = -15
    val expected = List(10,5).foldLeft(0)(_-_)
    assertEquals(obtained, expected)
  }

  test("forall test1") {
    val obtained = true
    val expected = forall(List(1,3,5,7), x => x % 2 == 1)
    assertEquals(obtained, expected)
  }

  test("forall test2") {
    val obtained = true
    val expected = forall(List(1), x => x % 2 == 1)
    assertEquals(obtained, expected)
  }

  test("forall test3") {
    val obtained = false
    val expected = forall(List(1,2,3,7,9), x => x % 2 == 1)
    assertEquals(obtained, expected)
  }

  test("forall test4") {
    val obtained = false
    val expected = forall(List(2,2,2,2,2), x => x % 2 == 1)
    assertEquals(obtained, expected)
  }
  test("isEmpty test1"){
    val obtained = false
    val expected = List(1,2,3,4,5).isEmpty
  }
  test("isEmpty test2"){
    val obtained = true
    val expected = List().isEmpty
  }
}
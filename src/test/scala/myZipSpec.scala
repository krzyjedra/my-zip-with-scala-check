import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks.whenever
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll

class myZipSpec extends AnyFlatSpec with should.Matchers {

  import myZip._

  "myzip" should "intertwines two sequences, " in {
    //  check((a: List[Int], b: List[String]) => myzip(a, b).size == (a ::: b).size)
    forAll { (a: List[Int], b: List[String]) =>
      whenever(b != 0 && b != Integer.MIN_VALUE
        && a != Integer.MIN_VALUE) {
        myzip(a, b).dropRight(1) should be === List(a.tail, b.tail).dropRight(1)
      }
    }
  }
  //        check((a: List[Int], b: List[String]) => myzip(a, b).dropRight(1) == (a ::: b).dropRight(1))

  "myzip" should "intertwine two sequences" in {
    val s1 = Seq(1, 2, 3)
    val s2 = Seq('a', 'a', 'a')
    val expected = Seq(1, 'a', 2, 'a', 3, 'a')

    myzip(s1, s2) shouldBe expected
  }

  "myzip" should "intertwine two sequences of different size, first is shorter" in {
    val s1 = Seq(1, 2)
    val s2 = Seq('a', 'a', 'a')
    val expected = Seq(1, 'a', 2, 'a', 'a')

    myzip(s1, s2) shouldBe expected
  }
  "myzip" should "intertwine two sequences of different size" in {
    val s1 = Seq(1, 2, 3)
    val s2 = Seq('a', 'a')
    val expected = Seq(1, 'a', 2, 'a', 3)

    myzip(s1, s2) shouldBe expected
  }

}

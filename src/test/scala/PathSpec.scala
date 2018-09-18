import org.scalatest._

class PathSpec extends FlatSpec with Matchers {

  "A Path" should "correctly create a new line with the midpoint added" in {
    val line = List(Point(10,100), Point(20,200))
    val expectedResult = List(line.head, Point(15,150), line(1))
    val augmentedLine = Path.enhanceWithMidpoint(line)

    augmentedLine shouldEqual expectedResult
  }

  it should "do it iteratively" in {
    val points = List(Point(10,100), Point(20,200))
    val augmentedPoints = Path.increasePointsOnLine(pointsToProcess = points, iterations = 2)

    val expectedResult = List(Point(10,100), Point(12,125), Point(15,150), Point(17, 175), Point(20,200))
    augmentedPoints shouldEqual expectedResult
  }

  it should "have the correct growth rate" in {
    val points = List(Point(10,100), Point(20,200))
    Path.increasePointsOnLine(pointsToProcess = points, iterations = 1).size shouldBe 3
    Path.increasePointsOnLine(pointsToProcess = points, iterations = 2).size shouldBe 5
    Path.increasePointsOnLine(pointsToProcess = points, iterations = 3).size shouldBe 9
  }

  it should "add more points to a path to increase fidelity" in {
    val points = List(Point(10,100), Point(20,200), Point(30, 300))
    val augmentedPoints = Path.lineMaker(pointsToProcess = points, List.empty[List[Point]])

    val expectedResult = List(points.head, Point(15,150), points(1), Point(25, 250), points.last)
    augmentedPoints shouldEqual expectedResult
  }

  it should "add more points to a path, case 2" in {
    val points = List(Point(30,300), Point(20,200), Point(10, 100))
    val augmentedPoints = Path.lineMaker(pointsToProcess = points, List.empty[List[Point]])

    val expectedResult = List(points.head, Point(25,250), points(1), Point(15, 150), points.last)
    augmentedPoints shouldEqual expectedResult
  }
}
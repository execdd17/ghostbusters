package codinggame

import org.scalatest._

class BoardPartitionerTest
  extends FlatSpec
  with Matchers
  with BeforeAndAfterEach {

  var squarePartitioner:BoardPartitioner = _
  var rectangularPartitioner:BoardPartitioner = _

  override def beforeEach(): Unit = {

    // 100 pixel square board
    squarePartitioner = new BoardPartitioner(10,10)

    // 200 pixel rectangular board
    rectangularPartitioner = new BoardPartitioner(20,10)
  }

  "A BoardPartitioner" should "split the board into the correct number of pieces" in {
    squarePartitioner.partition(2).size shouldBe 2
    rectangularPartitioner.partition(2).size shouldBe 2
  }

  it should "create the correct bounding boxes" in {
    val boundingBoxes = squarePartitioner.partition(2)
    boundingBoxes.head.topLeftPoint shouldEqual Point(0,0)
    boundingBoxes.head.bottomLeftPoint shouldEqual Point(0,10)
    boundingBoxes.head.topRightPoint shouldEqual Point(5,0)
    boundingBoxes.head.bottomRightPoint shouldEqual Point(5,10)

    boundingBoxes(1).topLeftPoint shouldEqual Point(5,0)
    boundingBoxes(1).bottomLeftPoint shouldEqual Point(5,10)
    boundingBoxes(1).topRightPoint shouldEqual Point(10,0)
    boundingBoxes(1).bottomRightPoint shouldEqual Point(10,10)
  }
}
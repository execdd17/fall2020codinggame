import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.wordspec.AnyWordSpec

class GameTreeTest extends AnyWordSpec with Matchers with BeforeAndAfterEach {

  var spellBook: Map[Int, Action] = _
  var allOrders: Map[Int, Action] = _

  override def beforeEach(): Unit = {
    spellBook = Map(
      0 -> Action(0, "CAST", 2,-1, 0, 0, 0, -1, 0, castable = true, repeatable = false),
      1 -> Action(1, "CAST",-1, 2, 0, 0, 0, -1, 0, castable = true, repeatable = false),
      2 -> Action(2, "CAST", 0, 0, 2, 0, 0, -1, 0, castable = true, repeatable = false),
      3 -> Action(3, "CAST", 0, 0,-1, 3, 0, -1, 0, castable = true, repeatable = false)
    )

    allOrders = Map(
      4 -> Action(4, "BREW",  -2, -2,  0,  0, 7,  -1, 0, castable = false, repeatable = false),
      5 -> Action(5, "BREW",   0, -2, -2, -2, 12, -1, 0, castable = false, repeatable = false),
      6 -> Action(6, "BREW",  -2,  0, -2,  0, 13, -1, 0, castable = false, repeatable = false),
      7 -> Action(7, "BREW",  -1, -3, -1, -2, 14, -1, 0, castable = false, repeatable = false),
    )
  }

  "return the best path when ingredients for an order already exists" in {
    val inventory = new Inventory(2,2,0,0)
    val orders = List(allOrders(4))
    val spells = List()

    val result = GameTree.bestPath(inventory, spells, orders, maxDepth = 0)
    result.totalScore shouldBe allOrders(4).price
    result.reversePath shouldEqual(List("BREW 4"))
  }

  "return the best path (highest price) for two orders that already exists" in {
    val inventory = new Inventory(2,2,2,2)
    val orders = List(allOrders(4), allOrders(5))
    val spells = List()

    val result = GameTree.bestPath(inventory, spells, orders, maxDepth = 0)
    result.totalScore shouldBe allOrders(5).price
    result.reversePath shouldEqual(List("BREW 5"))
  }

  "return the best path when diving depth 2" in {
    val inventory = new Inventory(2,0,0,0)
    val orders = List(allOrders(6))
    val spells = List(spellBook(2))

    val result = GameTree.bestPath(inventory, spells, orders, maxDepth = 1)
    result.totalScore shouldBe allOrders(6).price - 1 // - 1 because of recursion penalty
    result.reversePath.reverse shouldEqual(List("CAST 2", "BREW 6"))
  }

  "return the best path when diving depth 2 and extra noise" in {
    val inventory = new Inventory(2,0,0,0)
    val orders = List(allOrders(6))
    val spells = List(spellBook(1), spellBook(2)) // Spell 1 can not help the witch

    val result = GameTree.bestPath(inventory, spells, orders, maxDepth = 1)
    result.totalScore shouldBe allOrders(6).price - 1 // - 1 because of recursion penalty
    result.reversePath.reverse shouldEqual(List("CAST 2", "BREW 6"))
  }
}
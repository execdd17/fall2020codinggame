import math._
import scala.util._
import scala.io.StdIn._
import scala.util.Random

case class Plan(moves: List[String], value: Int)

trait Planner {
    def getPlan(orders: List[Action], inventory: Inventory, spellBook: SpellBook): Plan

    def calculateValue(moneyToGain: Int, moneyToDeny: Int, numberOfMoves: Int): Int = {
        (moneyToGain + Math.abs(moneyToDeny)) / numberOfMoves
    }
}

class EagerPlanner extends Planner {

    def getPlan(orders: List[Action], inventory: Inventory, spellBook: SpellBook): Plan = {
        val maybeOrder  = orders.find(order => inventory.canSupportOrder(order))
        
        if (maybeOrder.nonEmpty) {
            return Plan(
                moves=List(s"BREW ${maybeOrder.get.actionId}"), 
                value=calculateValue(maybeOrder.get.price, 0, 1)
            )
        } else if (spellBook.getRandomLegalSpell(inventory).nonEmpty) {
            val spell = spellBook.getRandomLegalSpell(inventory).get
            return Plan(
                moves=List(s"CAST ${spell.actionId}"), 
                value=calculateValue(0, 0, 1)
            )
        } else {
            return Plan(
                moves=List(s"REST"), 
                value=calculateValue(0, 0, 1)
            )
        }
    }
}

class SpellBook(val casts: List[Action]) { 
    val random = new Random

    def getRandomLegalSpell(inventory: Inventory): Option[Action] = {
        val spells = getLegalSpells(inventory) 

        if (spells.nonEmpty)
           Some(getRandomElement(spells, random))
        else 
            None
    }

    override def toString() = s"$casts"

    private def getLegalSpells(inventory: Inventory): List[Action] = {
        casts.filter { cast =>
            cast.castable && inventory.canSupportOrder(cast)
        }
    }

    private def getRandomElement[A](seq: Seq[A], random: Random): A = seq(random.nextInt(seq.length))
}

class Inventory(var blue: Int, var orange: Int, var green: Int, var mustard: Int) {
    val MAX_SIZE = 10

    def resolveOrder(order: Action): Inventory = {
        blue -= order.delta0
        orange -= order.delta1
        green -= order.delta2
        mustard -= order.delta3

        this
    }

    // this is addition because the deltas are negative
    def canSupportOrder(order: Action): Boolean = {
        val newSize = blue + order.delta0 + orange + order.delta1 + 
            green + order.delta2 + mustard + order.delta3

        blue + order.delta0 >= 0 &&
        orange + order.delta1 >= 0 &&
        green + order.delta2 >= 0 &&
        mustard + order.delta3 >= 0 &&
        newSize <= MAX_SIZE
    }

    def totalSize: Int = blue + orange + green + mustard

    override def toString() = s"blue:$blue orange:$orange green:$green mustard:$mustard"
}

case class Action(
    actionId: Int,
    actionType: String,
    delta0: Int,
    delta1: Int,
    delta2: Int,
    delta3: Int,
    price: Int,
    tomeIndex: Int,
    taxCount: Int,
    castable: Boolean,
    repeatable: Boolean
)

class Witch(val inventory: Inventory, val spellBook: SpellBook) {
    val planner = new EagerPlanner

    def evaluateRound(orders: List[Action]): String = {
        val plan = planner.getPlan(orders, inventory, spellBook)
        plan.moves.head
    }

    override def toString: String = s"Inventory: $inventory SpellBook: $spellBook"
}


/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {

    // game loop
    while(true) {
        val actionCount = readLine.toInt // the number of spells and recipes in play

        val actions = 0.until(actionCount).foldLeft(List.empty[Action]) { (memo, i) =>
            val Array(_actionId, actionType, _delta0, _delta1, _delta2, _delta3, _price, _tomeIndex, 
                _taxCount, _castable, _repeatable) = readLine split " "

            val action = Action(_actionId.toInt, actionType, _delta0.toInt, _delta1.toInt, _delta2.toInt, _delta3.toInt, _price.toInt,
                _tomeIndex.toInt, _taxCount.toInt, _castable.toInt != 0, _repeatable.toInt != 0)

            action :: memo
        }

        // for(i <- 0 until actionCount) {
        //     // actionId: the unique ID of this spell or recipe
        //     // actionType: in the first league: BREW; later: CAST, OPPONENT_CAST, LEARN, BREW
        //     // delta0: tier-0 ingredient change
        //     // delta1: tier-1 ingredient change
        //     // delta2: tier-2 ingredient change
        //     // delta3: tier-3 ingredient change
        //     // price: the price in rupees if this is a potion
        //     // tomeIndex: in the first two leagues: always 0; later: the index in the tome if this is a tome spell, equal to the read-ahead tax
        //     // taxCount: in the first two leagues: always 0; later: the amount of taxed tier-0 ingredients you gain from learning this spell
        //     // castable: in the first league: always 0; later: 1 if this is a castable player spell
        //     // repeatable: for the first two leagues: always 0; later: 1 if this is a repeatable player spell
        //     val Array(_actionId, actionType, _delta0, _delta1, _delta2, _delta3, _price, _tomeIndex, _taxCount, _castable, _repeatable) = readLine split " "
        //     val actionId = _actionId.toInt
        //     val delta0 = _delta0.toInt
        //     val delta1 = _delta1.toInt
        //     val delta2 = _delta2.toInt
        //     val delta3 = _delta3.toInt
        //     val price = _price.toInt
        //     val tomeIndex = _tomeIndex.toInt
        //     val taxCount = _taxCount.toInt
        //     val castable = _castable.toInt != 0
        //     val repeatable = _repeatable.toInt != 0
        // }
            
        // inv0: tier-0 ingredients in inventory
        // score: amount of rupees
        val Array(inv0, inv1, inv2, inv3, score) = (readLine split " ").map (_.toInt)
        // Console.err.println(s"MY INVENTORY Blue:$inv0 Orange:$inv1 Green:$inv2 Yellow:$inv3")

        val Array(inv00, inv11, inv22, inv33, score0) = (readLine split " ").map (_.toInt)
        // Console.err.println(s"THEIR INVENTORY Blue:$inv00 Orange:$inv11 Green:$inv22 Yellow:$inv33")
        
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        val myInventory = new Inventory(inv0, inv1, inv2, inv3)
        val me = new Witch(myInventory, spellBook=new SpellBook(actions.filter(_.actionType == "CAST")))
        Console.err.println(me.toString())
        val move = me.evaluateRound(actions.filter(_.actionType == "BREW"))

        // in the first league: BREW <id> | WAIT; later: BREW <id> | CAST <id> [<times>] | LEARN <id> | REST | WAIT
        println(move)
    }
}
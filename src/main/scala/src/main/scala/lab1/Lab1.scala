package com.pin.ailabs
package src.main.scala.lab1

/** Perform knights and their armor-bearers using A* alhorithm
 *  Search
 *
 *
 *
 *
 *    set of knights (X) and armor-bearers (x) as Set of Char
 *    -
 *
 *
 */

object Lab1 extends App {
  def printSets(set: Set[Set[Char]]): Unit =
    set.foreach(x => println(x.toList.sorted))
  def printSets(set: Iterator[Set[Char]]): Unit =
    set.foreach(x => println(x.toList.sorted))

  val knights = Set('A', 'B', 'C', 'D')
  val all = knights ++ knights.map(_.toLower)
  val leftBank = all - 'A' - 'a'
  val rightBank = Set() + 'A' + 'a'

  println("X is knight, x is armor-bearer")
  println("Initial left bank:  " + leftBank.toList.sorted)
  println("Initial right bank: " + rightBank.toList.sorted)

  //prepare the set of valid sets
  val valid = all.subsets.filter(set => set.forall(one =>
    one.isUpper //is knight
    //or is bearer and no other knights
    | (one.isLower & (set.contains(one.toUpper) | set.forall(x => x.isLower | x == one.toUpper)))
  )).toSet

//  println(valid.size)
//  printSets(valid)
//  println(valid.size)

def move(leftToRight: Boolean, fromBank: Set[Char], toBank: Set[Char], way: List[Set[Char]], ttl: Int): List[Set[Char]] = {
  val boatSet = (fromBank.subsets(1) ++ fromBank.subsets(2) ++ fromBank.subsets(3))
    .filter(x => (
      valid.contains(x) //boat must be valid too
      & valid.contains(rightBank ++ x) & valid.contains(fromBank -- x) //next banks must be valid
    )).toSet

  val finalBoat = boatSet.filter(x => (x ++ rightBank) == all)
  if (finalBoat.nonEmpty) {
    println("Complete!")
    way ++ finalBoat
  } else if (ttl == 0) {
    throw new InterruptedException
  } else {
    val z: List[Set[Char]] = List()
    val result: List[Set[Char]] = boatSet
//      .toList.sortWith(_.size > _.size)
      .fold(z) { (wayList: List[Set[Char]], boat: Set[Char]) => {
        val bset: Set[Char] = boat.toSet
        try {
          wayList ++ move(false, fromBank -- bset, toBank ++ bset, way ++ bset, ttl - 1)
        } catch {
          case e: InterruptedException => return wayList
        }
      }}
  }
}

//  println(boat.size)
//  printSets(boat)

  //    .map(_ ++ rightBank)
//    .filter(valid.contains(_)).toSet //valid right bank sets

//  nextRightBank.foreach(_ match {
//    case x if x.size == all.size =>
//  })


  move(true, leftBank, rightBank, List(), 10)

}

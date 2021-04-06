package com.pin.ailabs
package src.main.scala.lab1

import scala.collection.mutable

object KnightsAndBearersNode {
  // key is a set of char where every char is knight (upper case) or armor-bearer (lower case)
  val startKey: Set[Char] = "ABCDabcd".toSet
  val openSet: mutable.Set[KnightsAndBearersNode] = mutable.Set[KnightsAndBearersNode]()

  val validKeys: Set[Set[Char]] = startKey.subsets.filter(
    key => key.forall(
      one =>
        one.isUpper //is knight
          //or is bearer and no other knights
          | (one.isLower & (key.contains(one.toUpper) | key.forall(x => x.isLower | x == one.toUpper)))
  )).toSet

  def findNode(key: Set[Char], boatOnLeft1: Boolean): Option[KnightsAndBearersNode] = {
    openSet.find( node => {
      (node.key == key) & (node.boatOnLeft == boatOnLeft1)
    })
  }

}

/**State of left-side bank from knights-bearers puzzle
 *
 * @param key unique key describing particular state
 * @param gScore the shortest path length from start to this node. Initially is infinite
 * @param boatOnLeft is true if for this state boat is on the left river bank
 */
class KnightsAndBearersNode(val key : Set[Char], gScore: Long, val boatOnLeft: Boolean)
  extends Node(gScore, null) {

  /** @return true if node is final (nobody on left-side bank)  */
  def isFinal: Boolean = key.isEmpty

  /**
   * @return tuple of ( set of visited nodes, set of new nodes)
   */
  def getNeighbourNodes: (mutable.HashSet[Node], mutable.HashSet[Node]) = {
    val visitedAndNewNodes: (mutable.HashSet[Node], mutable.HashSet[Node]) =
      ( mutable.HashSet[Node](), mutable.HashSet[Node]() )

    val differKey = KnightsAndBearersNode.startKey -- key
    val fromKey = if (boatOnLeft) key else differKey
    val toKey = if (boatOnLeft) differKey else key

    val boatSet = (fromKey.subsets(1) ++ fromKey.subsets(2) ++ fromKey.subsets(3))
        .filter(k => (
          KnightsAndBearersNode.validKeys.contains(k) //boat must be valid too
          & KnightsAndBearersNode.validKeys.contains(toKey ++ k)
          & KnightsAndBearersNode.validKeys.contains(fromKey -- k) //next banks must be valid
        )).toSet

    boatSet.foreach(k =>
      KnightsAndBearersNode.findNode(k, boatOnLeft) match {
        case node => visitedAndNewNodes._1 += node
        case _ => visitedAndNewNodes._2 += new KnightsAndBearersNode(k, gScore + 1, !boatOnLeft)
      }
    )

    visitedAndNewNodes

  }

  /** @return heuristic (minimally possible) length of path from this node to the finalNode */
  def hScore: Long = {
    if (key.isEmpty)
      0
    else if (boatOnLeft)
      ( (key.size - 2) / 2 ) * 2 + 1
    else
      ( (key.size - 1) / 2 ) * 2 + 2
  }

}

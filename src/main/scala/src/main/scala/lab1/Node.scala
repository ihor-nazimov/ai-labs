package com.pin.ailabs
package src.main.scala.lab1

import com.pin.ailabs.src.main.scala.lab1.Node.BOAT_ON_LEFT

import scala.collection.mutable

object Node {
  // key is a set of char where every char is knight (upper case) or armor-bearer (lower case)
  val startKey: Set[Char] = "ABCDabcd".toSet

  //boatPosition constants
  val BOAT_ON_LEFT = true
  val BOAT_ON_RIGHT = false

  val validKeys: Set[Set[Char]] = startKey.subsets.filter(
    key => key.forall(
      one =>
        one.isUpper //is knight
          //or is bearer and no other knights
          | (one.isLower & (key.contains(one.toUpper) | key.forall(x => x.isLower | x == one.toUpper)))
  )).toSet

  def findNode(openSet: mutable.HashSet[Node], key: Set[Char], boatPosition: Boolean): Option[Node] = {
    openSet.find( node => {
      (node.key == key) & (node.boatPosition == boatPosition)
    })
  }

}

/**State of left-side bank from knights-bearers puzzle
 *
 * @param key unique key describing particular state
 * @param gScore the shortest path length from start to this node. Initially is infinite
 * @param boatPosition is true if for this state boat is on the left river bank
 */
class Node(val key : Set[Char], var gScore: Long, val boatPosition: Boolean) {
  /** preceding node in optimal path  */
  var cameFrom: Option[Node] = _

  /** @return true if node is final (nobody on left-side bank)  */
  def isFinal: Boolean = key.isEmpty

  /**
   * @return tuple of ( set of visited nodes, set of new nodes)
   */
  def getNeighbourNodes(openSet: mutable.HashSet[Node]) : (mutable.HashSet[Node], mutable.HashSet[Node]) = {
    val visitedAndNewNodes: (mutable.HashSet[Node], mutable.HashSet[Node]) =
      ( mutable.HashSet[Node](), mutable.HashSet[Node]() )

    val differKey = Node.startKey -- key
    val fromKey = if (boatPosition == BOAT_ON_LEFT) key else differKey
    val toKey = if (boatPosition == BOAT_ON_LEFT) differKey else key

    val boatSet = (fromKey.subsets(1) ++ fromKey.subsets(2) ++ fromKey.subsets(3))
        .filter(k => (
          Node.validKeys.contains(k) //boat must be valid too
          & Node.validKeys.contains(toKey ++ k)
          & Node.validKeys.contains(fromKey -- k) //next banks must be valid
        )).toSet

    boatSet.foreach(k =>
      Node.findNode(openSet, k, boatPosition) match {
        case None => visitedAndNewNodes._2 += new Node(k, gScore + 1, !boatPosition) //distance = 1
        case node => visitedAndNewNodes._1 += node.get
      }
    )

    visitedAndNewNodes

  }

  /** @return heuristic (minimally possible) length of path from this node to the finalNode */
  def hScore: Long = {
    if (key.isEmpty)
      0
    else if (boatPosition == BOAT_ON_LEFT)
      ( (key.size - 2) / 2 ) * 2 + 1
    else
      ( (key.size - 1) / 2 ) * 2 + 2
  }

  /** update scores with preceding node scores
   *
   * @param precedingNode node updated before
   * @return true if scores has been updated
   */
  def updateScores(precedingNode: Node): Boolean = {
    val tentativeGScore = precedingNode.gScore + 1 //1 is distance to neighbour
    if (tentativeGScore < gScore) {
      cameFrom = Option[Node](precedingNode)
      gScore = tentativeGScore
      true
    } else false
  }

  /** heuristic length of start-to-end path by this node */
  def fScore: Long = {
    hScore + gScore
  }

  @Override
  def toString: String {
    key.toList.sorted.mkString
  }
}

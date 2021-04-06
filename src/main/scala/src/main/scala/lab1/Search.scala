package com.pin.ailabs
package src.main.scala.lab1

//import java.util.Optional
import scala.collection.mutable
import scala.language.postfixOps
import scala.util.control.Breaks.break

/** A search node abstract class
 *
 * @param gScore the shortest path length from start to this node. By default is infinite
 */
abstract class Node(var gScore: Long = Long.MaxValue, val finalNode: Node) {
  /** preceding node in optimal path  */
  var cameFrom: Option[Node] = _

  /** @return true if node is final */
  def isFinal: Boolean

  /**
   * @return tuple of ( set of visited nodes, set of new nodes)
   */
  def getNeighbourNodes: (mutable.HashSet[Node], mutable.HashSet[Node])

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

    /** heuristic length of path from this node to the finalNode */
  def hScore: Long

  /** heuristic length of start-to-end path by this node */
  def fScore: Long = {
    hScore + gScore
  }
}

/** Search optimal path in node set
 */
class AStarPathFinder {
//  private var openSet: Set[Node]
  /**
   * @param start the start node
   */
  def findPath(start: Node): Option[List[Node]] = {
    val openSet = mutable.HashSet[Node](start)
    var current: Node = start

    while (openSet.nonEmpty) {
      //set current to best path node
      current = openSet.reduceLeft( (a, b) => {
        if ( a.fScore < b.fScore ) a else b
      })
      //return path if final node has found
      if (current.isFinal) {
        reconstructPath(current, List[Node]())
        break
      }
      openSet -= current

      val neighbours = current.getNeighbourNodes
      openSet += neighbours._2 //append new nodes

      //for already visited neighbours
      neighbours._1.foreach( neighbour => {
        neighbour.updateScores(current)
      })
      //for new neighbours
      neighbours._2.foreach( neighbour => {
        if (neighbour.updateScores(current)) openSet += neighbour
      })
    }
    _ //return Nil if no path
  }

  def reconstructPath(to: Node, path: List[Node]): List[Node] = to.cameFrom match {
    case from => reconstructPath(from.get, path :+ to)
    case _ => path
  }
}


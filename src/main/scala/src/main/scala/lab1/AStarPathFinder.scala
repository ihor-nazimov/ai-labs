package com.pin.ailabs
package src.main.scala.lab1

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.postfixOps

/** Search optimal path in node set
 */
object AStarPathFinder {
  /**
   * @param start the start node
   */
  def findPath(start: Node): Option[List[Node]] = {
    val openSet = mutable.HashSet[Node](start)
    var current: Node = start

    while (openSet.nonEmpty) {
      //set current to best path node
      current = openSet.reduceLeft( (a, b) => {
        if ( (a.fScore < b.fScore) & (b.boatPosition != current.boatPosition) ) a else b
      })
      //return path if final node has found
      if (current.isFinal) {
        return Option[List[Node]] (reconstructPath(current, List[Node]()))
      }
      openSet -= current

      val neighbours = current.getNeighbourNodes(openSet: mutable.HashSet[Node])
//      openSet ++= neighbours._2 //append new nodes

      //for already visited neighbours
      neighbours._1.foreach( neighbour => {
        neighbour.updateScores(current)
      })
      //for new neighbours
      neighbours._2.foreach( neighbour => {
        if (neighbour.updateScores(current)) openSet += neighbour
      })
    }

    None //return Nil if no path
  }

  @tailrec
  def reconstructPath(to: Node, path: List[Node]): List[Node] = to.cameFrom match {
    case Some(from) => reconstructPath(from, to :: path )
    case _ => path
  }
}


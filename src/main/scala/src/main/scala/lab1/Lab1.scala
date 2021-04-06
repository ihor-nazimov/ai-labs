package com.pin.ailabs
package src.main.scala.lab1

/** Perform knights and their armor-bearers using A* algorithm
 *
 */

object Lab1 extends App {
  val path = AStarPathFinder.findPath( new Node( Node.startKey, 0, Node.BOAT_ON_LEFT ) )

}

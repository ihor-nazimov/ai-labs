package com.pin.ailabs
package src.main.scala.lab1

/** Perform knights and their armor-bearers using A* algorithm
 *
 */

object Lab1 extends App {
  def keyString(key: Set[Char]): String = key.toList.sorted.mkString

  val path = AStarPathFinder.findPath( new Node( Node.startKey, 0, Node.BOAT_ON_LEFT ) )

  println("\nPath:\n" + keyString(Node.startKey))
  path match {
    case Some(path) => path.foreach( node => {
      if (node.boatPosition == Node.BOAT_ON_RIGHT) {
        val boat = keyString(node.cameFrom.get.key -- node.key)
        val leftBank = keyString(node.key)
        val rightBank = keyString(Node.startKey -- node.key -- boat)
        println(s"$leftBank -> $boat -> $rightBank")
        println(s"$leftBank        ${keyString(Node.startKey -- node.key)}")
      } else {
        val boat = keyString(node.key -- node.cameFrom.get.key)
        val leftBank = keyString(node.key -- boat)
        val rightBank = keyString(Node.startKey -- node.key -- boat )
        println(s"$leftBank <- $boat <- $rightBank")
        println(s"${keyString(node.key)}        $rightBank")
      }
    })
    case _ => println("Path doesn't exists")
  }
}


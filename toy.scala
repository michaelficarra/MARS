import scala.actors.Actor
import scala.actors.Actor._
import java.util.ArrayList
import java.util.Random


implicit def toInteger(i : Int) = new Integer(i)

class Integer(int : Int) extends Proxy {
	def self = int
	def times(block : Int => Unit) = {
		var i = 0
		while (i < self) {
			block(i)
			i += 1
		}
	}
}


val caller = self
val prng   = new Random()
var nodes  = Array[Actor]()

// make 5 nodes
5 times ((whichTime) => {
	nodes = nodes ++ Array(
		actor { loop { receive {
			case block : (Any => Unit) =>
				block(self)
			case "stop" =>
				println("node received stop message")
				caller ! "done"
		}}}
	)
})


// define a behaviour for these nodes (this one simply keeps passing messages
//  to random nodes until it gets back to the originator)
def nodeBehaviour0 (actor : Actor) : Unit = {
	var rand = prng.nextInt(nodes.length)
	println("sending message to " + rand.toString())
	if(rand == 0)
		nodes(0) ! "stop"
	else
		nodes(rand) ! nodeBehaviour0 _
}


// get it started
println("starting")
nodes(0) ! nodeBehaviour0 _


// wait for a node to tell us that the process is finished
receive { case "done" => println("received done message") }

import scala.actors.Actor
import scala.actors.Actor._
import java.util.ArrayList
import java.util.Random


implicit def toInt(i : Int) = new Integer(i)

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


// make some nodes
10 times ((whichTime) => {
	var clock = 0
	nodes = nodes ++ Array(
		actor { loop { receive {
			case (from : Actor, block : NodeBehaviour, timestamp : Int) =>
				val id = nodes.indexOf(self)
				println("#" + id + " @ " + clock + ": received code to execute from node #" + (nodes.indexOf(from)))
				// do some lamport clock calculations here
				block(from, clock)
			case _ =>
				println("error, received bad message")
				exit(1.asInstanceOf[AnyRef])
		}}}
	)
})


// behaviours are just function wrappers so we can pattern match on them
class NodeBehaviour (block : ((Actor, Int) => Unit)) {
	def apply(from : Actor, clock : Int) : Unit = block(from, clock)
	override def toString() : String = block.toString()
}

// define a behaviour for these nodes (these ones simply keep passing messages
//  to random nodes until it gets back to the originator)

val nodeBehaviour0 = new NodeBehaviour(b0 _)

def b0(from : Actor, clock : Int) : Unit = {
	var rand = prng.nextInt(nodes.length)
	val id = nodes.indexOf(self)
	print("#" + id + ": performing computation 0; ")
	if(rand == 0) {
		println("telling main thread that computation is done")
		caller ! ((self, 'done, clock))
	} else if(rand % 4 == 0) {
		println("telling #" + rand + " to perform computation 1")
		nodes(rand) ! ((self, nodeBehaviour1, clock))
	} else {
		println("telling #" + rand + " to perform computation 0")
		nodes(rand) ! ((self, nodeBehaviour0, clock))
	}
}

val nodeBehaviour1 = new NodeBehaviour(b1 _)

def b1(from : Actor, clock : Int) : Unit = {
	var rand = prng.nextInt(nodes.length)
	val id = nodes.indexOf(self)
	print("#" + id + ": performing computation 1; ")
	if(rand == 0) {
		println("telling main thread that computation is done")
		caller ! ((self, 'done, clock))
	} else if(rand % 4 == 0) {
		println("telling #" + rand + " to perform computation 0")
		nodes(rand) ! ((self, nodeBehaviour0, clock))
	} else {
		println("telling #" + rand + " to perform computation 1")
		nodes(rand) ! ((self, nodeBehaviour1, clock))
	}
}


// get it started
println("starting")
val initialTime = 0
nodes(0) ! (self, nodeBehaviour0, initialTime)


// wait for a node to tell us that the process is finished
receive { case (from : Actor, 'done, timestamp : Int) => println("main thread received done notification") }

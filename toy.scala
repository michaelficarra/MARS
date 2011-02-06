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

// make some nodes
10 times ((whichTime) => {
	nodes = nodes ++ Array(
		actor { loop { receive {
			case (from : Actor, block : NodeBehaviour) =>
				val id = nodes.indexOf(self)
				println("#" + id + ": received code to execute from node #" + (nodes.indexOf(from)))
				block.call(from)
		}}}
	)
})


// behaviours are just function wrappers so we can pattern match on them
class NodeBehaviour (block : (Actor => Unit)) {
	def call(from : Actor) : Unit = {
		block(from)
	}
	override def toString() : String = block.toString()
}

// define a behaviour for these nodes (these ones simply keep passing messages
//  to random nodes until it gets back to the originator)

val nodeBehaviour0 = new NodeBehaviour(b0)

def b0(from : Actor) : Unit = {
	var rand = prng.nextInt(nodes.length)
	val id = nodes.indexOf(self)
	print("#" + id + ": performing computation 0; ")
	if(rand == 0) {
		println("telling main thread that computation is done")
		caller ! 'done
	} else if(rand % 4 == 0) {
		println("telling #" + rand + " to perform computation 1")
		nodes(rand) ! (self, nodeBehaviour1)
	} else {
		println("telling #" + rand + " to perform computation 0")
		nodes(rand) ! (self, nodeBehaviour0)
	}
}

val nodeBehaviour1 = new NodeBehaviour(b1)

def b1(from : Actor) : Unit = {
	var rand = prng.nextInt(nodes.length)
	val id = nodes.indexOf(self)
	print("#" + id + ": performing computation 1; ")
	if(rand == 0) {
		println("telling main thread that computation is done")
		caller ! 'done
	} else if(rand % 4 == 0) {
		println("telling #" + rand + " to perform computation 0")
		nodes(rand) ! (self, nodeBehaviour0)
	} else {
		println("telling #" + rand + " to perform computation 1")
		nodes(rand) ! (self, nodeBehaviour1)
	}
}


// get it started
println("starting")
nodes(0) ! (self, nodeBehaviour0)


// wait for a node to tell us that the process is finished
receive { case 'done => println("main thread received done notification") }

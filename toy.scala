import scala.actors.Actor
import scala.actors.Actor._
import java.util.ArrayList
import java.util.Random


val caller = self
val prng   = new Random()
var nodes  = Seq[Actor]()


// make some nodes
0 to 9 foreach ((whichTime) => {
	var clock = 0
	val id = whichTime
	def log(msg : String) = println( id + " @ " + clock + ": " + msg )
	nodes = nodes ++ Array(
		actor { loop { receive {
			case (from : Actor, block : NodeBehaviour, timestamp : Int) =>
				// do some lamport clock calculations here
				clock = clock max (timestamp + 1)
				log("received code to execute from node #" + (nodes.indexOf(from)))
				var times = new Array[Int](nodes.length)
				times update (id, clock)
				nodes filter { _ != self } foreach ((node) => {
					node ! (self, 'clock)
				})
				nodes filter { _ != self } foreach ((node) => {
					receive {
						case (from : Actor, timestamp : Int) =>
							times update (nodes indexOf from, timestamp)
					}
				})
				println(times toSeq)
				block(from, clock)
			case (from : Actor, 'clock) =>
				from ! (self, clock)
			case _ =>
				log("error: received bad message")
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

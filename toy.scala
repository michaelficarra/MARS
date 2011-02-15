import scala.actors.Actor
import scala.actors.Actor._
import java.util.ArrayList
import java.util.Random


val numberOfNodes = 10


val main  = self
val prng  = new Random()
var nodes = Seq[Actor]()

def nodeName(node : Actor) : String = {
	val id = nodes indexOf node
	if(id < 0) return "main"
	else return "node #" + id
}


// behaviours are just function wrappers so we can pattern match on them
case class NodeBehaviour (block : (Actor, Array[Int]) => Unit) {
	def apply(sender : Actor, clock : Array[Int]) : Unit = block(sender, clock)
}

// define a behaviour for these nodes (these ones simply keep passing messages
//  to random nodes until it gets back to the originator)

def b0(sender : Actor, clock : Array[Int]) : Unit = {
	val rand = prng.nextInt(nodes.length)
	val time = clock(nodes indexOf self)
	print(nodeName(self) + " @" + time + ": performing computation 0; ")
	if(rand == 0) {
		println("telling main thread that computation is done")
		main ! ((self, clock, 'done))
	} else if(rand % 4 == 0) {
		println("telling #" + rand + " to perform computation 1")
		nodes(rand) ! ((self, clock, nodeBehaviour1))
	} else {
		println("telling #" + rand + " to perform computation 0")
		nodes(rand) ! ((self, clock, nodeBehaviour0))
	}
}

val nodeBehaviour0 = new NodeBehaviour(b0 _)

def b1(sender : Actor, clock : Array[Int]) : Unit = {
	val rand = prng.nextInt(nodes.length)
	val time = clock(nodes indexOf self)
	print(nodeName(self) + " @" + time + ": performing computation 1; ")
	if(rand == 0) {
		println("telling main thread that computation is done")
		main ! ((self, clock, 'done))
	} else if(rand % 4 == 0) {
		println("telling #" + rand + " to perform computation 0")
		nodes(rand) ! ((self, clock, nodeBehaviour0))
	} else {
		println("telling #" + rand + " to perform computation 1")
		nodes(rand) ! ((self, clock, nodeBehaviour1))
	}
}
val nodeBehaviour1 = new NodeBehaviour(b1 _)



// make some nodes
0 until numberOfNodes foreach { id =>
	var clock = new Array[Int](numberOfNodes)
	clock padTo (numberOfNodes, 0)
	def log(msg : String) = println( "node #" + id + ": " + msg )
	def updateClock(timestamp : Array[Int]) = {
		clock.indices foreach { idx =>
			clock(idx) = clock(idx) max timestamp(idx)
		}
		incrementClock
	}
	def incrementClock = {
		clock(id) = clock(id) + 1
	}
	nodes = nodes :+ actor { loop { receive {
		case (sender : Actor, timestamp : Array[Int], block : NodeBehaviour) =>
			log("received code to execute from " + nodeName(sender))
			log("clock before: " + clock.toList.toString)
			updateClock(timestamp)
			log(" clock after: " + clock.toList.toString)
			block(sender, clock)
		case (sender : Actor, 'clock) =>
			sender ! (self, clock)
		case (sender : Actor, 'id) =>
			sender ! (self, id)
		case _ =>
			log("error: received bad message")
			exit(1.asInstanceOf[AnyRef])
	}}}
}


// get it started
println("starting")
val initialTime = new Array[Int](numberOfNodes)
initialTime padTo (numberOfNodes, 0)
nodes(0) ! (self, initialTime, nodeBehaviour0)


// wait for a node to tell us that the process is finished
receive { case (sender : Actor, timestamp : Array[Int], 'done) => println("main thread received done notification") }

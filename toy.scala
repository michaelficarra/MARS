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
	val time = clock(nodes indexOf self)
	var nextBehaviour = nodeBehaviour0
	if(prng.nextInt(5) == 0)
		nextBehaviour = nodeBehaviour1
	println(nodeName(self) + " @" + time + ": performing computation 0")
	for(node <- router)
		node ! ((self, clock, nextBehaviour))
}

val nodeBehaviour0 = new NodeBehaviour(b0 _)

def b1(sender : Actor, clock : Array[Int]) : Unit = {
	val time = clock(nodes indexOf self)
	var nextBehaviour = nodeBehaviour1
	if(prng.nextInt(5) == 0)
		nextBehaviour = nodeBehaviour0
	println(nodeName(self) + " @" + time + ": performing computation 1")
	for(node <- router)
		node ! ((self, clock, nextBehaviour))
}

val nodeBehaviour1 = new NodeBehaviour(b1 _)

// a routing function used by the above behaviours to determine
// the nodes to whom additional messages should be sent

def router : List[Actor] = {
	val rand = prng.nextInt(12)
	if(rand == 0) {
		println("telling main thread that computation is done")
		main ! ((self, 'done))
	} else if(rand > 8) {
		return List(
			nodes(prng.nextInt(nodes.length)),
			nodes(prng.nextInt(nodes.length))
		)
	} else {
		return List(
			nodes(prng.nextInt(nodes.length))
		)
	}
	return List()
}



// make some nodes
0 until numberOfNodes foreach { id =>
	var clock = new Array[Int](numberOfNodes)
	clock padTo (numberOfNodes, 0)
	def log(msg : String) = println( "node #" + id + ": " + msg )
	def updateClock(sender : Actor, timestamp : Array[Int]) = {
		val sid = nodes indexOf sender
		clock.indices foreach { idx =>
			clock(idx) = clock(idx) max timestamp(idx)
		}
		if(sid >= 0) clock(sid) = clock(sid) max timestamp(sid)
		incrementClock
	}
	def incrementClock = {
		clock(id) = clock(id) + 1
	}
	nodes = nodes :+ actor { loop { receive {
		case (sender : Actor, timestamp : Array[Int], block : NodeBehaviour) =>
			log("received code to execute from " + nodeName(sender))
			log("clock before: " + clock.toList.toString)
			updateClock(sender, timestamp)
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
receive { case (sender : Actor, 'done) => println("main thread received done notification") }

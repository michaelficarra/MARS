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
val rand = new Random()
var nodes = Array[Actor]()

3 times (_ =>
	nodes = nodes ++ Array(actor {
		receive {
			case "start" =>
				nodes(rand.nextInt(nodes.length)) ! "message"
			case _ =>
				println("node received message")
				caller ! "done"
		}
	})
)


println("starting")

nodes(0) ! "start"

receive { case "done" => println("received done message") }

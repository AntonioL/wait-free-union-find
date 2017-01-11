import java.util.concurrent.atomic.{AtomicReferenceArray, AtomicInteger}
class WFUnionFind (size : Int)  {

	private class Node (val dummyNext : Int, val rank : Int) {
		private val container = new AtomicInteger(dummyNext)
		def next = container.get
		def casNext(old : Int, newValue : Int) = container.compareAndSet(old, newValue)
	}

	private val array = new AtomicReferenceArray[Node](Array.tabulate(size)(i => new Node(i, 0)))

	private def updateRoot(x : Int, oldRank : Int, y : Int, newRank : Int) : Boolean = {
		val old = array.get(x)
		if(old.next != x || old.rank != oldRank) {
			return false
		}
		val newNode = new Node(y, newRank)
		return array.compareAndSet(x, old, newNode)
	}

	private def find(idx : Int) : Int = {
		var (x, node) = (idx, array.get(idx))
		while(x != node.next) {
			val t = node.next
			val tNode = array.get(t)
			node.casNext(t, tNode.next)
			x = t; node = tNode;
		}
		return x
	}

	def sameSet(x : Int, y : Int) : Boolean = {
		while(true) {
			val (set_x, set_y) = (find(x), find(y))
			if(set_x == set_y) {
				return true
			} else if(array.get(set_x).next == set_x) {
				return false
			}
		}
		sys.error("Never reach here")
	}

	def union(x : Int, y : Int) : Unit = {
		while(true) {
			var set_x = find(x); var set_y = find(y)
			if(set_x == set_y) return
			var set_x_rank = array.get(set_x).rank
			var set_y_rank = array.get(set_y).rank
			if(set_x_rank > set_y_rank || (set_x_rank == set_y_rank && set_x > set_y)) {
				val (t1, t2) = (set_x, set_x_rank)
				set_x = set_y; set_x_rank = set_y_rank
				set_y = t1; set_y_rank = t2
			}
			if(updateRoot(set_x, set_x_rank, set_y, set_x_rank)) {
				if(set_x_rank == set_y_rank) {
					updateRoot(set_y, set_y_rank, set_y, set_y_rank + 1)
				}
				return
			}
		}
	}



}
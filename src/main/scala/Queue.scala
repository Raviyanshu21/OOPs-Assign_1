trait Queue {
  var queue:List[Double] = List.empty
  var front: Int = -1
  var rear: Int = -1

  def enqueue(item: Double): String = {
    if(rear == -1 && front == -1) {
      front = front + 1
      rear = rear + 1
      queue = queue ::: List(item)
      "Item is enqueued."
    }
    else{
      rear = rear + 1
      queue = queue ::: List(item)
      "Item is enqueued.."
    }
  }

  def dequeue: String = {
    if(front == -1 && rear == -1){
      "Underflow.."
    }
    else if(front == rear ){
      queue=queue.drop(1)
      front = -1
      rear = -1
      "Item dequeued.."
    }
    else{
      queue = queue.drop(1)
      front = front + 1
      "Item dequeued.."
    }
  }

  def getQueue: List[Double] = {
    queue
  }
}

class DoubleQueue extends Queue {
  override def enqueue(item: Double): String = {
    if (rear == -1 && front == -1) {
      front = front + 1
      rear = rear + 1
      val doubleOfItem = 2 * item // double of item
      queue = queue ::: List(doubleOfItem)
      "Item is enqueued.."
    }
    else {
      rear = rear + 1
      val doubleOfItem = 2 * item  // double of the item.
      queue = queue ::: List(doubleOfItem)
      "Item is enqueued.."
    }
  }
  print(enqueue(5) + "\n")
  print(enqueue(6) + "\n")
  print(enqueue(7) + "\n")
  print(getQueue + "\n")
  print(dequeue + "\n")
  print(getQueue + "\n")
}

class SquareQueue extends Queue{
  override def enqueue(item: Double): String = {
    if (rear == -1 && front == -1) {
      front = front + 1
      rear = rear + 1
      val squareOfItem = scala.math.pow(item,2) //square of item
      queue = queue ::: List(squareOfItem)
      "Item is enqueued.."
    }
    else {
      rear = rear + 1
      val squareOfItem = scala.math.pow(item,2) //square of item
      queue = queue ::: List(squareOfItem)
      "Item is enqueued.."
    }
  }
  print(enqueue(5) + "\n")
  print(enqueue(6) + "\n")
  print(enqueue(7) + "\n")
  print(getQueue + "\n")
  print(dequeue + "\n")
  print(getQueue + "\n")
}

object Trait extends Queue with App{
  var obj = new SquareQueue()
  var doubleObj = new DoubleQueue()
}


 


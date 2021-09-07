package scalatron.botwar.botPlugin.algvas
import scala.collection.mutable
import scala.util.Random
import scala.collection.JavaConverters._

class ControlFunction {
  def respond(input: String): String = {
    val (opcode, paramMap) = CommandParser(input)
    if( opcode == "React" ) {
      val viewString = paramMap("view")
      val view = View(viewString)
      val generation = paramMap("generation").toInt
      if(generation == 0){
        MasterBot(view, paramMap)
      } else {
        SlaveBot(view, paramMap)
      }
    } else ""
  }
}

trait BotTrait{
  def apply(view: View, params: Map[String, String]): String
}

object MasterBot extends BotTrait {
  def apply(view: View, params: Map[String, String]): String = {
    var response = ""
    view.DFS('P', 'B') match {
      case Some(offset) =>
        response += "Move(direction=" + offset._1 + ")|Status(text=Hunting)"

      case None =>
        view.DFS('_') match {
          case Some(offset) =>
            response += "Move(direction=" + offset._1 + ")|Status(text=No food to be seen)"
          case None =>
            response += "Status(text=Stuck!)"
        }
    }

    view.DFS('S') match {
      case None =>
        if(params("energy").toInt >= 1000){
          val role = Array("harvester", "defender", "killer").apply(Random.nextInt(3))
          response += "|Spawn(direction=" + XY.Right + ",role=" + role + ")"
        }
      case _ =>
    }
    response
  }
}

object SlaveBot extends BotTrait {
  def apply(view: View, params: Map[String, String]) : String = {

    var response = ""
    if(params("role") == "defender"){ //Defender
      view.DFS('b') match {
        case Some(offset) =>

          if(offset._2 <= 3) {
            response += "Status(text=Exploaded)|Explode(size=5)"
          } else {
            response += "Move(direction=" + offset._1 + ")|Status(text=Defending)"
          }
          
        case None =>
          response += "Move(direction=" + params("master") + ")|Status(text=Guard returning)"
      }

    } else if (params("role") == "harvester") { //Harvester
      if(params("energy").toInt < 500)
        view.DFS('P', 'B') match {
          case Some(offset) =>
            response += "Move(direction=" + offset._1 + ")|Status(text=Harvesting)"

          case None =>
            response += "Move(direction=" + params("master") + ")|Status(text=Returning)"
        } else {

        view.DFS('M') match {
          case Some(offset) =>
            response += "Move(direction=" + offset._1 + ")|Status(text=Returning)"
          case None =>
            response += "Move(direction=" + params("master") + ")|Status(text=Returning)"
        }
      }
    } else {
        if(params("energy").toInt < 300)
            view.DFS('m') match {
                case Some(offset) =>
                    response += "Explode(size=4)|Status(text=Attacking)"
                case None =>
                    response += "Move(direction=" + params("master") + ")|Status(text=Returning)"
            }
    }
    response
  }
}

case class View(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = XY(size/2, size/2)
  def DFS(target: Char*) : Option[(XY, Int)] = {

    val routingInfo = mutable.Map(cells
      .grouped(size)
      .map(_.toArray)
      .toArray
      .zipWithIndex
      .flatMap(row => row._1
        .zipWithIndex
        .map(
          col => XY(col._2, row._2) -> (col._1, false, 0, XY(0, 0))
        )
      ).toSeq: _*)

    def traceBack(target: XY): XY = {
      val direction = routingInfo(target)._4
      val next = target + direction
      if(routingInfo(next)._4 == XY.Zero) {
        return direction.negate
      }

      traceBack(next)
    }

    routingInfo(center) = routingInfo(center).copy(_2 = true, _3 = 0)

    val q = mutable.Queue(center)

    def canAccess(position: XY) = !Set('W', '?', 'm', 's', 'p', 'b', 'S', 'M').filter(!target.contains(_)).contains(routingInfo(position)._1)
    while(q.nonEmpty) {
      val curPos = q.dequeue()
      if(target.contains(routingInfo(curPos)._1)){
        return Some((traceBack(curPos), routingInfo(curPos)._3))
      }
      
      val tempDirections = Array(XY.Up, XY.Left, XY.Right, XY.Down, XY.UpLeft, XY.RightUp, XY.LeftDown, XY.DownRight)

      val directions = if (target.head == '_') Random.shuffle(tempDirections.toSeq).toArray else tempDirections
      for (direction <- directions) {
        val nextPos = curPos + direction
        if ((0 until size contains nextPos.x) && (0 until size contains nextPos.y) && canAccess(nextPos) && !routingInfo(nextPos)._2) {
            routingInfo(nextPos) = routingInfo(nextPos).copy(_2 = true, _3 = routingInfo(curPos)._3 + 1, _4 = direction.negate)
            q.enqueue(nextPos)
        }
      }
    }
    None
  }
  
}

case class XY(x: Int, y: Int) {
  override def toString = x + ":" + y
  def isNonZero = x!=0 || y!=0
  def isZero = x==0 && y==0
  def isNonNegative = x>=0 && y>=0
  def updateX(newX: Int) = XY(newX, y)
  def updateY(newY: Int) = XY(x, newY)
  def addToX(dx: Int) = XY(x+dx, y)
  def addToY(dy: Int) = XY(x, y+dy)
  def +(pos: XY) = XY(x+pos.x, y+pos.y)
  def -(pos: XY) = XY(x-pos.x, y-pos.y)
  def *(factor: Double) = XY((x*factor).intValue, (y*factor).intValue)
  def distanceTo(pos: XY) : Double = (this-pos).length
  def length : Double = math.sqrt(x*x + y*y)
  def signum = XY(x.signum, y.signum)
  def negate = XY(-x, -y)
  def negateX = XY(-x, y)
  def negateY = XY(x, -y)
}

object XY {
  def apply(s: String) : XY = {
    val xy = s.split(':').map(_.toInt) // e.g. "-1:1" => Array(-1,1)
    XY(xy(0), xy(1))
  }

  val Zero = XY(0,0)
  val One =  XY(1,1)

  val Right      = XY( 1,  0)
  val RightUp    = XY( 1, -1)
  val Up         = XY( 0, -1)
  val UpLeft     = XY(-1, -1)
  val Left       = XY(-1,  0)
  val LeftDown   = XY(-1,  1)
  val Down       = XY( 0,  1)
  val DownRight  = XY( 1,  1)
}

object CommandParser {
  def apply(command: String) = {
    def splitParam(param: String) = {
      val segments = param.split('=')
      if( segments.length != 2 )
        throw new IllegalStateException("invalid key/value pair: " + param)
      (segments(0),segments(1))
    }

    val segments = command.split('(')
    if( segments.length != 2 )
      throw new IllegalStateException("invalid command: " + command)
    val params = segments(1).dropRight(1).split(',')
    val keyValuePairs = params.map( splitParam ).toMap
    (segments(0), keyValuePairs)
  }
}

class ControlFunctionFactory {
  def create = new ControlFunction().respond _
}


package lib

import scala.collection.mutable._
import java.io._
import org.json.JSONTokener
import org.json.JSONObject
import org.json.JSONArray

object Callgraph {

  class Edge(val callee: String, val args: List[String]) {

  }

  var graph: Map[String, MutableList[Edge]] = Map()

  def add(caller: String, callee: String, args: List[String]) = {
    val edge = new Edge(callee, args)
    if (graph.contains(caller)) {
      graph(caller) += edge
    } else {
      graph.put(caller, MutableList(edge))
    }
  }

  def isCalled(function: String): Boolean = {
    graph.foldLeft(false) {
      (r, l) =>
        r || l._2.foldLeft(false) {
          (a, e) =>
            a || e.callee.equals(function)
        }
    }
  }

  def processExprsForCall(caller: String, o: JSONObject): List[String] = {
    if (!o.keySet().contains("Type")) return Nil
    o.getString("Type") match {
      case "Call" =>
        if (o.getJSONObject("Call").getString("Calltype") == "direct") {
          val callObject = o.getJSONObject("Call")
          val args = callObject.getJSONArray("Args")
          val argsList = ListBuffer[String]()
          for (i <- Range(0, args.length())) {
            argsList += Util.extractArgumentFromJson(args.getJSONObject(i).getJSONObject("Arg"))
          }
          add(caller, callObject.getString("Callee"), argsList.toList)
        }

      case "Expr" =>
        if (o.keySet.contains("Body"))
          processExprsForCall(caller, o.getJSONObject("Body"))
      case "BinaryOperator" =>
        var bo = o.getJSONObject("BinaryOperator")
        processExprsForCall(caller, bo.getJSONObject("Lhs"))
        processExprsForCall(caller, bo.getJSONObject("Rhs"))
      case "UnaryOperator" =>
        processExprsForCall(caller, o.getJSONObject("SubExpr"))
      case "Expr" =>
        processExprsForCall(caller, o.getJSONObject("Body"))
      case "ArraySubscriptExpr" =>
        processExprsForCall(caller, o.getJSONObject("Lhs"))
        processExprsForCall(caller, o.getJSONObject("Rhs"))
      case _ => return Nil
    }
    return Nil
  }

  def printD3JSON() {
    graph.keys.foreach {
      caller =>
        val edges = graph(caller)
        edges.foreach(
          edge => println(s"""{source:\"$caller\", target: \"${edge.callee}\", type:\"licensing\"},"""))
    }
  }

  def processFunction(o: JSONObject) = {
    if (o.keySet().contains("FunctionDecl")) {
      var f = o.getJSONObject("FunctionDecl")
      if (f.keySet().contains("Body") &&
        f.getJSONObject("Body").keySet().contains("Body")) {
        var body = f.getJSONObject("Body")
        Util.recursivelyProcessExprs[String](body, processExprsForCall(f.getString("Name"), _: JSONObject))
      }
    }
  }

  def print(file:String) = {
    println("Writing callgraph to " + file)
    val pf = new File(file)
    if(pf.exists()) pf.delete()
    val f = new FileWriter(file, true)
    f.append("digraph G {\r\n")
    graph.keys.foreach {
      caller =>
        val edges = graph(caller)
        edges.foreach(
          edge => f.append(caller + " -> " + edge.callee + "[label=\"(" + edge.args.mkString(",") + ")\"]\r\n"))
    }
    f.append("}\r\n")
    f.close()
  }
  
  
  def print = {
    println("digraph G {")
    graph.keys.foreach {
      caller =>
        val edges = graph(caller)
        edges.foreach(
          edge => println(caller + " -> " + edge.callee + "[label=\"(" + edge.args.mkString(",") + ")\"]"))

    }
    println("}")
  }

}
package lib

import org.json.JSONTokener
import org.json.JSONObject
import org.json.JSONArray
import scala.collection.mutable.ListBuffer
import scala.Range
import scala.collection.mutable._

object Util {

  def str2jsonObject(s: String): JSONObject = {
    var tokener: JSONTokener = new JSONTokener(s)
    var jsonObject: JSONObject = new JSONObject(tokener)
    jsonObject
  }

  def str2jsonArray(s: String): JSONArray = {
    var tokener: JSONTokener = new JSONTokener(s)
    var jsonArray: JSONArray = new JSONArray(tokener)
    jsonArray
  }

  def isCalleeX(s: String, c: String): Boolean = {
    var obj = str2jsonObject(s)
    obj.get("Callee").equals(c)
  }

  def getNameFromDecl(o: JSONObject): String = {
    if (o.keySet().contains("Decl")) {
      o.get("Decl") match {
        case "VarDecl" =>
          o.getJSONObject("Var").getString("Name")
        case "ParmVar" =>
          o.getJSONObject("ParmVar").getString("Name")
        case "FunctionDecl" =>
          o.getJSONObject("FunctionDecl").getString("Name")
        case _ => o.getString("Decl")
      }
    } else {
      "Unknown"
    }
  }

  def getTypeFromDecl(o: JSONObject): String = {
    if (o.keySet.contains("Decl")) {
      o.get("Decl") match {
        case "VarDecl" =>
          o.getJSONObject("Var").getString("Type")
        case "ParmVar" =>
          o.getJSONObject("ParmVar").getString("Type")
        case _ => assert(false).asInstanceOf[String]
      }
    } else {
      "Unknown"
    }
  }

  def getIntegerFromIntegerLiteral(o: JSONObject): String = {
    o.get("IntegerLiteral").toString()
  }

  def extractArgumentFromJson(o: JSONObject): String = {
    //println(o)
    if (o.keySet().contains("Type")) {
      o.get("Type") match {
        case "DeclRef"                  => getNameFromDecl(o.getJSONObject("DeclRef"))
        case "ArraySubscriptExpr"       => extractArgumentFromJson(o.getJSONObject("Lhs")) + "[" + extractArgumentFromJson(o.getJSONObject("Rhs")) + "]"
        case "IntegerLiteral"           => getIntegerFromIntegerLiteral(o)
        case "StringLiteral"            => o.get("StringLiteral").asInstanceOf[String]
        case "BinaryOperator"           => expr2str(o)
        case "Call"                     => expr2str(o)
        case "ParmVar"                  => getNameFromDecl(o.getJSONObject("ParmVar"))
        case "UnaryExprOrTypeTraitExpr" => expr2str(o)
        case "UnaryOperator"            => o.get("OpCode").asInstanceOf[String] + "(" + extractArgumentFromJson(o.getJSONObject("SubExpr")) + ")"
        case "ParenExpr"                => "(" + extractArgumentFromJson(o.getJSONObject("SubExpr")) + ")"
        case "CStyleCastExpr"           => "(CAST)"
        case "CharacterLiteral"         => o.get("CharacterLiteral").toString()
        case "FloatingLiteral"          => o.get("FloatingLiteral").toString()
        case "ConditionalOperator"      => o.get("RHS").toString() + " == " + o.get("LHS").toString()
        case "MemberExpr"               => extractArgumentFromJson(o.getJSONObject("Base")) + "->" + o.getString("Member") 
      }
    } else {
      return "unknown(1)"
    }
  }

  /*
   * Converts a JSONArray to a String List where each 
   * member is the name of the arg.
   */
  def argsJsonArray2List(a: JSONArray): List[String] = {
    if (a == null) List()
    else {
      var retVal = ListBuffer[String]()
      for (i <- Range(0, a.length())) {
        var arg = Util.extractArgumentFromJson(a.get(i).asInstanceOf[JSONObject].get("Arg").asInstanceOf[JSONObject])
        retVal += arg
      }
      retVal.toList
    }
  }

  def recursivelyProcessDecls[T](o: JSONObject, f: JSONObject => T): List[T] = {
    var result = ListBuffer[T]()

    if (!o.keySet().contains("Type")) {
      return result.toList
    }

    o.getString("Type") match {
      case "CompoundStmt" =>
        var body = o.getJSONArray("Body")
        for (i <- Range(0, body.length())) {
          result ++= recursivelyProcessDecls(body.getJSONObject(i), f)
        }
      case "DeclStmt" =>
        var body = o.getJSONArray("Body")
        for (i <- Range(0, body.length())) {
          result += f(body.getJSONObject(i))
        }
      case "WhileStmt" =>
        var body = o.getJSONObject("Body")
        result ++= recursivelyProcessDecls(body, f)
      case "DoStmt" =>
        var body = o.getJSONObject("Body")
        result ++= recursivelyProcessDecls(body, f)
      case "IfStmt" =>
        var thenStmt = o.getJSONObject("ThenStmt")
        var elseStmt = o.getJSONObject("ElseStmt")
        result ++= recursivelyProcessDecls(thenStmt, f)
        result ++= recursivelyProcessDecls(elseStmt, f)
      case "Expr" | "ReturnStmt" | "NullStmt" | "ContinueStmt"
        | "BreakStmt" | "GotoStmt" | "LabelStmt" => ()
      case "ForStmt" =>
        var body = o.getJSONObject("Body")
        result ++= recursivelyProcessDecls(body, f)
      case "CompoundAssignOperator" => () // TODO
      case "SwitchStmt" =>
        var body = o.getJSONArray("Body")
        for (i <- Range(0, body.length())) {
          result ++= recursivelyProcessDecls(body.getJSONObject(i), f)
        }
      case "CaseStmt" =>
        var body = o.getJSONObject("Body")
        result ++= recursivelyProcessDecls(body, f)
      // ignore lhs?
      case "DefaultStmt" =>
        if (o.keySet.contains("Body")) {
          var body = o.getJSONObject("Body")
          result ++= recursivelyProcessDecls(body, f)
        }
      case _ =>
        println("Unknown statement type encountered(1): " + o.getString("Type"))
        println(o.toString())
        System.exit(-1)
    }

    return result.toList
  }

  def expr2str(o: JSONObject): String = {
    if (!o.keySet().contains("Type")) {
      return ""
    }

    o.getString("Type") match {
      case "DeclRef" =>
        return getNameFromDecl(o.getJSONObject("DeclRef"))
      case "Call" =>
        var callObject = o.getJSONObject("Call")
        var args = callObject.getJSONArray("Args")
        var argsList: ListBuffer[String] = ListBuffer()
        for (i <- Range(0, args.length())) {
          argsList += expr2str(args.getJSONObject(i))
        }
        if (callObject.getString("Calltype").equals("direct")) {
          return callObject.getString("Callee") + "(" + argsList.mkString(",") + ")"
        } else {
          return expr2str(callObject.getJSONObject("Callee")) + "(" + argsList.mkString(",") + ")"
        }
      case "BinaryOperator" =>
        var bo = o.getJSONObject("BinaryOperator")
        var operator = o.getString("Opcode")
        return expr2str(bo.getJSONObject("Lhs")) + operator + expr2str(bo.getJSONObject("Rhs"))
      case "UnaryOperator" =>
        return o.getString("OpCode") + expr2str(o.getJSONObject("SubExpr"))
      case "Expr" =>
        return expr2str(o.getJSONObject("Body"))
      case "ArraySubscriptExpr" =>
        return expr2str(o.getJSONObject("Lhs")) + "[" + expr2str(o.getJSONObject("Rhs")) + "]"
      case "IntegerLiteral" =>
        return o.get("IntegerLiteral").toString()
      case "StringLiteral" =>
        return o.getString("StringLiteral")
      case "UnaryExprOrTypeTraitExp" =>
        if (o.keySet().contains("Argument")) return o.getString("Argument")
        else return "UnaryExprOrTypeTraitExp"
      case "CStyleCastExpr" =>
        return "Cast"
      case _ =>
        // maybe it is an arg
        if (o.keySet().contains("Arg")) {
          return expr2str(o.getJSONObject("Arg"))
        } else {
          println("expr2str : unknown type " + o.getString("Type"))
        }
    }
    return ""

  }

  def getStatementCount(o: JSONObject): Int = {

    if (!o.keySet().contains("Type") && !o.keySet().contains("Body")) {
      return 0
    }

    o.getString("Type") match {
      case "Body" =>
        getStatementCount(o.getJSONObject("Body"))
      case "CompoundStmt" =>
        o.getJSONArray("Body").length()
      case "DeclStmt" =>
        o.getJSONArray("Body").length()
      case "WhileStmt" =>
        var body = o.getJSONObject("Body")
        return getStatementCount(body)
      case "IfStmt" =>
        getStatementCount(o.getJSONObject("ThenStmt")) +
          getStatementCount(o.getJSONObject("ElseStmt"))
      case "Expr"       => 0
      case "ReturnStmt" => 1
      case _ =>
        println("Unknown statement type encountered(2): " + o.getString("Type"))
        println(o.toString())
        System.exit(-1).asInstanceOf[Int]
    }
  }

  def recursivelyProcessExprs[T](o: JSONObject, f: JSONObject => List[T]): List[T] = {
    var result = ListBuffer[T]()

    if (!o.keySet().contains("Type")) {
      return result.toList
    }

    o.getString("Type") match {
      case "CompoundStmt" | "SwitchStmt" =>
        var body = o.getJSONArray("Body")
        for (i <- Range(0, body.length())) {
          result ++= recursivelyProcessExprs(body.getJSONObject(i), f)
        }
      case "CaseStmt" =>
        var body = o.getJSONObject("Body")
        var cond = o.getJSONObject("Lhs")
        result ++= f(cond)
        result ++= recursivelyProcessExprs(body, f)
      case "DeclStmt" =>
        var body = o.getJSONArray("Body")
        for (i <- Range(0, body.length())) {
          var b = body.getJSONObject(i)
          if (b.keySet().contains("Decl")) {
            b.get("Decl") match {
              case "VarDecl" =>
                var varDecl = b.getJSONObject("Var")
                if (varDecl.keySet().contains("Init")) {
                  result ++= f(varDecl.getJSONObject("Init"))
                }
              case "ParmVar" =>
                var varDecl = b.getJSONObject("ParmVar")
                if (varDecl.keySet().contains("Init")) {
                  result ++= f(varDecl.getJSONObject("Init"))
                }
            }
          }
        }
      case "WhileStmt" =>
        var body = o.getJSONObject("Body")
        var cond = o.getJSONObject("Cond")
        result ++= f(cond)
        result ++= recursivelyProcessExprs(body, f)
      case "DoStmt" =>
        var body = o.getJSONObject("Body")
        var cond = o.getJSONObject("Cond")
        result ++= f(cond)
        result ++= recursivelyProcessExprs(body, f)
      case "ForStmt" =>
        var body = o.getJSONObject("Body")
        var cond = o.getJSONObject("Cond")
        var incr = o.getJSONObject("Incr")
        result ++= f(cond)
        result ++= f(incr)
        result ++= recursivelyProcessExprs(body, f)
      case "IfStmt" =>
        var thenStmt = o.getJSONObject("ThenStmt")
        var elseStmt = o.getJSONObject("ElseStmt")
        var cond = o.getJSONObject("Cond")
        result ++= f(cond)
        result ++= recursivelyProcessExprs(thenStmt, f)
        result ++= recursivelyProcessExprs(elseStmt, f)
      case "Expr" =>
        result ++= f(o)
      case "ReturnStmt" =>
        if (o.keySet().contains("ReturnValue")) {
          result ++= f(o.getJSONObject("ReturnValue"))
        }
      case "NullStmt" | "BreakStmt" | "LabelStmt" | "GotoStmt"
        | "ContinueStmt" => ()
      case "CompoundAssignOperator" => () // TODO
      case "Body" =>
        result ++= recursivelyProcessExprs(o.getJSONObject("Body"), f)
      case "DefaultStmt" =>
        if (o.keySet.contains("Body")) {
          var body = o.getJSONObject("Body")
          result ++= recursivelyProcessExprs(body, f)
        }
      case _ =>
        println("Unknown statement type encountered(3): " + o.getString("Type"))
        println(o.toString())
        System.exit(-1)
    }

    return result.toList
  }

  def getLHSVariablesFromExpr[T](o: JSONObject): List[T] = {
    var list = ListBuffer[T]()

    if (!o.keySet().contains("Type")) {
      return list.toList
    }

    o.getString("Type") match {
      case "DeclRef" =>
        var name = getNameFromDecl(o.getJSONObject("DeclRef"))
        list += name.asInstanceOf[T]
      case "BinaryOperator" =>
        var bo = o.getJSONObject("BinaryOperator")
        o.getString("Opcode") match {
          case "=" | "++" | "%=" | "+=" | "--" =>
            list ++= getLHSVariablesFromExpr(bo.getJSONObject("Lhs"))
            list ++= getLHSVariablesFromExpr(bo.getJSONObject("Rhs"))
          case _ => ()
        }
      case "UnaryOperator" =>
        o.getString("OpCode") match {
          case "++" | "--" =>
            list ++= getLHSVariablesFromExpr(o.getJSONObject("SubExpr"))
          case _ => ()
        }
      case "Expr" =>
        if (o.keySet.contains("Body"))
          list ++= getLHSVariablesFromExpr(o.getJSONObject("Body"))
      case _ => ()
    }

    return list.toList
  }

  def getVariablesFromExpr[T](o: JSONObject): List[T] = {
    var list = ListBuffer[T]();

    if (!o.keySet().contains("Type")) {
      return list.toList
    }

    o.getString("Type") match {
      case "DeclRef" =>
        var name = getNameFromDecl(o.getJSONObject("DeclRef"))
        list += name.asInstanceOf[T]
      case "Call" =>
        var call = o.getJSONObject("Call")
        var args = call.getJSONArray("Args")
        for (i <- Range(0, args.length())) {
          list ++= getVariablesFromExpr(args.getJSONObject(i))
        }
      case "BinaryOperator" =>
        var bo = o.getJSONObject("BinaryOperator")
        list ++= getVariablesFromExpr(bo.getJSONObject("Lhs"))
        list ++= getVariablesFromExpr(bo.getJSONObject("Rhs"))
      case "UnaryOperator" =>
        list ++= getVariablesFromExpr(o.getJSONObject("SubExpr"))
      case "Expr" =>
        if (o.keySet().contains("Body"))
          list ++= getVariablesFromExpr(o.getJSONObject("Body"))
      case "ArraySubscriptExpr" =>
        list ++= getVariablesFromExpr(o.getJSONObject("Lhs"))
        list ++= getVariablesFromExpr(o.getJSONObject("Rhs"))
      case _ =>
        // maybe it is an arg
        if (o.keySet().contains("Arg")) {
          list ++= getVariablesFromExpr(o.getJSONObject("Arg"))
        } else {
          //println("getVariablesFromExpr : unknown type " + o.getString("Type"))
          //println(o.toString())
        }
    }
    return list.toList
  }

  def getLocalFromStmt(o: JSONObject): (String, String) = {
    return (getNameFromDecl(o), getTypeFromDecl(o))
  }

  def processFunction(f: Function) = {

    if (f.jo.keySet().contains("FunctionDecl")) {

      var fdo = f.jo.getJSONObject("FunctionDecl")

      println("++++++++++++++++")
      // name
      println("Function: " + f.name)

      // return type
      println("Return Type: " + fdo.getString("ReturnType"))

      // parameters
      var params = fdo.getJSONArray("Parameters")
      var paramsList = ListBuffer[(String, String)]()
      for (i <- Range(0, params.length())) {
        var param = params.getJSONObject(i).getJSONObject("ParmVar")
        var p = (param.getString("Name"), param.getString("Type"))
        f.params += Parameter(p._1, Type(p._2))
        paramsList += p
      }
      println("Params: " + paramsList.mkString(","))

      // locals
      var locals = List[(String, String)]()
      if (fdo.keySet().contains("Body") &&
        fdo.getJSONObject("Body").keySet().contains("Body")) {
        var body = fdo.getJSONObject("Body")
        locals = recursivelyProcessDecls[(String, String)](body, getLocalFromStmt)
        locals foreach {
          case l =>
            f.locals += Parameter(l._1, Type(l._2)) // TODO
        }
        println("Locals: " + locals.mkString(":"))
      } else {
        println("No locals found.")
      }

      // stack size
      var stackSize = locals.foldLeft(4)((r, l) => r + getSizeFromType(l._2))
      stackSize += paramsList.foldLeft(0)((r, p) => r + getSizeFromType(p._2))
      println("Estimated Stack Size: " + stackSize)

      // number of calls

      // variables used (mod def)
      var varsUsed = List[(String)]()
      if (fdo.keySet().contains("Body") &&
        fdo.getJSONObject("Body").keySet().contains("Body")) {
        var body = fdo.getJSONObject("Body")
        varsUsed = recursivelyProcessExprs[String](body, getVariablesFromExpr[String])
        println("Variables used: " + varsUsed.toSet.mkString(","))
      } else {
        println("No used variables found.")
      }

      // statement count
      if (fdo.keySet().contains("Body") &&
        fdo.getJSONObject("Body").keySet().contains("Body")) {
        println("Statement count: " + getStatementCount(fdo.getJSONObject("Body")))
      } else {
        println("No body found.")
      }

      // variables used in lhs
      var lhsVars = List[(String)]()
      if (fdo.keySet().contains("Body") &&
        fdo.getJSONObject("Body").keySet().contains("Body")) {
        var body = fdo.getJSONObject("Body")
        lhsVars = recursivelyProcessExprs[String](body, getLHSVariablesFromExpr[String])
        lhsVars foreach {
          case l =>
            f.modSet += Parameter(l, null) // TODO
        }
        println("LHS Variables: " + lhsVars.toSet.mkString(","))
      } else {
        println("No LHS variables found.")
      }

      // body
      println("---------------")
    }
  }

  def getSizeFromType(t: String): Int = {
    t match {
      case "int"  => 4
      case "char" => 1
      case _      => 4
    }
  }
  
}
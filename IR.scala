package lib

import org.json._
import scala.collection.mutable.{ Set => MSet }
import scala.collection.mutable.{ MutableList => MList }
import java.util.Formatter.Conversion

object IR {

  val voidType: Type = Type("void")

}

class CfgBlock {
  // TODO: fill this part out
}

class SourceLocation {
  val file: String = ""
  val line: String = ""
  override def toString() = "$file@$line"
}

class IR(id: Int, jo: JSONObject) {
  var name: String = ""
  val related: Set[IR] = Set()
  val sl: SourceLocation = null
}

case class TypeDef(id: Int, jo: JSONObject) {
  val identifier: String = jo.getString("Identifier")
  val t: Type = Type(jo.getString("Type"))
  
  override def toString() = {
    "typedef " + t + " ;\n"
  } 
}

case class Type(name: String) {

  def equals(o:Type) : Boolean = o.name.equals(name) 
  
  def size(): Int = {
    name match {
      case "int" => 4
      case _     => 4
    }
  }

  override def toString() = name
}

case class Enum(id: Int, jo: JSONObject) extends IR(id, jo) {
  name = jo.getJSONObject("EnumDecl").getString("Identifier")
  var entries: MList[(String, Int)] = MList()
  val enumeration = jo.getJSONObject("EnumDecl").getJSONArray("Enumeration")

  for (i <- Range(0, enumeration.length())) {
    val o = enumeration.getJSONObject(i)
    val key = o.keys().next()
    if (o.get(key).isInstanceOf[JSONObject]) {
      val oo = o.getJSONObject(key)
      val enumType = oo.getString("Type")
      entries += ((key, oo.get(enumType).asInstanceOf[Int]))
    } else {
      entries += ((key, o.get(key).asInstanceOf[Int]))
    }
  }

  override def toString(): String = {
    var retVal: String = ""
    retVal += (s"enum $name {")
    retVal += entries.mkString(",")
    retVal += "}"
    return retVal
  }

}

case class Module(id: Int, jo: JSONObject) extends IR(id, jo) {
  val translationUnits: MSet[TranslationUnit] = MSet()

  name = jo.getString("Name")

  override def toString() = {
    var result = ""
    for (tu <- translationUnits) {
      result += tu.toString() + "-----"
    }
    result
  }
}

case class TranslationUnit(id: Int, jo: JSONObject) extends IR(id, jo) {
  val functions: MSet[Function] = MSet()
  val globals: MSet[Variable] = MSet()
  val imports: MSet[String] = MSet()
  val enums: MSet[Enum] = MSet()
  val typedefs: MSet[TypeDef] = MSet()

  name = jo.getString("Name")

  override def toString() = {
    var result = ""
    for (global <- globals) {
      result += global.toString() + ";\n"
    }
    for (function <- functions) {
      result += function.toString() + "\n"
    }
    result
  }
}

case class Function(id: Int, jo: JSONObject) extends IR(id, jo) {
  val locals: MSet[Parameter] = MSet() // TODO
  val params: MSet[Parameter] = MSet()
  var returnType: Type = null
  var cfg: CfgBlock = null
  var modSet: MSet[Parameter] = MSet() // TODO

  if (jo.keySet().contains("FunctionDecl")) {

    name = jo.getJSONObject("FunctionDecl").getString("Name")

    returnType = Type(jo.getJSONObject("FunctionDecl").getString("ReturnType"))
  } else {
    name = ""
    returnType = Type("unknown")
  }

  override def toString() = {
    var result = ""
    result += returnType.toString() + " "
    result += name + "("
    result += params.mkString(",")
    result += ") {"
    if (cfg != null) result += cfg // TODO
    result += "}"
    result
  }
}

case class Parameter(name: String, t: Type) {

}

case class Variable(id: Int, jo: JSONObject) extends IR(id, jo) {
  val varType: Type = Type(jo.getJSONObject("Var").getString("Type"))
  name = jo.getJSONObject("Var").getString("Name")

  override def toString = {
    var result = ""
    result += varType + " " + name + ";\n"
    result
  }
  
  override def hashCode: Int =
    41 *
      (41 + varType.hashCode()) +
      (41 + id.hashCode()) +
      (41 + jo.hashCode()) +
      name.hashCode()

  override def equals(o: Any): Boolean = {
    o.isInstanceOf[Variable] &&
      o.asInstanceOf[Variable].varType.equals(varType) &&
      o.asInstanceOf[Variable].jo.equals(jo) &&
      o.asInstanceOf[Variable].id.equals(id) &&
      o.asInstanceOf[Variable].name.equals(name)
  }
}

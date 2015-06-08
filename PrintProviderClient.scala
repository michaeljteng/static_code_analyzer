package lib

import org.json._

object PrintProviderClient extends Provider {

  override def getModule(fileNames: List[String]): Module = {
    val moduleJsonObject: JSONObject = Util.str2jsonObject("{\"Name\":\"module\"}")
    var module: Module = Module(-1, moduleJsonObject)

    for (fileName <- fileNames) {
      val fileLines: List[String] = scala.io.Source.fromFile(fileName).getLines.toList
      val tuJsonObject: JSONObject = Util.str2jsonObject("{\"Name\":\"" + (new java.io.File(fileName)).getName + "\", \"Id\":\"0\"}")
      val tuFactLines: List[String] = fileLines.filter { x => !x.contains("factfact") }
      val tuFactFactLines: List[String] = fileLines.filter { x => x.contains("factfact") }
      val tu: TranslationUnit = TranslationUnit(tuJsonObject.getInt("Id"), tuJsonObject)

      for (tuFactLine <- tuFactLines) {
        val tuFactLineObject: JSONObject = Util.str2jsonObject(tuFactLine)
        tuFactLineObject.getString("Type") match {
          case "Function" =>
            tu.functions += lib.Function(tuFactLineObject.getInt("Id"), tuFactLineObject)
          case "Var" =>
            tu.globals += lib.Variable(tuFactLineObject.getInt("Id"), tuFactLineObject)
          case "Enum" =>
            tu.enums += lib.Enum(tuFactLineObject.getInt("Id"), tuFactLineObject)
          case "Typedef" =>
            tu.typedefs += TypeDef(tuFactLineObject.getInt("Id"), tuFactLineObject.getJSONObject("Typedef"))
          case "Record" =>
            ()
          case unknown =>
            //println("Implement support for " + unknown)
            //println(tuFactLine)
        }
      }

      module.translationUnits.add(tu)
    }

    return module
  }


}
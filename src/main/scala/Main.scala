object Main {
  def main(args: Array[String]) {
    val spec = """[(col5),col4+' -- '+col3,col2+","+col1]"""
    val row = List("alpha","bravo","charlie","delta","echo")

    val ast = transform.ImportTransformer(spec)
    println(ast)
    ast match {
      case Some(parsed) => println(parsed(row))
      case None => println("Failed to parse!")
    }
  }
}

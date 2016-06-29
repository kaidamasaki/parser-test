object Main {
  def main(args: Array[String]) {
    val spec = """[(col5),trim(col4)+'--'+col3,col2+";"+col1,slice(col6,8),slice(col7,0,4),col9]"""
    val row = List("lie","char",
                   "vo"," bra ",
                   "alfa",
                   "a small delta",
                   "echo echo echo!",
                   "ignored",
                   "foxtrot")

    val ast = transform.ImportTransformer(spec)
    println(ast)
    ast match {
      case Some(parsed) => println(parsed(row))
      case None => println("Failed to parse!")
    }
  }
}

object Main {
  def main(args: Array[String]) {
    // val spec = """[col9001,(col5),trim(col4)+'--'+col3,col2+";"+col1,slice(col6,8),slice(col7,0,4),{f:col9,f2:{g:col10},f3:{}},col11.replace(/m/,"h"),/india/gi]"""
    val spec = """[col9001,(col5),trim(col4)+'--'+col3,col2+";"+col1,slice(col6,8),slice(col7,0,4),{f:col9,f2:{g:col10},f3:{}},col11.replace(/m/,"h"),/india/gi]"""
    val row = List("lie","char",
                   "vo"," bra ",
                   "alfa",
                   "a small delta",
                   "echo echo echo!",
                   "ignored",
                   "foxtrot",
                   "golf",
                   "motel")

    val ast = transform.ast.Transform(spec)
    println(ast)
    ast match {
      case Some(parsed) => println(parsed(row))
      case None => println("Failed to parse!")
    }
  }
}

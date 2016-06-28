object Main {
  def main(args: Array[String]) {
    val spec = """[(col5),col4,col3,col2+","+col1]"""
    val row = Seq("foo","bar","baz","17","true")

    println(transform.ImportTransformer(spec))
  }
}

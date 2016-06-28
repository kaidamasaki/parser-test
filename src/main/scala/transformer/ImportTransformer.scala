package transform

object ImportTransformer {
  def apply(transformDefinition: String): Option[ast.Transform] = try {
    ast.Transform(transformDefinition)
  } catch {
    case e: IllegalArgumentException =>
      e.printStackTrace()
      None
  }
}

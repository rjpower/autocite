package autocite

import scala.xml.Node
object XMLImplicits {
  /**
   * Helper class for dealing with XML attributes.
   *
   * Adds the attr(name : String) method to a node, which
   * returns the text value of the first matching attribute.
   */
  final class NodeView(val n: Node) {
    def attr(name: String): String = {
      n.attribute(name).get(0).text
    }
    
    def iattr(name : String): Int = {
      n.attribute(name).get(0).text.toInt
    }
  }

  implicit def nodeToHelper(n: Node) = new NodeView(n)
}

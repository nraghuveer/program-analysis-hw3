
package hwk;

import java.io.File
 

object Main {
  def main(args: Array[String]) { 
    // val ast = GenerateAST(new File("test/simple.js"))
    val ast = GenerateAST(new File("hwk2/test/while.js"))
    ast.prep
    
    print(ast)
    
    ast.buildGraph(ast)

  }
}

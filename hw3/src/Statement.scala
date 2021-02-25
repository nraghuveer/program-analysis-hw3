
package hwk

import java.util.Optional
import java.util.concurrent.Callable
import scala.collection.mutable.ListBuffer
;



sealed abstract class Statement extends AbstractSyntaxTree {
  // add label to statements and add height
  def prep {
    this.setid
    this match {
      case Script(stmts) => { addHeights(stmts); stmts.foreach(s => s.prep); }
      case BlockStmt(stmts) => { addHeights(stmts); stmts.foreach(s => s.prep); }
      case VarDeclListStmt(decls) => { addHeights(decls); decls.foreach(s => s.prep); }
      case FunctionDecl(_, fun) => { fun.height = height; addHeight(fun.asInstanceOf[FunctionExpr].body); }
      case IfStmt(_, thenPart, elsePart) => { addHeights(List(thenPart, elsePart)); thenPart.prep; elsePart.prep; }
      case SwitchStmt(_, cases, defaultCase) => {
        val d =  cases ++ (defaultCase match {case Some(c) => {c.default=true; List(c)} case None => List()})
        addHeights(d)
        d.foreach(c => c.prep)
       
      }
      case CaseStmt(_, body) => { addHeight(body); body.prep; }
      case DoWhileStmt(_, body) => { addHeight(body); body.prep; }
      case WhileStmt(_, body) => { addHeight(body); body.prep; }
      case ForStmt(_, _, _, body) => {  addHeight(body); body.prep; }
      case ForInStmt(_, _, body) => { addHeight(body); body.prep; }
      case LabeledStmt(_, body) => { addHeight(body); body.prep; }
      case _ =>
    }
  }
  
  override def toString = this match {
    case Script(stmts) => toString(stmts)
    case BlockStmt(stmts) => space + "{\n" + toString(stmts) + space + "}"
    case VarDeclListStmt(decls) => toString(decls)
    case EmptyStmt() => ""
    case ExprStmt(expr) => space + expr.toString
    case VarDeclStmt(name, expr) => { 
      val e = expr match { case EmptyExpr() => "" case _ => " = " + expr }
      space + "var " + name + e
    }
    case FunctionDecl(name, fun) =>  fun.toString
    case ReturnStmt(expr) => space + "return " + expr
    case IfStmt(cond, thenPart, elsePart) => { 
      val e = elsePart match { case EmptyStmt() => "" case _ => " else\n" + elsePart }
      space + "if (" + cond + ") " + "\n" + thenPart + e
    }
    case SwitchStmt(cond, cases, defaultCase) => {      
      val d =  cases ++ (defaultCase match {case Some(c) => List(c) case None => List()}) 
      space + "switch (" + cond + ") {\n" + d.foldRight("")((s, c) => s + "\n" + c) + space + "}"
    }
    case c@CaseStmt(expr, body) => { 
      c.default match {
        case true => space + "default :\n" + body
        case false => space + "case " + expr + " :\n" + body
      }
    }
    case BreakStmt(breakLabel) => space + "break " + breakLabel
    case ContinueStmt(continueLabel) => space + "continue " + continueLabel
    case DoWhileStmt(cond, body) => space + "do\n" + body + "while (" + cond + ")\n" 
    case WhileStmt(cond, body) =>  space + "while (" + cond + ")\n" + body  
    case ForStmt(init, cond, increment, body) => { 
      val c = cond match { case Some(x)=>x.toString case None => "" }
      val i = increment match { case Some(x)=>x.toString case None => "" }
      space + "for (" + init + "; " + c + "; " + i + ")\n" + body
    }
    case ForInStmt(init, expr, body) => space + "for (" + init + " in " + expr + ")" + body
    case LabeledStmt(label, body) => label.foldRight("")((e,c) => space + e + ":\n" + c) + body 
    case _ => ""
  }
  
  def toString(stmts: List[Statement]) = stmts.foldRight("")((s, c) =>  
    (s match { 
      case WhileStmt(_,_) => s+"\n"
      case DoWhileStmt(_,_) => s+"\n"
      case ForStmt(_,_,_,_) => s+"\n"
      case ForInStmt(_,_,_) => s+"\n"
      case FunctionDecl(_,_) => s+"\n"
      case IfStmt(_,_,_) => s+"\n"
      case SwitchStmt(_,_,_) => s+"\n" 
      case EmptyStmt() => ""
      case _ =>  s + ";\n"  
    }) + c
  )

  // Homework changes are from here ...

  def makeDotLines(s: (Long, Long)): String = s"${s._1} -> ${s._2}"


  // class to hold the label properties for a given statement
  // 1. init
  // 2. final
  // 3. flow
  // 4. reverse flow
  case class LabelProps(label_init: Label, label_final: List[Label], label_flow: List[(Label, Label)]) {
    def reverse_flow = this.label_flow.reverse
  }

  var labelProps: LabelProps = _
  var dotLines: List[String] = List()  // dot lines notation for the given statement


  def generateLabelProps {
    this match {
      case Script(stmts) => { stmts.foreach(s => s.generateLabelProps) }
      case VarDeclStmt(_, expr) => {
        this.labelProps = LabelProps(this.id, List(this.id), List())
      }
      case EmptyStmt() => { this.labelProps = LabelProps(this.id, List(this.id), List())}
      case ExprStmt(expr) => {
        this.labelProps = LabelProps(this.id, List(this.id), List())
      }
      case IfStmt(_, thenPart, elsePart) => {
        // first generate for the child i.e then and else parts
        thenPart.generateLabelProps
        elsePart.generateLabelProps
        val label_final = thenPart.labelProps.label_final ++ elsePart.labelProps.label_final
        // 1. combine then and else parts flow
        // 2. connect if cond to the then and else part inits
        val label_flow: List[(Label, Label)] = thenPart.labelProps.label_flow ++ elsePart.labelProps.label_flow ++ List((this.id, thenPart.labelProps.label_init), (this.id, elsePart.labelProps.label_init))
        this.labelProps = LabelProps(this.id, label_final, label_flow)
        this.dotLines = this.dotLines ++ label_flow.map(makeDotLines)
        }
      case WhileStmt(_, body) => {
        body.generateLabelProps
        // for while init and final are gonna be same
        // 1. connect while condition to the body init
        // 2. connect body final points to the while cond
        val label_flow: List[(Label, Label)] = List((this.id, body.labelProps.label_init)) ++ body.labelProps.label_flow ++ body.labelProps.label_final.map((_, this.id))
        this.labelProps = LabelProps(this.id, List(this.id), label_flow)


        // generate dot lines inside a subgraph for the dotnotation
        this.dotLines = this.dotLines :+ s"subgraph cluster${this.id} {"
        this.dotLines = this.dotLines ++ List(makeDotLines(this.id, body.labelProps.label_init)) ++ body.labelProps.label_final.map(p => makeDotLines(p, this.id))
        this.dotLines = this.dotLines ++ body.dotLines
        this.dotLines = this.dotLines ++ List(s"""label = "while - ${this.asInstanceOf[WhileStmt].cond.toString}" """)
        this.dotLines = this.dotLines :+ "}"
      }
      case BlockStmt(stmts) => {
        // generate label properties for each statment in the block
        stmts.foreach(s => s.generateLabelProps)
        val label_init = stmts.head.labelProps.label_init
        // final is same as final statement of the block
        val label_final = stmts.last.labelProps.label_final
        // connect consecutive statements i.e 1 to 2, 2 to 3, 3 to 4
        // if these is only statement, set empty set
        var inner_flow: List[(Label, Label)] = if (stmts.length > 1) stmts.sliding(2).toList.map(group => group(0).labelProps.label_final.map(f => (f, group(1).labelProps.label_init))).flatten else List[(Label, Label)]()
        this.dotLines = this.dotLines ++ inner_flow.map(makeDotLines) ++ stmts.map(_.dotLines).flatten
        val label_flow = inner_flow ++ stmts.map(_.labelProps.label_flow).flatten
        this.labelProps = LabelProps(label_init, label_final, label_flow)

      }
    }
  }
  

  def buildGraph(stmt: Statement): Unit =  {
    stmt.generateLabelProps
    val cfBuilder = new ControlFlowBuilder()
    cfBuilder.build(stmt, StartStatement())
    cfBuilder.generateCodeLabels(stmt)
    cfBuilder.toDotNotion
  }
}

// Given statements returns a graph of labels
class ControlFlowBuilder() {
  var flow: List[(AbstractSyntaxTree.Label, AbstractSyntaxTree.Label)] = List()
  var prevStatement: Statement = StartStatement()
  var idMap: Map[Long, String] = Map((-1.asInstanceOf[Long] -> "START"))
  var dotNotationLines: ListBuffer[String] = ListBuffer[String]()

  def generateCodeLabels(stmt: Statement): Unit = {  // generates display code labels and maps with the assigned id
    stmt match {
      case script: Script => script.stmts.foreach(s => this.generateCodeLabels(s))
      case varDeclStmt: VarDeclStmt => this.idMap = this.idMap + (varDeclStmt.id -> varDeclStmt.toString)
      case exprStmt: ExprStmt => this.idMap = this.idMap + (exprStmt.id -> exprStmt.toString)
      case ifStmt: IfStmt => {
        this.idMap = this.idMap + (ifStmt.id -> ifStmt.cond.toString)
        // recurse for then and else blocks
        this.generateCodeLabels(ifStmt.thenPart)
        this.generateCodeLabels(ifStmt.elsePart)
      }
      case whileStmt: WhileStmt => {
        this.idMap = this.idMap + (whileStmt.id -> whileStmt.cond.toString)
        // recurse for the body
        this.generateCodeLabels(whileStmt.body)
      }
      case blockStmt: BlockStmt => {
        // recurse for all the statements block
        blockStmt.stmts.foreach(s => this.generateCodeLabels(s))
      }
      case emptyStmt: EmptyStmt => this.idMap = this.idMap + (emptyStmt.id -> s"${emptyStmt.id}: Empty Stmt")
      case _ =>
    }
  }

  def toDotNotion = {
    println("\n\n############ DOT FILE ###################\n\n")
    println("digraph G{")
    println("node [shape = rec, height=.3];")
    this.idMap.foreach(x => println(s""" "${x._1}" [label="${x._2}"] """))
    this.dotNotationLines.foreach(println)
    println("}")
    println("\n#######################################\n")
  }

  def attachToFlow(stmt: Statement, prev_stmt: Statement): Unit = {
    // given a statement and parent, attaches all of the statements flow and parents final to the statement init
    this.dotNotationLines = this.dotNotationLines ++ prev_stmt.labelProps.label_final.map(p => s"${p} -> ${stmt.labelProps.label_init}") ++ stmt.labelProps.label_flow.map(p => s"${p._1} -> ${p._2}")
  }

  def makeDotLine(s: (Long, Long)): String = s"${s._1} -> ${s._2}"

  def build(stmt: Statement, prev_stmt: Statement): Unit = {
    stmt match {
      case script: Script => {
        // connect the consecutive statements i.e id's
        (List(prev_stmt) ++ script.stmts).sliding(2).toList.map(group => this.build(group(1), group(0)))
      }
      case varDeclStmt: VarDeclStmt => {
        // connect with given prev statement
        this.dotNotationLines = this.dotNotationLines ++ prev_stmt.labelProps.label_final.map(p => makeDotLine(p, varDeclStmt.labelProps.label_init))
      }
      case exprStmt: ExprStmt => {
        // connect with given prev statement
        this.dotNotationLines = this.dotNotationLines ++ prev_stmt.labelProps.label_final.map(p => makeDotLine(p, exprStmt.labelProps.label_init))
      }
      case ifStmt: IfStmt => {
        this.dotNotationLines = this.dotNotationLines ++ ifStmt.dotLines
      }
      case whileStmt: WhileStmt => {
        this.dotNotationLines = this.dotNotationLines ++ prev_stmt.labelProps.label_final.map(p => s"${p} -> ${whileStmt.labelProps.label_init}")
        this.dotNotationLines = this.dotNotationLines ++ whileStmt.dotLines
      }
      case blockStmt: BlockStmt => {
        this.dotNotationLines = this.dotNotationLines ++ prev_stmt.labelProps.label_final.map(p => s"${p} -> ${blockStmt.stmts(0).labelProps.label_init}") ++ blockStmt.dotLines
      }
      case emptyStmt: EmptyStmt =>
    }

  }
}


case class StartStatement() extends Statement {  // Represents start block, just a placebo
  labelProps = LabelProps(-1, List(-1), List())  // -1 is later referred as START code label
};
case class Script(stmts : List[Statement]) extends  Statement  // s1; s2
case class BlockStmt(stmts : List[Statement]) extends Statement  // { s1; s2; }
case class VarDeclListStmt(decls : List[Statement]) extends Statement  // var a=1, b=2
case class EmptyStmt() extends Statement
case class ExprStmt(expr : Expression) extends Statement()  // console.log(x); or b = 10;
case class VarDeclStmt(name : IntroduceVar, expr : Expression) extends Statement  // var a=1;
case class FunctionDecl(name : IntroduceVar, fun : Expression) extends Statement  // function fact(n) { ... }
case class ReturnStmt(expr : Expression) extends Statement  // return 1
case class IfStmt(cond : Expression, thenPart : Statement, elsePart : Statement) extends Statement
case class SwitchStmt(cond : Expression, cases : List[CaseStmt], defaultCase : Option[CaseStmt]) extends Statement
case class CaseStmt(expr : Expression, body : Statement) extends Statement { var default = false }
case class BreakStmt(breakLabel : String) extends Statement
case class ContinueStmt(continueLabel : String) extends Statement
case class DoWhileStmt(cond : Expression, body : Statement) extends Statement
case class WhileStmt(cond : Expression, body : Statement) extends Statement
case class ForStmt(init : ForInit, cond : Option[Expression], increment : Option[Expression], body : Statement) extends Statement
case class ForInStmt(init : ForInInit, expr : Expression, body : Statement) extends Statement
case class LabeledStmt(label : List[String], stmt : Statement) extends Statement
case class TryStmt(body : Statement, catchClause : List[CatchStmt], finalCatch : Option[Statement]) extends Statement
case class CatchStmt(name : IntroduceVar, body : Statement) extends Statement
case class ThrowStmt(expr : Expression) extends Statement




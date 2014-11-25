import com.github.tototoshi.csv._
import java.io.{UnsupportedEncodingException, FileReader, File, StringReader}
import scala.collection.mutable.ListBuffer

class DecisionNode(col:Int = -1, v:String = "", tb:DecisionNode , fb:DecisionNode, r:Map[String, Int]){
  val column: Int = col
  val value: String = v
  val truthb: DecisionNode = tb
  val falseb: DecisionNode = fb
  val results: Map[String, Int] = r
  def this() = this(-1,"",null,null, Map[String, Int]())
  def this(r:Map[String, Int]) = this(-1,"",null,null, r)
  def this(col:Int, v:String, tb:DecisionNode , fb:DecisionNode) = this(col,v,tb,fb,Map[String, Int]())


}

class DecisionTree(){
  def log2(x: Double) = scala.math.log(x)/scala.math.log(2)
  def isNumeric(input: String): Boolean = input.forall(_.isDigit)

  def printTree(tree:DecisionNode, indent:String=""):Unit = {
    if (!tree.results.isEmpty){
      println(tree.results)
    }else{
      //criteria
      println("Column-" +  tree.column + " : " + tree.value + "? ")

      //branches
      print(indent + "T->")
      printTree(tree.truthb , indent + "   " )
      print(indent + "F->")
      printTree(tree.falseb , indent + "   " )

    }
  }
  def buildtree(rows:List[List[String]] ): DecisionNode = {
      if(rows.length==0) return new DecisionNode()

      var currentScore:Double = entropy(rows)
      var bestGain:Double = 0.0
      var bestValue:String = ""
      var bestCol:Int = -1
      var bestSets:Tuple2[List[List[String]],List[List[String]]] = (null,null)


      val columnCount:Int = rows(0).length
     // first column is id, last column is the result
      for(col <- (0 to (columnCount - 2))){
      //for(col <- (1 to (columnCount - 2))){
        var uniqCol:Set[String] = Set()
        for (row <- rows) {
          uniqCol += row(col)
        }
        for(value <- uniqCol){
          val resultSets =  divideSet(rows,col,value)
          val p = resultSets._1.length.toFloat/rows.length
          val gain = currentScore - p*entropy(resultSets._1) - (1-p)*entropy(resultSets._2)
          if (gain > bestGain && resultSets._1.length>0 && resultSets._2.length>0){
              //println("New best gain value ,col: " , value , col)
              bestGain = gain
              bestValue = value
              bestCol = col
              bestSets = resultSets
          }
        }
      }
      if (bestGain>0){
        val trueBranch = buildtree(bestSets._1)
        val falseBranch = buildtree(bestSets._2)
        return new DecisionNode(col=bestCol,v=bestValue, tb=trueBranch, fb=falseBranch, Map[String, Int]() )
      } else {
        return new DecisionNode(r=uniqueCounts(rows))
      }
  }

  def entropy(rows:List[List[String]]): Double = {
    /*
      Calculate the entropy of of entire data set

     */
    val c:Map[String, Int] = uniqueCounts(rows)
    var ent:Double = 0.0
    for ((k,v) <- c){
      val p:Float = v.toFloat/rows.length
      ent = ent-p*log2(p)
    }
    return ent
  }


  def uniqueCounts(rows:List[List[String]]): Map[String, Int] = {
    /*
        Calculate the count of each unique result (last column is the results)
     */
    return rows.groupBy(_.last).map(t => (t._1, t._2.length))
  }


  def divideSet(rows:List[List[String]], column:Int, value:String): Tuple2[List[List[String]],List[List[String]]]  = {
    /*
        Divide set into two separate result tuples based on value
        If is string then equal or not and if number than larger or smaller

     */
    var l1 = ListBuffer[List[String]]()
    var l2 = ListBuffer[List[String]]()

    if(isNumeric(value)) {
      for (row <- rows) {
        val currentValue: Int = row(column).toInt
        if (currentValue >= value.toInt) {
          l1 += row
        } else {
          l2 += row
        }
      }
    }else{
      for (row <- rows) {
        if (row(column) == value) {
          l1 += row
        } else {
          l2 += row
        }
      }
    }
    return (l1.toList,l2.toList)
  }

}
object First{
  def main(args:Array[String]): Unit ={
    //val a = new DecisionNode(1,"2",null,null, Map[Int, String]() )
    val reader = CSVReader.open(new File("src/main/resources/train.csv"))
    //val reader = CSVReader.open(new File("src/main/resources/decision_tree_example.csv"))
    val dt = new DecisionTree()

    val rows: List[List[String]]  = reader.all().drop(13000)
    //val rows: List[List[String]]  = reader.all()
    //println(dt.divideSet(rows,2,"yes"))
    //print(dt.entropy(rows))
    val tree = dt.buildtree(rows)
    dt.printTree(tree)
    //val result_sets:Tuple2[ListBuffer[List[String]], ListBuffer[List[String]]] = dt.divideSet(rows, 1, 2500)
    //println(result_sets._1)
    //println(result_sets._2)
    //println (dt.uniqueCounts(result_sets._2))
    //reader foreach { fields =>
    //  println(fields)
    //}
    reader.close()
  }
}
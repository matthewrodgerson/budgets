package controllers
import domain._
//import org.json4s._

object test {
val projList = List(("F163","AAAAA","project A","SIT1"),("F163","AAAAA","project A","SIT2"),("F163","BBBBB","project B","SIT2"),("F162","CCCCC","project C","SIT3"))
                                                  //> projList  : List[(String, String, String, String)] = List((F163,AAAAA,projec
                                                  //| t A,SIT1), (F163,AAAAA,project A,SIT2), (F163,BBBBB,project B,SIT2), (F162,C
                                                  //| CCCC,project C,SIT3))

def q(inString:String):String = {
 '"'+inString+'"'
}                                                 //> q: (inString: String)String

def c(start:String,typeList:List[String],valList:List[String]):String = {
  if (typeList.isEmpty) start.substring(0,start.length-1)+"}" else
  c(start+q(typeList.head)+":"+q(valList.head)+",",typeList.tail,valList.tail)
}                                                 //> c: (start: String, typeList: List[String], valList: List[String])String
val nameList = List("name","desc","SIT cycle")    //> nameList  : List[String] = List(name, desc, SIT cycle)
val nameList2 = List("desc","SIT cycle")          //> nameList2  : List[String] = List(desc, SIT cycle)

def toJSON (inList:List[(String,String,String,String)],outputString:String,currentReleaseLevel:String,currentProjectLevel:String):String = {
 if (inList.isEmpty) return outputString+"]}" else {
   

   // Same release and project
   if (inList.head._1 == currentReleaseLevel & inList.head._2 == currentProjectLevel) {
   val extraBit = ","+c("{",nameList2,List(inList.head._3,inList.head._4))
   val newString = outputString + extraBit
   toJSON (inList.tail,newString,currentReleaseLevel,currentProjectLevel) }
  
   // Same release, different project
   else if (inList.head._1 == currentReleaseLevel & inList.head._2 !=currentProjectLevel) {
   val extraBit = "]},[{"+q("name")+":"+q(inList.head._2)+ ","+q("children")+ ":[" +
     //c("{",nameList,List(inList.head._2,inList.head._3,inList.head._4))
     //"{"+ q("name")+":"+q(inList.head._2)+ ","+q("children")+ ":["+
     c("{",nameList2,List(inList.head._3,inList.head._4))
   val newString = outputString + extraBit
   toJSON (inList.tail,newString,currentReleaseLevel,inList.head._2) }
      
   // New Release level, not first time
   else if (currentReleaseLevel != "") {
   val extraBit = "]},{"+ q("name")+":"+q(inList.head._1)+ ","+q("children")+ ":[" +
     "{"+ q("name")+":"+q(inList.head._2)+ ","+q("children")+ ":["+
     c("{",nameList2,List(inList.head._3,inList.head._4))
   val newString = outputString + extraBit
   toJSON (inList.tail,newString,inList.head._1,inList.head._2) }
 
   // First time through
   else {
   val extraBit = "[{"+q("name")+":"+q(inList.head._1)+ ","+q("children")+ ":[" +
     //c("{",nameList,List(inList.head._2,inList.head._3,inList.head._4))
     "{"+ q("name")+":"+q(inList.head._2)+ ","+q("children")+ ":["+
     c("{",nameList2,List(inList.head._3,inList.head._4))
   val newString = outputString + extraBit
   toJSON (inList.tail,newString,inList.head._1,inList.head._2) }
  
 }
}                                                 //> toJSON: (inList: List[(String, String, String, String)], outputString: Stri
                                                  //| ng, currentReleaseLevel: String, currentProjectLevel: String)String
val starter = "{"+q("name")+":"+q("Releases")+","+q("children")+":"
                                                  //> starter  : String = {"name":"Releases","children":
val endJSON = toJSON (projList,starter,"","")+"]}"//> endJSON  : String = {"name":"Releases","children":[{"name":"F163","children
                                                  //| ":[{"name":"AAAAA","children":[{"desc":"project A","SIT cycle":"SIT1"},{"de
                                                  //| sc":"project A","SIT cycle":"SIT2"}]},[{"name":"BBBBB","children":[{"desc":
                                                  //| "project B","SIT cycle":"SIT2"}]},{"name":"F162","children":[{"name":"CCCCC
                                                  //| ","children":[{"desc":"project C","SIT cycle":"SIT3"}]}]}
}


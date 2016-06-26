package domain

import javax.activity.ActivityCompletedException

/**
 * @author RodgersonM
 */

case class Project(val name: String, val projectDesc:String, val children:List[Activity]) {}

case class Activity(val name:String, val children:List[Effort])

case class Effort(val name:String, val effortDays:Double) {}


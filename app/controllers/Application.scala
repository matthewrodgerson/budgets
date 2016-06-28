package controllers

import play.api._
import play.api.mvc._
import dal._
import domain._
import play.api.libs.json._
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}


object EffortDal {
  val effortDal = new dal.EffortDal {}
  val releaseList = effortDal.getProjects("")
}

class Application extends Controller {

  def searchProjects(searchTerm: String, perPage:Int, pageNo:Int) = 
    {displayProjList(perPage,pageNo,searchTerm)}
 
  def q(inString:String):String = {
     '"'+inString+'"'}

  def sortProjects(inProjects:List[Project]):List[Project] = {
    if (inProjects.tail.isEmpty) List(new(Project) (inProjects.head.name, inProjects.head.projectDesc, sortActivities(inProjects.head.children)))
    else new (Project) (inProjects.head.name, inProjects.head.projectDesc, sortActivities(inProjects.head.children)) :: sortProjects(inProjects.tail)
  }

  def sortActivities(inActivities:List[Activity]):List[Activity] = {
    if (inActivities.tail.isEmpty) List(sortEffort(inActivities.head))
    else sortEffort(inActivities.head) :: sortActivities(inActivities.tail)
  }

  def sortEffort(inActivity:Activity):Activity = {
    val original = inActivity.children(0).effortDays
    val revised  = inActivity.children(1).effortDays
    val revisedEffort = inActivity.children(1)
    val actual   = inActivity.children(2).effortDays
    val actualEffort = inActivity.children(2)
    val forecast = inActivity.children(3).effortDays
    val forecastEffort = inActivity.children(3)
    val surplusEffort:Effort = new(Effort) ("Surplus" , revised-(actual+forecast))
    val forecastEffortWithinBudget:Effort = new (Effort) ("forecast in budget", revised-actual)
    val forecastEffortOverBudget:Effort = new (Effort) ("forecast over budget", actual+forecast-revised)
    val alreadyOverBudget:Effort = new(Effort) ("over budget", revised-actual)
    if (actual+forecast <= revised )
      {if (forecast==0) {if (surplusEffort.effortDays==0) new(Activity) (inActivity.name,List(actualEffort))
                        else new(Activity) (inActivity.name,List(actualEffort,surplusEffort))}
      else {if (surplusEffort.effortDays==0) new(Activity) (inActivity.name,List(actualEffort,forecastEffort))
            else new(Activity) (inActivity.name,List(actualEffort,forecastEffort,surplusEffort))}}
    else if (actual <= revised && actual+forecast>revised)
      {new (Activity)(inActivity.name, List(actualEffort,forecastEffortWithinBudget,forecastEffortOverBudget))}
    else {if (forecast==0) new (Activity) (inActivity.name,List(revisedEffort,alreadyOverBudget))
          else new (Activity) (inActivity.name,List(revisedEffort,alreadyOverBudget,forecastEffortOverBudget))}
  }

  def displayProjList(perPage:Int,pageNo:Int,searchTerm:String) = Action {
    val effortDal = new dal.EffortDal {}
    val projectList = sortProjects(effortDal.getProjects(searchTerm))
    implicit val formats = Serialization.formats(NoTypeHints)
    val starter = "{"+q("name")+":"+q("Projects")+","+q("children")+":"
    val endJSON = (starter+write(projectList)+"}")
    Ok(views.html.main("Project Details")(views.html.budgetViewer(endJSON)))
  }
    
}

package dal

import sqlest._
import domain.Project
import domain.Activity
import domain.Effort

/**
 * @author RodgersonM
 */
class ProjectTable(alias: Option[String]) extends Table("project", alias) {
  val projectCode = column[String]("procde")
  val projectDesc = column[String]("desc")
  val projectStatus = column[String]("status")
}

object ProjectTable extends ProjectTable(None)

class EffortTable(alias: Option[String]) extends Table("effort", alias) {
  val name = column[String]("project_code")
  val effortType = column[String]("effort_type")
  val original = column[Double]("effort_original")
  val revised = column[Double]("effort_revised")
  val actual = column[Double]("effort_actual")
  val forecast = column[Double]("effort_forecast")
}

object EffortTable extends EffortTable(None)

trait EffortDal extends SqlestDb {

  def getProjects(searchTerm: String) : List[Project] = {
      select
      .from(EffortTable)
          .innerJoin(ProjectTable).on(EffortTable.name===ProjectTable.projectCode)
        .where(ProjectTable.projectStatus==="ACTIVE")
      .orderBy (EffortTable.name)
      .extractAll(projectExtractor)
  }
  
    lazy val projectExtractor = extract[Project](
    name = EffortTable.name,
    projectDesc = ProjectTable.projectDesc,
    children = activityExtractor.asList)
    .groupBy(EffortTable.name)

    lazy val activityExtractor = extract[Activity] (
      name = EffortTable.effortType,
      children = effortExtractor
    )

    lazy val effortExtractor = extract[List[Effort]] (
      originalEffortExtractor,
      revisedBudgetExtractor,
      actualEffortExtractor,
      forecastEffortExtractor
    )

    lazy val originalEffortExtractor = extract[Effort] (
      name = coalesce("original","original").as("name1"), effortDays=EffortTable.original)
  lazy val revisedBudgetExtractor = extract[Effort] (
      name = coalesce("revised","revised").as("name2"), effortDays = EffortTable.revised)
    lazy val actualEffortExtractor = extract[Effort] (
      name = coalesce("actual","actual").as("name3"), effortDays = EffortTable.actual)
    lazy val forecastEffortExtractor = extract[Effort] (
      name = coalesce("forecast","forecast").as("name4"), effortDays = EffortTable.forecast)


}


  
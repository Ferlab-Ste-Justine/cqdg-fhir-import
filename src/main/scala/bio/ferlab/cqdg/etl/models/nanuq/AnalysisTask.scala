package bio.ferlab.cqdg.etl.models.nanuq

import ca.uhn.fhir.model.api.annotation.ResourceDef
import org.hl7.fhir.r4.model.Task.{TaskIntent, TaskPriority, TaskStatus}
import org.hl7.fhir.r4.model.{DateTimeType, Task}

import java.util.Date

@ResourceDef(name = "Task")
class AnalysisTask extends Task {

}

object AnalysisTask{
  def apply(): AnalysisTask = {
    val t = new AnalysisTask()
    t.setStatus(TaskStatus.COMPLETED)
    t.setIntent(TaskIntent.ORDER)
    t.setPriority(TaskPriority.ROUTINE)
    t.setAuthoredOnElement(new DateTimeType(new Date()))
    t
  }
}
package models

import scala.io.Source
import java.io._
import scala.util.Try

// my custom written models
import models.Task
import models.State
import models.Serializer
import models.Task.given

final case class  StateModel private  (
    nextID: Int,
    tasks: Map[Int, Task]
){
    def createTask(taskDesc: String): Option[String] = 
        if (taskDesc.isEmpty()) None 
        else
            val task = Task(nextID, taskDesc)
            val newState = new StateModel(nextID + 1, tasks + (nextID  -> task))
            saveTasks(newState.tasks, s"Task added successfully (ID: ${newState.nextID - 1})")
    
    def listTasks(taskState: String)(using s: Serializer[Task]): Option[String] = 
        val allTasks = StateModel.getCurrentTasks
        taskState match
            case "doing" => Some(s.display(allTasks.filter((k,v) => v.state == State.InProgress )))
            case "todo"  => Some(s.display(allTasks.filter((k,v) => v.state == State.ToDo )))
            case "done"  => Some(s.display(allTasks.filter((k,v) => v.state == State.Done )))
            case ""      => Some(s.display(allTasks))
            
        
    def markTask(toState: String, taskId: Int): Option[String] = 
            val allTasks = StateModel.getCurrentTasks
            
            allTasks.get(taskId).map((v) => Task.updateTaskState(v, toState) ) match
                case Some(newTask) => saveTasks(allTasks.updated(taskId, newTask), "Updated successfully")
                case None => Some("Error: No task with the given ID")
            

    def updateTask(taskId: Int, newDesc: String): Option[String] = 
        val allTasks = StateModel.getCurrentTasks
        allTasks.get(taskId).map(t => Task.updateTaskDesc(t, newDesc) ) match {
            case Some(newTask) => saveTasks(allTasks.updated(taskId, newTask), "Updated successfully")
            case None => Some("Error: No task with the given ID")
        }
    
    def deleteTask(taskId: Int): Option[String] = 
        val allTasks = StateModel.getCurrentTasks
        StateModel.getCurrentTasks.get(taskId) match {
            case Some(_) => saveTasks(allTasks - taskId, "Deleted successfully")
            case None => Some("Error: No task with the given ID")
        }

    private def saveTasks(myTasks: Map[Int, Task], success: String): Option[String] = 
        StateModel.saveTasks(myTasks) match {
                case scala.util.Success(_) => Some(success)
                case scala.util.Failure(e) => Some(s"Error saving state: ${e.getMessage}")
            }


        
}


object StateModel{

    private val file: File =  File(System.getProperty("user.dir") ++ "/db.json")

    def apply(): StateModel = 

        if(!file.exists() || file.isDirectory())
            file.createNewFile()

        val prevTasks =  getCurrentTasks 
        if(prevTasks.isEmpty)
            new StateModel(1, Map.empty)
        else 
            new StateModel(prevTasks.keys.max + 1, prevTasks)

    private def saveTasks(tasks: Map[Int, Task])(using s: Serializer[Task]): Try[Unit] = 
        val dp = new PrintWriter(file)
        Try { 
            dp.write(tasks.values.map{task => s.serializeToString(task)}.mkString("[", ",", "]"))
            dp.flush()
            dp.close()
        }

    private def getCurrentTasks(using s: Serializer[Task]): Map[Int, Task]= 

        val source = Source.fromFile(file)
        val res = s.serializeBack( source
                .getLines()
                .foldLeft("")
                ((acc,line) => acc + line + "\n") 
        )
        source.close()
        res
}

        
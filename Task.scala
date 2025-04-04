package models

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.matching.Regex





final case class Task private (id: Int, desc: String, state: State, timeCreated: String, updated: Option[String])
object Task {
        def apply(id: Int,task: String): Task =
            Task(id,task, State.ToDo, updateTime(), None)
        
        def updateTime(): String = 
            val currentDateTime  = LocalDateTime.now()
            val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")
            currentDateTime.format(formatter)

        def updateTaskState(t: Task, newState:String): Task = 
            newState match {
                case "todo" => Task(t.id, t.desc, State.ToDo, t.timeCreated, Some(updateTime()))
                case "done" => Task(t.id, t.desc, State.Done, t.timeCreated, Some(updateTime()))
                case "doing" => Task(t.id, t.desc, State.InProgress, t.timeCreated, Some(updateTime()))
            }

        def updateTaskDesc(t: Task, newDesc: String): Task = 
            new Task(t.id, newDesc, t.state, t.timeCreated, t.updated)


        given Serializer[Task] with 
            def serializeToString(task: Task): String = 
                s"""\n
    {
        "id": ${task.id},
        "description": "${task.desc}",
        "state": "${task.state}",
        "timeCreated": "${task.timeCreated}",
        "timeUpdated": "${ task.updated.getOrElse("")}"
    }
"""

            def serializeBack(value: String): Map[Int,Task] = 
                if(value.isEmpty()) Map()
                else
                    val pattern: Regex = "[\\(\\{]([^)}]+)[\\)\\}]".r
                    pattern.findAllIn(value)
                    .matchData.map(_.group(1))
                    .toList
                    .map(obj => obj.trim())
                    .map(obj => obj.split(","))
                    .map{arr => 
                        val pf = (s: String) => s.slice(1, s.length ) 
                        val id = arr.find(_.trim().startsWith("\"id\":")).map(_.split(":")(1).trim().toInt).getOrElse(0)
                        val desc = arr.find(_.trim().startsWith("\"description\":")).map(obj =>  pf(obj.split(":")(1)) ).getOrElse("")
                        val state = arr.find(_.trim().startsWith("\"state\":")).map( obj =>  pf(obj.split(":")(1))  ).getOrElse("")
                        val timeCreated = arr.find(_.trim().startsWith("\"timeCreated\":")).map( obj =>  pf(obj.split("\"timeCreated\":")(1))  ).getOrElse("")
                        val timeUpdated = arr.find(_.trim().startsWith("\"timeUpdated\":")).map( obj =>  pf(obj.split("\"timeUpdated\":")(1))  ).getOrElse("")
                        new Task(id, desc, State.values.find(_.toString == state).getOrElse(State.InProgress), timeCreated, Some(timeUpdated))
                    }.map(task => (task.id -> task))
                    .toMap
            
            def display(value: Map[Int, Task]): String = 
                value.map((k,v) => 
                    s"ID:${v.id} Task: ${v.desc} State: ${v.state} timeCreated: ${v.timeCreated} timeUpdated: ${v.updated.getOrElse("None")}")
                    .mkString("\n")

    }


enum State { 
        case Done 
        case InProgress
        case ToDo
    }
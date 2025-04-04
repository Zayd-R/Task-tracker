import models.StateModel


object MyApp  {


    def main(args: Array[String]): Unit = 

        val usage: String =
            """
        task-cli add ["Your task"]
        task-cli update ["task ID"] ["new task"]
        task-cli delete ["task ID"]
        task-cli mark [done, doing, todo] ["task ID"]
        task-cli list [done, todo, doing]
        task-cli list
        """


        def parseArg(list: List[String], state: StateModel): Option[String] = 
            list match
                case "add" :: taskDesc :: Nil => state.createTask(taskDesc)
                case "update" :: id :: newDesc :: Nil => state.updateTask(id.toInt, newDesc)
                case "delete" :: id :: Nil => state.deleteTask(id.toInt)
                case "mark" :: stringState :: id :: Nil => state.markTask(stringState, id.toInt) 
                case "list" :: Nil =>  state.listTasks("")
                case "list" :: stringState :: Nil => state.listTasks(stringState)
                case _ => Some(usage)
        
        println(parseArg(args.toList, StateModel()).getOrElse(usage))


}
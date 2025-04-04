CLI Taks tracker using scala 3.3.5

# Task CLI Application

This is a simple command-line interface (CLI) application for managing tasks. You can add, update, delete, mark, and list tasks directly from the terminal.

## Features

- **Add a Task:** Add a new task with a description.
- **Update a Task:** Update the description of an existing task.
- **Delete a Task:** Remove a task by its ID.
- **Mark a Task:** Mark a task as "in progress" or "done."
- **List Tasks:** List all tasks or filter them by status (e.g., `todo`, `in progress`, `done`).

## Installation

1. **Clone the repository:**

   ```bash
   git clone https://github.com/Zayd-R/Task-tracker/
   cd Task-tracker

2. **Compile the source code:**
    ```bash
    chmod +x runs
    ./runs
    ```
    Or    
   ```bash
   scalac Task-tracker.scala Task.scala StateModel.scala Serializer.scala
   
3. **Run the application:**
    ```bash
   scala MyApp
   ```
## Usage
```bash
# Adding a new task
scala MyApp add "Buy groceries"
# Output: Task added successfully (ID: 1)

# Updating a task
scala MyApp update 1 "Buy groceries and cook dinner"
# Output: Task updated successfully (ID: 1)

# Deleting a task
scala MyApp delete 1
# Output: Task deleted successfully (ID: 1)

# Marking a task as in progress
scala MyApp mark doing 1
# Output: Task marked as in progress (ID: 1)

# Marking a task as done
scala MyApp mark done 1
# Output: Task marked as done (ID: 1)

# Listing all tasks
scala MyApp list
# Output: List of all tasks

# Listing tasks by status
scala MyApp list todo
scala MyApp list doing
scala MyApp list done

```

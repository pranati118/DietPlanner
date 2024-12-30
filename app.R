library(shiny)
library(ggplot2)
library(caret)

set.seed(123)
example_data <- data.frame(
  age = sample(18:60, 100, replace = TRUE),
  weight = sample(50:100, 100, replace = TRUE),
  height = sample(150:190, 100, replace = TRUE),
  activity_level = sample(c("Sedentary", "Active", "Highly Active"), 100, replace = TRUE),
  goal = sample(c("Weight Loss", "Muscle Gain", "Maintenance"), 100, replace = TRUE),
  calories_intake = sample(1800:3000, 100, replace = TRUE)
)

model <- train(calories_intake ~ age + weight + height + activity_level + goal, 
               data = example_data, 
               method = "lm")

ui <- fluidPage(
  titlePanel("Diet and Exercise Planner"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Enter Your Health Data"),
      
      numericInput("age", "Age", value = 30, min = 18, max = 100),
      numericInput("weight", "Weight (kg)", value = 70, min = 30, max = 200),
      numericInput("height", "Height (cm)", value = 170, min = 100, max = 250),
      selectInput("activity", "Activity Level", choices = c("Sedentary", "Active", "Highly Active")),
      selectInput("goal", "Fitness Goal", choices = c("Weight Loss", "Muscle Gain", "Maintenance")),
      actionButton("generate_plan", "Generate Diet & Exercise Plan"),
      hr(),
      
      numericInput("calories_intake", "Calories Consumed", value = 0, min = 0, max = 5000),
      numericInput("exercise_time", "Exercise Time (min)", value = 0, min = 0, max = 180),
      actionButton("log_progress", "Log Progress")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Diet & Exercise Plan", 
                 h4("Personalized Diet Plan"),
                 verbatimTextOutput("diet_plan"),
                 h4("Personalized Exercise Plan"),
                 verbatimTextOutput("exercise_plan")),
        
        tabPanel("Progress Tracking",
                 h4("Calorie Intake and Exercise History"),
                 plotOutput("calorie_plot"),
                 plotOutput("exercise_plot"),
                 h4("Goal Progress"),
                 plotOutput("goal_progress"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$diet_plan <- renderText({
    req(input$generate_plan)
    
    user_data <- data.frame(
      age = input$age,
      weight = input$weight,
      height = input$height,
      activity_level = input$activity,
      goal = input$goal
    )
    
    predicted_calories <- predict(model, newdata = user_data)
    
    paste("Based on your health data, your daily caloric intake should be around:", predicted_calories, "calories.")
  })
  
  output$exercise_plan <- renderText({
    req(input$generate_plan)
    paste("Recommended exercises based on your fitness goal:", 
          if(input$goal == "Weight Loss") "Cardio 3 days/week, Strength 2 days/week", 
          if(input$goal == "Muscle Gain") "Strength 4 days/week, Cardio 2 days/week", 
          if(input$goal == "Maintenance") "Balanced cardio and strength 3-4 days/week")
  })
  
  progress_data <- reactiveVal(data.frame(Date = character(), Calories = numeric(), ExerciseTime = numeric()))
  
  observeEvent(input$log_progress, {
    new_entry <- data.frame(Date = Sys.Date(), Calories = input$calories_intake, ExerciseTime = input$exercise_time)
    progress_data(rbind(progress_data(), new_entry))
  })
  
  # Visualization: Calorie intake
  output$calorie_plot <- renderPlot({
    req(progress_data())
    ggplot(progress_data(), aes(x = Date, y = Calories)) +
      geom_line() + labs(title = "Calorie Intake Over Time", x = "Date", y = "Calories")
  })
  
  output$exercise_plot <- renderPlot({
    req(progress_data())
    ggplot(progress_data(), aes(x = Date, y = ExerciseTime)) +
      geom_bar(stat = "identity") + labs(title = "Exercise Time Over Time", x = "Date", y = "Minutes")
  })
  
  output$goal_progress <- renderPlot({
    req(input$goal)
    goal_progress <- if(input$goal == "Weight Loss") {
      input$weight - 2 # Example: user lost 2 kg
    } else {
      input$weight
    }
    
    ggplot(data.frame(Goal = c("Target", "Current"), Value = c(70, goal_progress)), aes(x = Goal, y = Value, fill = Goal)) +
      geom_bar(stat = "identity") +
      labs(title = "Goal Progress", y = "Weight (kg)")
  })
}

shinyApp(ui = ui, server = server)

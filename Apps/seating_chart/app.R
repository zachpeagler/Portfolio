# Seating Chart

# dependencies
library("shiny")
library("tidyverse")

# UI
ui <- fluidPage(
  titlePanel("Seating Chart"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File", accept = c(".csv")),
      numericInput("nrows", "Number of Rows", min = 1, max = 20, value = 5),
      numericInput("ncols", "Number of Columns", min = 1, max = 20, value = 5),
      actionButton("assign_seats", "Assign Seats")
    ),
    mainPanel(
      plotOutput("seating_chart"),  # Placeholder for the seating chart
      verbatimTextOutput("grid_print")  # For debugging: print grid to see seat assignments
    )
  )
)

server <- function(input, output, session) {
  seating <- reactiveValues(data = NULL, chart = NULL, colors = NULL)
  
  observeEvent(input$file1, {
    # Read the uploaded CSV file and create a seat column
    seating$data <- read.csv(input$file1$datapath) %>%
      mutate(seat = c(1:nrow(.)))  # Create a seat column
    
    # Debugging: Check the data loaded from the CSV file
    print("Data loaded from CSV:")
    print(seating$data)
  })
  
  observeEvent(input$assign_seats, {
    req(seating$data)  # Ensure data is available
    
    # Get student information
    students <- seating$data
    
    # Create an empty grid for seat assignments
    num_rows <- input$nrows
    num_cols <- input$ncols
    grid <- matrix(NA, nrow = num_rows, ncol = num_cols)  # Initialize an empty grid
    colors <- rainbow(length(unique(students$group)))  # Generate colors for each group
    
    # --- Part 1: Assign Front Row Students ---
    front_row_students <- students %>% filter(frontRow == TRUE)
    
    # Assign front row seats (1st row)
    for (i in seq_len(nrow(front_row_students))) {
      if (i <= num_cols) {
        grid[1, i] <- front_row_students$name[i]
      }
    }
    
    # Debugging: Check grid after assigning front-row students
    print("Grid after assigning front-row students:")
    print(grid)
    
    # --- Part 2: Assign Remaining Students ---
    other_students <- students %>% filter(frontRow == FALSE)  # Non-front-row students
    remaining_seats <- which(is.na(grid), arr.ind = TRUE)  # Get remaining empty seats
    
    # Shuffle the order of non-front-row students
    shuffled_students <- sample(other_students$name)
    
    # Ensure we do not exceed the available remaining seats
    num_students_to_seat <- min(length(shuffled_students), nrow(remaining_seats))
    
    # Assign remaining students to the empty seats
    for (i in seq_len(num_students_to_seat)) {
      seat <- remaining_seats[i, ]
      grid[seat[1], seat[2]] <- shuffled_students[i]
    }
    
    # Store the seating chart
    seating$chart <- grid  # Store grid to reactive values
    
    # Debugging: Check grid after assigning remaining students
    print("Grid after assigning remaining students:")
    print(grid)
    
    # Create a reactive color map for the students based on groups
    seating$colors <- setNames(colors, unique(students$group))
  })
  
  output$seating_chart <- renderPlot({
    req(seating$chart)  # Ensure the chart is ready
    
    # Define grid dimensions
    num_rows <- nrow(seating$chart)
    num_cols <- ncol(seating$chart)
    
    # Set up the plot with no points initially
    plot(1:num_cols, 1:num_rows, type = "n", xlim = c(0, num_cols + 1), ylim = c(0, num_rows + 1), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    grid()
    
    # Prepare a variable to keep track of student positions for symbol and color
    student_positions <- data.frame(x = numeric(0), y = numeric(0), name = character(0), group = character(0))
    
    # Add seat labels (names)
    for (r in 1:num_rows) {
      for (c in 1:num_cols) {
        if (!is.na(seating$chart[r, c])) {
          # Get the student name and group for coloring
          student_name <- seating$chart[r, c]
          student_group <- seating$data$group[match(student_name, seating$data$name)]
          
          # Store the position for later plotting
          student_positions <- rbind(student_positions, data.frame(x = c, y = num_rows - r + 1, name = student_name, group = student_group))
        }
      }
    }
    
    # Plot each student with appropriate symbol and color
    for (i in 1:nrow(student_positions)) {
      seat_color <- seating$colors[student_positions$group[i]]
      # Use different symbols for front row students
      if (student_positions$name[i] %in% seating$data$name[seating$data$frontRow == TRUE]) {
        # Plot with a star for front row students
        points(student_positions$x[i], student_positions$y[i], pch = 8, col = seat_color, cex = 2)  # Star symbol
      } else {
        # Plot regular point for others
        points(student_positions$x[i], student_positions$y[i], pch = 21, col = seat_color, bg = seat_color, cex = 2)  # Circle symbol
      }
      
      # Add the text label
      text(student_positions$x[i], student_positions$y[i], student_positions$name[i], cex = 0.8, col = "black")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
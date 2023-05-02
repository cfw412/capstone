
if (!require(rsconnect)){
  install.packages('rsconnect')
}

if (!require(shiny)){
  install.packages('shiny')
}
library(shiny)

if (!require(tidyverse)){
  install.packages('tidyverse')
}
library(tidyverse)

if (!require(DT)){
  install.packages('DT')
}
library(DT)

if (!require(htmlwidgets)){
  install.packages('htmlwidgets')
}
library(htmlwidgets)

if (!require(readxl)){
  install.packages('readxl')
}
library(readxl)

if (!require(GA)){
  install.packages('GA')
}
library(GA)

if (!require(httr)){
  install.packages('httr')
}
library(httr)

if (!require(jsonlite)){
  install.packages('jsonlite')
}
library(jsonlite)

### Import and Clean Data ###

# Steps to pull files from repo
# this could go in a function to de-duplicate code
owner <- "cfw412"
repo <- "capstone"
path <- "Data"

# Construct the URL for the API endpoint with the main branch
url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path, "?ref=main")

# Make a GET request to the API endpoint and convert the response to a data frame
# response <- GET(url)
# data <- content(response, "text")
# df <- fromJSON(data)
# 
# ga_salary_data <- data.frame()
# 
# ga_salary_data_url = df %>%
#   filter(name == "2022_MLS_GoalsAdded_and_Salary_Data.xlsx") %>%
#   select('download_url') %>%
#   pull()
# 
# # Steps to pull files from a different folder in the repo
# path <- "Data/MLS_Player_Season_Stats"
# 
# url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path, "?ref=main")
# 
# response <- GET(url)
# data <- content(response, "text")
# df <- fromJSON(data)
# 
# temp_file <- tempfile()
# download.file(ga_salary_data_url, temp_file, mode = "wb")

## Comment all lines above and up to the library import statements
## Uncomment the below line and add files to your local machine 
temp_file = "/Users/charleswhorton/Desktop/GradSchool/Capstone/capstone/Data/2022_MLS_GoalsAdded_and_Salary_Data.xlsx"

Player_Goals_Added <- read_excel(temp_file, sheet = "Player_Goals_Added")
Player_Salary = read_excel(temp_file, sheet = "Player_Salary") %>%
  select(-c("Season"))

MLSPA_Player_Salary_202209 =
  read_csv("http://s3.amazonaws.com/mlspa/9_2_2022-Roster-Freeze-Salary-List.csv?mtime=20221017131703") %>%
  select(-c("Nickname")) %>%
  mutate(`2022 Base Salary` = floor(as.numeric(gsub("[$,]", "", `2022 Base Salary`)))) %>%
  mutate(`2022 Guar. Comp.` = floor(as.numeric(gsub("[$,]", "", `2022 Guar. Comp.`)))) %>%
  mutate(Source = 9)

MLSPA_Player_Salary202204 = 
  # read_csv("http://s3.amazonaws.com/mlspa/2022_salary_list_4.15__for_site.csv?mtime=20220517164257") %>%
  read_csv("/Users/charleswhorton/Desktop/GradSchool/Capstone/capstone/Data/MLSPA_Player_Salary_202204.csv") %>%
  mutate(`2022 Base Salary` = floor(as.numeric(gsub("[$,]", "", `2022 Base Salary`)))) %>%
  mutate(`2022 Guar. Comp.` = floor(as.numeric(gsub("[$,]", "", `2022 Guar. Comp.`)))) %>%
  mutate(Source = 4)

MLSPA_Player_Salary202204 = rename(MLSPA_Player_Salary202204, "Position" = "Playing Position")

MLSPA_Player_Salary =
  union(MLSPA_Player_Salary_202209, MLSPA_Player_Salary202204)
  # MLSPA_Player_Salary202204

MLSPA_Player_Salary = MLSPA_Player_Salary %>%
  mutate(Player = paste(MLSPA_Player_Salary$'First Name', MLSPA_Player_Salary$'Last Name', sep = " "))

MLSPA_Player_Salary = MLSPA_Player_Salary %>%
  relocate(c("Player", "Club", "Position", "2022 Base Salary", "2022 Guar. Comp."))

MLSPA_Player_Salary = rename(MLSPA_Player_Salary,
                             "Team" = "Club",
                             "Base Salary" = "2022 Base Salary",
                             "Guaranteed Compensation" = "2022 Guar. Comp.") %>%
  select(-c("Last Name", "First Name", Team, Position))

MLSPA_Player_Salary = MLSPA_Player_Salary %>%
  arrange(desc(Player)) %>%
  group_by(Player) %>%
  mutate(Rank = row_number(desc(Source))) %>%
  filter(Rank == 1)

Salary_Data =
  full_join(Player_Salary, MLSPA_Player_Salary %>% select(-c(Source, Rank)), by = 'Player')

Player_Data = Player_Goals_Added %>% 
  left_join(Salary_Data, by = "Player")

Player_Data = Player_Data %>% 
  mutate('Guaranteed Compensation.x' 
         = coalesce(Player_Data$'Guaranteed Compensation.x',Player_Data$'Guaranteed Compensation.y')) %>%
  mutate('Base Salary.x' 
         = coalesce(Player_Data$'Base Salary.x',Player_Data$'Base Salary.y')) %>%
  mutate('Position.x' 
         = coalesce(Player_Data$'Position.x',Player_Data$'Position.y')) %>%
  mutate('Team.x' 
         = coalesce(Player_Data$'Team.x',Player_Data$'Team.y')) %>%
  select(-c("Team.y",
            "Position.y",
            "Base Salary.y", 
            "Guaranteed Compensation.y")) %>% 
  rename("Position" = "Position.x",
         "Team" = "Team.x",
         "Base_Salary" = "Base Salary.x",
         "Guaranteed_Compensation" = "Guaranteed Compensation.x",
         "Goals_Added" = "Goals Added") %>% 
  drop_na(Base_Salary)

Player_Data %>%
  filter(is.na(Base_Salary))

Player_Data = data.frame(Player_Data %>% 
                           mutate(Log_Goals_Added = 
                                    log10(Goals_Added+(abs(min(Player_Data$Goals_Added))+1.01)))) %>%
  mutate(across('Log_Goals_Added', round, 4))

### Shiny App ###

shinyApp(
  
  # UI function
  ui = fluidPage(
    
    # Title
    titlePanel("Player Optimization App"),
    
    # Set the input fields
    sidebarLayout(
      sidebarPanel(
        h4("Select the number of players at each position:"),
        
        # Numeric inputs for minimum and maximum players per position
        uiOutput("position_inputs"),
        
        numericInput(inputId = "max_salary", 
                     label = "What is the salary limit?", 
                     value = 750000,
                     min = 0),
        actionButton(inputId = "btn_run", 
                     label = "Run")
      ),
      
      # Set the output display
      mainPanel(
        verbatimTextOutput("input_values")
        )
      ),
    
    fluidRow(
      column(12, tableOutput('test_table')),
      column(width = 6, 
             uiOutput("numericInputs")),
      column(12, dataTableOutput('table'))
    ),
  ),
  
  # Server function 
  server = function(input, output, session) {
    
    # reactive value for the input number
    input_num = reactive({input$input_number})
    
    # reactive value for the max salary
    max_salary = reactive({input$max_salary})

    output$position_inputs = renderUI({

      position_list = c(unique(Player_Data$Position)) # This is duplicated below.

      selected_position_values_list = 
        lapply(position_list,
               function(value) {
                 div(style = paste0("display: inline-block; width: ", 
                                    length(position_list) * 9, 
                                    "px; text-align: center;"),
                     numericInput(paste0("input_", value),
                                  label = value,
                                  value = 0,
                                  min = 0)
                     )
                 })
      tagList(selected_position_values_list)
      })
    
    output$numericInputs = renderUI({
      tags$style(type = "text/css", 
                 "#input_number { width: 125px; }", 
                 "#max_salary { width: 125px; }"
      )
    })
    
    # display data on 'btn_run' click
    observeEvent(input$btn_run, {
      
      position_list = c(unique(Player_Data$Position)) # This is duplicated below
      
      numeric_input_position_objects_names = paste0("input_", position_list)
      
      # Define a function to get the input values
      get_input_value = function(input_name) {
        input <- input[[input_name]]
        return(input)
      }
      
      # Get the input values for each input name
      numeric_input_position_values = lapply(numeric_input_position_objects_names, 
                                             function(input_name) {
                                               input <- input[[input_name]]
                                               return(input)
                                             })
      
      # remove positions from input values where the value is 0
      position_values = data.frame(position = position_list,
                              input_value = 
                                unlist(numeric_input_position_values)) %>%
        filter(input_value > 0)
      
      # turn the position_values dataframe into a table() format
      positions_required = position_values %>%
        spread(position, input_value, fill = 0)
      
      ### setting the population equal to the number of players entered
      sel_pop = sum(position_values$input_value)
      
      ### setting the max salary equal to the input value
      max_salary = max_salary()
      
      # Filter the Player_Data dataframe on the players in the selected positions
      Filtered_Player_Data = 
        Player_Data %>%
        filter(Position %in% position_values$position) %>%
        select(c('Player',
                 'Position',
                 'Guaranteed_Compensation',
                 'Log_Goals_Added'))
      
      ### GA Functions
      ### Initialize population
      myInit = function(k){
        
        function(GA){
          mat = matrix(0, ncol = GA@nBits, nrow = GA@popSize)
          
          for(i in seq_len(GA@popSize))
            mat[i, sample(GA@nBits, k)] <- 1
          
          for (i in 1:length(position_values$input_value)) {
            if (position_values$input_value[i] > 0) {
              ind = which(Filtered_Player_Data$Position == names(position_values$input_value)[i])
              mat[, ind[sample(length(ind), 1)]] <- 1
            }
          }
          
          mat
        }
      }
      
      ### Crossover function
      myCrossover = function(GA, parents){

        parents = GA@population[parents,] %>%
          apply(1, function(x) which(x == 1)) %>%
          t()

        parents_diff = list("vector", 2)
        parents_diff[[1]] = setdiff(parents[2,], parents[1,])
        parents_diff[[2]] = setdiff(parents[1,], parents[2,])

        children_ind = list("vector", 2)
        for(i in 1:2){
          k = length(parents_diff[[i]])
          change_k = sample(k, sample(ceiling(k/2), 1))
          children_ind[[i]] = if(length(change_k) > 0){
            c(parents[i, -change_k], parents_diff[[i]][change_k])
          } else {
            parents[i,]
          }
        }

        children = matrix(0, nrow = 2, ncol = GA@nBits)
        for(i in 1:2)
          children[i, children_ind[[i]]] <- 1

        list(children = children, fitness = c(NA, NA))
      }
      
      ### Mutation function
      myMutation <- function(GA, parent){
        ind <- which(GA@population[parent,] == 1)
        n_change <- sample(sel_pop, 1)
        ind[sample(length(ind), n_change)] = 
          sample(setdiff(seq_len(GA@nBits), ind), n_change)
        parent <- integer(GA@nBits)
        parent[ind] <- 1
        
        parent
      }
      
      ### Fitness function (Objective function)
      fitness=function(x)
      {
        current_goals_added=x%*%Filtered_Player_Data$Log_Goals_Added
        current_salary=x%*%Filtered_Player_Data$Guaranteed_Compensation
        positions = Filtered_Player_Data$Position[x == 1] # positions of selected players
        positions_count = table(positions) # count of players in each position
        
        if(current_salary>max_salary) {
          return(0) # Penalty for exceeding the max salary
        }
        else if (all(positions_count == positions_required)) {
          return(current_goals_added)
        }
        else {
          return(0)
        }
      }
      
      GA <- ga(
        type = "binary",
        fitness = fitness,
        nBits = nrow(Filtered_Player_Data),
        population = myInit(sel_pop),
        crossover = myCrossover,
        mutation = myMutation,
        run = 125,
        pmutation = .25,
        maxiter = 300,
        popSize = 30,
        monitor = FALSE
      )

      output$table = renderDataTable(Filtered_Player_Data[GA@solution == 1,] %>%
                                       select(c('Player',
                                                'Position',
                                                'Log_Goals_Added',
                                                'Guaranteed_Compensation')),
                                     options = list(
                                       searching = FALSE,
                                       lengthChange = FALSE,
                                       dom = "lfrt"
                                     ))
      
    })
  }
)



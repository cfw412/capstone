
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

# # Steps to pull files from repo
# # this could go in a function to de-duplicate code
# owner <- "cfw412"
# repo <- "capstone"
# path <- "Data" 
# 
# # Construct the URL for the API endpoint with the main branch
# url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path, "?ref=main")
# 
# # Make a GET request to the API endpoint and convert the response to a data frame
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
  read_csv("http://s3.amazonaws.com/mlspa/2022_salary_list_4.15__for_site.csv?mtime=20220517164257") %>%
  mutate(`2022 Base Salary` = floor(as.numeric(gsub("[$,]", "", `2022 Base Salary`)))) %>%
  mutate(`2022 Guar. Comp.` = floor(as.numeric(gsub("[$,]", "", `2022 Guar. Comp.`)))) %>%
  mutate(Source = 4)

MLSPA_Player_Salary202204 = rename(MLSPA_Player_Salary202204, "Position" = "Playing Position")

MLSPA_Player_Salary =
  union(MLSPA_Player_Salary_202209, MLSPA_Player_Salary202204)

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

Player_Data <- data.frame(Player_Data %>% 
  mutate(Log_Goals_Added = log10(Goals_Added+(abs(min(Player_Data$Goals_Added))+1.01))))

# ### GA Functions
# 
# ### Initialize population
# myInit <- function(k){
# 
#   function(GA){
#     m <- matrix(0, ncol = GA@nBits, nrow = GA@popSize)
# 
#     for(i in seq_len(GA@popSize))
#       m[i, sample(GA@nBits, k)] <- 1
# 
#     m
#   }
# }
# 
# 
# ### Crossover function
# myCrossover <- function(GA, parents){
# 
#   parents <- GA@population[parents,] %>%
#     apply(1, function(x) which(x == 1)) %>%
#     t()
# 
#   parents_diff <- list("vector", 2)
#   parents_diff[[1]] <- setdiff(parents[2,], parents[1,])
#   parents_diff[[2]] <- setdiff(parents[1,], parents[2,])
# 
#   children_ind <- list("vector", 2)
#   for(i in 1:2){
#     k <- length(parents_diff[[i]])
#     change_k <- sample(k, sample(ceiling(k/2), 1))
#     children_ind[[i]] <- if(length(change_k) > 0){
#       c(parents[i, -change_k], parents_diff[[i]][change_k])
#     } else {
#       parents[i,]
#     }
#   }
# 
#   children <- matrix(0, nrow = 2, ncol = GA@nBits)
#   for(i in 1:2)
#     children[i, children_ind[[i]]] <- 1
# 
#   list(children = children, fitness = c(NA, NA))
# }
# 
# ### Mutation function
# myMutation <- function(GA, parent){
#   
#   print(nrow(nBits))
#   
#   ind <- which(GA@population[parent,] == 1)
#   n_change <- sample(sel_pop, 1)
#   ind[sample(length(ind), n_change)] <- sample(setdiff(seq_len(GA@nBits), ind), n_change)
#   parent <- integer(GA@nBits)
#   parent[ind] <- 1
# 
#   parent
# }
# 
# 
# ### Fitness function (Objective function)
# fitness=function(x)
# {
#   print(nrow(x))
#   print(nrow(Player_Data))
#   
#   current_goals_added=x%*%Player_Data$Log_Goals_Added
#   current_salary=x%*%Player_Data$Guaranteed_Compensation
#   if(current_salary>max_salary)
#   {
#     return(0) # Return a very low fitness value for solutions that violate the salary constraint
#   }
#   else
#   {
#     return(current_goals_added)
#   }
# }

### Shiny App ###

shinyApp(
  
  # UI function
  ui = fluidPage(
    
    # Title
    titlePanel("Player Optimization App"),
    
    # Set the input fields
    sidebarLayout(
      sidebarPanel(
        numericInput(inputId = "input_number", 
                     label = "How many players do you want to return?", 
                     value = 0),
        numericInput(inputId = "max_salary", 
                     label = "What is the salary limit?", 
                     value = 0),
        actionButton(inputId = "btn_run", 
                     label = "Run")
      ),
      
      # Set the output display
      mainPanel(
        verbatimTextOutput(outputId = "output_result"))
    ),
    
    fluidRow(
      column(12, dataTableOutput('table')))
  ),
  
  # Server function 
  server = function(input, output) {
    
    
    # reactive value for the input number
    input_num = 
      reactive({input$input_number})
    
    # reactive value for the max salary
    max_salary = 
      reactive({input$max_salary})
    
    # display data on 'btn_run' click
    observeEvent(input$btn_run, {
      
      # num_players_returned = input_num()
      
      # Call the genetic algorithm function here with input_num() as input
      # Replace "genetic_algorithm_function" with the name of your function
      n = 25
      sel_pop = input_num()
      type = 'binary'
      max_salary = max_salary()
      
      Player_Data = data.frame(slice(Player_Data, 1:n)) %>%
        select(c('Player', 'Guaranteed_Compensation', 'Log_Goals_Added'))
      
      ### GA Functions
      
      ### Initialize population
      myInit <- function(k){
        
        function(GA){
          m <- matrix(0, ncol = GA@nBits, nrow = GA@popSize)
          
          for(i in seq_len(GA@popSize))
            m[i, sample(GA@nBits, k)] <- 1
          
          m
        }
      }
      
      
      ### Crossover function
      myCrossover <- function(GA, parents){
        
        parents <- GA@population[parents,] %>%
          apply(1, function(x) which(x == 1)) %>%
          t()
        
        parents_diff <- list("vector", 2)
        parents_diff[[1]] <- setdiff(parents[2,], parents[1,])
        parents_diff[[2]] <- setdiff(parents[1,], parents[2,])
        
        children_ind <- list("vector", 2)
        for(i in 1:2){
          k <- length(parents_diff[[i]])
          change_k <- sample(k, sample(ceiling(k/2), 1))
          children_ind[[i]] <- if(length(change_k) > 0){
            c(parents[i, -change_k], parents_diff[[i]][change_k])
          } else {
            parents[i,]
          }
        }
        
        children <- matrix(0, nrow = 2, ncol = GA@nBits)
        for(i in 1:2)
          children[i, children_ind[[i]]] <- 1
        
        list(children = children, fitness = c(NA, NA))
      }
      
      ### Mutation function
      myMutation <- function(GA, parent){
        
        # print(nrow(nBits))
        
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
        
        current_goals_added=x%*%Player_Data$Log_Goals_Added
        current_salary=x%*%Player_Data$Guaranteed_Compensation
        if(current_salary>max_salary)
        {
          return(0) # Penalty for exceeding the max salary
        }
        else
        {
          return(current_goals_added)
        }
      }
      
      GA <- ga(
        type = "binary",
        fitness = fitness,
        nBits = nrow(Player_Data),
        population = myInit(sel_pop),
        crossover = myCrossover,
        mutation = myMutation,
        run = 150,
        pmutation = .8,
        maxiter = 150,
        popSize = 50
      )

      output$table = renderDataTable(Player_Data[GA@solution == 1,] %>%
                                       select(c('Player',
                                                'Log_Goals_Added',
                                                'Guaranteed_Compensation')))
      
      # output$table = renderDataTable(Player_Data[sel_pop,])
      })
  }
)


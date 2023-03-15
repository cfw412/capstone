
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

### Import and Clean Data ###

# Steps to pull files from repo
# this could go in a function to de-duplicate code
owner <- "cfw412"
repo <- "capstone"
path <- "Data" 

# Construct the URL for the API endpoint with the main branch
url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path, "?ref=main")

# Make a GET request to the API endpoint and convert the response to a data frame
response <- GET(url)
data <- content(response, "text")
df <- fromJSON(data)

ga_salary_data <- data.frame()

ga_salary_data_url = df %>%
  filter(name == "2022_MLS_GoalsAdded_and_Salary_Data.xlsx") %>%
  select('download_url') %>%
  pull()

# Steps to pull files from a different folder in the repo
path <- "Data/MLS_Player_Season_Stats" 

url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path, "?ref=main")

response <- GET(url)
data <- content(response, "text")
df <- fromJSON(data)

temp_file <- tempfile()
download.file(ga_salary_data_url, temp_file, mode = "wb")

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

Player_Data <- Player_Data %>% 
  mutate(Log_Goals_Added = log10(Goals_Added+(abs(min(Player_Data$Goals_Added))+1.01)))

### Shiny App ###

shinyApp(
  
  # UI function
  ui = fluidPage(
    
    # Title
    titlePanel("Example App"),
    
    # Set the input fields
    sidebarLayout(
      sidebarPanel(
        numericInput(inputId = "input_number", 
                     label = "Enter the row number", 
                     value = 1),
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
    
    # display data on 'btn_run' click
    observeEvent(input$btn_run, 
                 {row_num <- input_num()
                 output$table = 
                   renderDataTable(Player_Data[row_num,] %>% 
                                     select(c('Player', 
                                              'Goals_Added', 
                                              'Guaranteed_Compensation')))})
  }
)


library(shiny)
library(shinydashboard)
library(babynames)
library(tidyverse)
library(viridis)
library(plotly)


ui <- dashboardPage(
  
  # format
  skin="blue",
  
  # define the title
  dashboardHeader(
    title="Baby Names"
  ),
  
  # define the sidebar
  dashboardSidebar(
    # set sidebar menu  
    sidebarMenu(
      textInput("in_name","Name",value="Ander"),
      menuItem("Name by Year", tabName = "by_year"),
      menuItem("Similar names", tabName = "similar")
    )
  ),
  
  # define the body
  dashboardBody(
    tabItems(
      # first page
      tabItem("by_year",
              h2("Babies named",textOutput("in_name1", inline=TRUE),"by year of birth and sex"),
              box(plotlyOutput("p_timeseries"), width= 500)
      ),
      # second page
      tabItem("similar",
              h2("Babies with names similar to ",textOutput("in_name2", inline=TRUE)),
              h3("10 most similar names in the last 10 years by sex"),
              box(plotlyOutput("p_hist_similar"), width= 500),
              h3("100 most similar names all time"),
              box(dataTableOutput("t_similar"), width= 500)
      )
    )
  )
  
)

server <- function(input, output) {
  
  # --------------------------------------------------
  # define the name for titling
  # --------------------------------------------------
  # define the name twice to be used twice above
  output$in_name1 <- renderText({
    input$in_name
  })
  output$in_name2 <- renderText({
    input$in_name
  })
  
  # --------------------------------------------------
  # time series of name
  # --------------------------------------------------
  output$p_timeseries <- renderPlotly({
    
    in_name <-  input$in_name
    
    p_timeseries <- ggplot(babynames %>% filter(name==in_name),
                           aes(x=year,
                               y=n,
                               color=sex)) +
      geom_line() + 
      scale_color_viridis(discrete = TRUE) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            legend.position = "none") + 
      scale_x_continuous(limits = c(min(babynames$year),NA)) 
    
    ggplotly(p_timeseries) %>%
      layout(hovermode = "x") %>%
      rangeslider(start=min(babynames$year), end=lubridate::year(lubridate::today())) 
  })
  
  # --------------------------------------------------
  # time series of similar names
  # --------------------------------------------------
  output$p_hist_similar <- renderPlotly({
    
    # get name
    in_name <-  input$in_name
    
    # find similar names
    unique_names <- babynames %>% 
      filter(year>2010) %>% 
      pull(name) %>%
      unique()
    dist <- c(stringdist::stringdistmatrix(in_name, unique_names))
    close_names <- unique_names[order(dist)][2:11]
    
    # plot
    p_hist_similar <- ggplot(babynames %>% 
                                     filter(name%in%close_names & 
                                              year > 2010) %>%
                                     group_by(name, sex) %>%
                                     summarise(n=sum(n)),
                                   aes(x=name,
                                       y=n,
                                       fill=sex)) +
      geom_histogram(stat="identity",
                     position = position_dodge2(preserve = "single")) + 
      theme_minimal() +
      theme(axis.title = element_blank()) 
    
    # plotly for final graph
    ggplotly(p_hist_similar)
  })
  
  # --------------------------------------------------
  # table of similar names
  # --------------------------------------------------
  output$t_similar <- renderDataTable({
    
    # get name
    in_name <-  input$in_name
    
    # find similar names
    unique_names <- babynames %>% 
      pull(name) %>%
      unique() 
    dist <- c(stringdist::stringdistmatrix(in_name, unique_names))
    close_names <- data.frame(name=unique_names[order(dist)][1:100],
                              rank=1:100)
    
    # summarize results for those names
    babynames %>% 
      right_join(close_names, by="name") %>%
      group_by(name, rank, year) %>% 
      summarise(male = sum(n*(sex=="M")),
                female = sum(n*(sex=="F")),
                total = sum(n)) %>% 
      group_by(name, rank) %>% 
      summarise(peak = max(year[total==max(total)]),
                male = sum(male),
                female = sum(female),
                total = sum(total)) %>%
      mutate(female=paste0(round(100*female/total),"%"),
             male=paste0(round(100*male/total),"%")) %>%
      arrange(rank)
  })
  
}

shinyApp(ui, server)
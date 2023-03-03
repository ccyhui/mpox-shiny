source('mpox.R')

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(zoo)
library(tidyverse)
library(lubridate)

mpox <- read_csv('data/mpox.csv', show_col_types=F)

top_countries <- mpox %>%
    group_by(country) %>%
    summarize(total_cases=sum(cases)) %>%
    arrange(desc(total_cases)) %>%
    slice(1:11) %>%
    pull(country)

plot_cases <- function(mpox_data, countries, end_date, metric=c('new', 'cumulative', 'smooth')){
    df_plot <- mpox_data %>%
        filter(date<=as_date(end_date),
               country %in% countries)
    
    if (metric=='new') {
        p <- df_plot %>% ggplot(aes(x=date, y=cases, fill=country)) +
            geom_bar(position='stack', stat='identity', alpha=0.8) +
            labs(x='Date', y='New', fill='Country')
    } else if (metric=='cumulative') {
        p <- df_plot %>%
            ggplot(aes(x=date, y=cum_cases, color=country)) +
            geom_line() + geom_point(size=1, alpha=0.8) +
            labs(x='Date', y='Cumulative', color='Country')
    } else if (metric=='smooth') {
        p <- df_plot %>%
            ggplot(aes(x=date, y=smooth_cases, color=country)) +
            geom_line() +
            labs(x='Date', y='Smooth', color='Country')
    }
    
    out <- p +
        scale_x_date(date_breaks='1 month', date_labels='%b %y') +
        theme_bw()
    
    plotly::ggplotly(out)
}

plot_heatmap <- function(mpox_data, countries, end_date, metric=c('new', 'cumulative', 'smooth')){
    df_plot <- mpox_data %>%
        filter(date<=as_date(end_date),
               country %in% countries)
    
    if (metric=='new') {
        p <- df_plot %>% ggplot(aes(x=date, y=country, fill=cases)) +
            geom_tile(color='white') +
            labs(x='Date', y='New', fill='Country')
    } else if (metric=='cumulative') {
        p <- df_plot %>%
            ggplot(aes(x=date, y=country, fill=cum_cases)) +
            geom_tile(color='white') +
            labs(x='Date', y='Cumulative', fill='Country')
    } else if (metric=='smooth') {
        p <- df_plot %>%
            ggplot(aes(x=date, y=country, fill=smooth_cases)) +
            geom_tile(color='white') +
            labs(x='Date', y='Smooth', fill='Country')
    }
    
    out <- p +
        scale_x_date(date_breaks='1 month', date_labels='%b %y') +
        scale_fill_gradient(low='white', high='blue') +
        theme_bw()
    
    plotly::ggplotly(out)
}

summ_box <- function(mpox_data, end_date){
    
    latest_cases <- mpox_data %>%
        filter(date<=as_date(end_date),
               country=='All') %>%
        pull(cases) %>%
        tail(1)
    
    cum_cases <- mpox_data %>%
        filter(date<=as_date(end_date),
               country=='All') %>%
        pull(cum_cases) %>%
        tail(1)
    
    total_countries <- mpox_data %>%
        filter(date<=as_date(end_date),
               country!='All',
               cum_cases!=0) %>%
        group_by(country) %>%
        slice(which.max(date)) %>%
        pull(country) %>%
        n_distinct()
    
    return(c(latest_cases, cum_cases, total_countries))
    
}

ui <- fluidPage(
    
    useShinydashboard(),
    
    theme=shinytheme('flatly'),
    navbarPage(
        title='Monkeypox in Europe',
        tabPanel(
            'Plots',
            sidebarLayout(
                sidebarPanel(
                    sliderInput('end_date', 'End Date', min=as.Date(min(mpox$date)), max=today(), value=today()),
                    selectInput('countries', 'Countries', choices=unique(mpox$country), multiple=TRUE, selected=top_countries),
                    selectInput('metric', 'Metric', c('New'='new', 'Cumulative'='cumulative', 'Smooth'='smooth'))
                ),
                mainPanel(
                    fluidRow(
                        valueBoxOutput('cum_cases_box'),
                        valueBoxOutput('latest_cases_box'),
                        valueBoxOutput('total_countries_box')
                    ),
                    tabsetPanel(
                        tabPanel('Line Chart', plotlyOutput('line_plot')),
                        tabPanel('Heatmap', plotlyOutput('heatmap'))
                    )
                )
            )
        )
    )
)

server <- function(input, output) {
    
    output$line_plot <- renderPlotly({
        plot_cases(mpox, input$countries, input$end_date, input$metric)
    })
    
    output$heatmap <- renderPlotly({
        plot_heatmap(mpox, input$countries, input$end_date, input$metric)
    })
    
    output$cum_cases_box <- renderValueBox({
        cum_cases <- summ_box(mpox, input$end_date)[2]
        valueBox(
            cum_cases,
            paste('Cumulative Cases'),
            icon=icon('chart-line'),
            color='purple'
        )
    })
    
    output$latest_cases_box <- renderValueBox({
        latest_cases <- summ_box(mpox, input$end_date)[1]
        valueBox(
            latest_cases,
            paste('Latest Confirmed Cases'),
            icon=icon('check-circle'),
            color='light-blue'
        )
    })
    
    output$total_countries_box <- renderValueBox({
        total_countries <- summ_box(mpox, input$end_date)[3]
        valueBox(
            total_countries,
            paste('Affected Countries'),
            icon=icon('globe'),
            color='blue'
        )
    })
}

shinyApp(ui=ui, server=server)
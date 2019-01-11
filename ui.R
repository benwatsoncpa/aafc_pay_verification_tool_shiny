rm(list=ls())

for (i in 1:50) gc(reset=T)

library(shiny)
library(data.table)
library(tidyverse)

# Read Data and format dates --------------------------------------------------

pris <- fread("pris.csv")
from.dates <- fread("from.dates.csv")
end.dates <- fread("end.dates.csv")

from.dates[,ed_frm_dt:=as.Date(ed_frm_dt)]
end.dates[,ed_end_dt:=as.Date(ed_end_dt)]

shinyUI(
  fluidPage(
    titlePanel("Pay Verification Tool"),
    h4("Select Employee and Pay Cheque Periods"),
    fluidRow(
      column(2,
             selectInput(
               "PRI",
               "Personnel Record Identifier (PRI)",
               choices = sort(unique(pris$pri_numb)),
               selected = min(pris$pri_numb)
               )
             ),
      column(2,
             offset=0,
             sliderInput(
               "start",
               "Bi-weekly Pay Start Date",
               min = as.Date(min(from.dates$ed_frm_dt)),
               max = as.Date(max(from.dates$ed_frm_dt)),
               value = as.Date(min(from.dates$ed_frm_dt)),
               step = 14,
               timeFormat = "%Y-%m-%d"
               )
      ),
      column(2,
             offset=0,
             sliderInput(
               "end",
               "Bi-weekly Pay End Date",
               min = as.Date(min(end.dates$ed_end_dt)),
               max = as.Date(max(end.dates$ed_end_dt)),
               value = as.Date(max(end.dates$ed_end_dt)),
               step = 14,
               timeFormat = "%Y-%m-%d")
             )
      ),
    h4("Select Graph Type and Line Colour"),
    fluidRow(column(2,
                    radioButtons("graph",
                                 "Graph Type:",
                                 choices = list(
                                   "Difference",
                                   "Cumulative Difference"
                                 ),
                                 selected = "Difference"
                    )
    ),
    column(3,
           radioButtons(
             "status",
             "Line Colour:",
             choices = list("Action", "Classification", "Employment Type","Branch"),
             selected = "Action")
    )),
    h4("Export Data"),
    fluidRow(
      column(2,
             downloadButton(outputId="export_current.csv",
                            label="Export current employee"))
    ),
    title = "Discovering Pay Issues is Fun",
    plotOutput('biweekly_plot')
    )
  )
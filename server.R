  library(shiny)
  library(data.table)
  library(tidyverse)

# Shiny Server ----------------------------------------------------------------

shinyServer(function(input, output) {
  output$biweekly_plot <- renderPlot({
    
    col.classes <- c("integer",
                     "Date",
                     "Date",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "character",
                     "character",
                     "character",
                     "character",
                     "character")
    
    # Read and pre-process data -----------------------------------------------
    df <- fread(paste("./files/",input$PRI,".csv",sep=''),colClasses = col.classes)
    
    df[,ed_frm_dt:=as.Date(ed_frm_dt)]
    df[,ed_end_dt:=as.Date(ed_end_dt)]
    df <- df[ed_frm_dt <=as.Date("2018-01-25")]
    df[is.na(action),action:="No Info"]
    df[is.na(classification),classification:="No Info"]
    df[is.na(emp.type),emp.type:="No Info"]
    
    
    # Manipulate dataset ------------------------------------------------------
    df <- df[ed_frm_dt>=input$start & ed_frm_dt<=input$end]
    df[,input.graph:=input$graph]
    df[,input.status:=input$status]
    df[,value:=ifelse(input.graph=="Difference",diff,cumulative)]
    
    df[,variable := ifelse(input.status=="Action",action,
                           ifelse(input.status=="Classification",classification,
                                  ifelse(input.status=="Branch",branch,emp.type)))]
    
    # Export Function ---------------------------------------------------------
    output$export_current.csv <- downloadHandler(
      filename = "export.current.csv",
      content = function(file) {
        write.csv(select(df,pri_numb:emp.type), file, row.names = FALSE)
      }
    )
    
    # Set Breaks --------------------------------------------------------------
    breaks.arg <- if (length(df$ed_frm_dt)<=18){
      df$ed_frm_dt} else{
      seq(from=min(df$ed_frm_dt),
          to=max(df$ed_end_dt),
          length.out = 18)
      }
    

    # Visualize ---------------------------------------------------------------
    ggplot(df,aes(ed_frm_dt,value,group = group,col = as.factor(variable))) +
      geom_point() + geom_line() +
      guides(col=guide_legend(title="")) +
      labs(title = input$PRI,x = "Biweekly Pay Date",y = "Biweekly Gross Variance", face = "bold", size = 18) +
      theme(text = element_text(size=16), legend.text=element_text(size=16), axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_x_date(date_labels = "%d-%b-%Y",breaks=breaks.arg)+
      scale_y_continuous(labels= scales::comma)+
      geom_hline(aes(yintercept=0), colour="grey", linetype="dashed")
  }, height = 550, width = 1000)
})

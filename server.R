shinyServer(function(input, output) {
  
  # First UI input (Service column) filter clientData 
  output$serviceControls <- renderUI({
    if (is.null(clientData()))
      return("No client selected")
    selectInput("service_select", 
                "Choose Service:", 
                choices = as.character(levels(clientData()$Service.Name)),
                selected = input$service_select
    )
  })
  
  # Second UI input (Rounds column) filter service-filtered clientData  
  output$roundControls <- renderUI({
    if (is.null(serviceFiltered_clientData()))
      return("No service selected")
    
    checkboxGroupInput("round_select", 
                       "Choose Round:", 
                       choices = as.character(levels(serviceFiltered_clientData()$Round)))
  })  
  
  # First data load (client data)
  clientData <- reactive({
    if (is.null(input$client_select))
      return(NULL)
    get(input$client_select)
  })
    
  # Second data load (filter by service column)
  serviceFiltered_clientData <- reactive({
    dat <- clientData()
    if (is.null(dat))
      return(NULL)
    if (!is.null(input$service_select)) # !
      dat <- dat[dat$Service.Name %in% input$service_select,]
    dat <- droplevels(dat)
    return(dat)
  })
  
  # Third data load (filter by round column)
  roundFiltered_clientData <- reactive({
    dat <- serviceFiltered_clientData()
    if (is.null(dat))
      return(NULL)
    if (!is.null(input$round_select)) # !
      dat <- dat[dat$Round %in% input$round_select,]
    return(dat)
  })
  
  ####################################################### END DATA FILTERIING AREA  

  # Audit count panel for Summary tab
  output$auditsCount <- renderText({
    if (is.null(roundFiltered_clientData()))
      return("No client selected")
    paste("Total Audits:",nrow(roundFiltered_clientData()))
  })
  # Audit count panel for navigation well panel  
  output$auditsCount2 <- renderText({
    if (is.null(roundFiltered_clientData()))
      return("No client selected")
    nrow(roundFiltered_clientData())
  })
  
  
  # Specialist count panel for Summary tab
  output$specialistCount <- renderText({
    if (is.null(roundFiltered_clientData()))
      return("No client selected")
    paste("Unique Specialists:",length(unique(roundFiltered_clientData()$Specialist)))
  })
  # Specialist count panel for navigation well panel
  output$specialistCount2 <- renderText({
    if (is.null(roundFiltered_clientData()))
      return("No client selected")
    length(unique(roundFiltered_clientData()$Specialist))
  })
  
  # Score describe reactive
  ScoreDescribe.reactive <- reactive({
    if (is.null(roundFiltered_clientData()))
      return("No client selected")
    summarize(roundFiltered_clientData(), 
              mean      = round(mean(Score),2),
              median    = round(median(Score),2),
              mad       = round(mad(Score),2),
              sd        = round(sd(Score),2),
              se        = round(sd(Score) / sqrt(length(Score)),2),
              min       = round(min(Score),2),
              max       = round(max(Score),2),
              range     = round(max(Score) - min(Score),2),
              skew      = round(skew(Score),2),
              kurtosis  = round(kurtosi(Score),2)
    )
  })
  
  # Score describe panel
  output$ScoreDescribe <- renderTable({
    ScoreDescribe.reactive()
  },include.rownames=FALSE)
  
  # Duration describe reactive
  DurationDescribe.reactive <- reactive({
    if (is.null(roundFiltered_clientData()))
      return("No client selected")
    summarize(roundFiltered_clientData(), 
              mean      = round(mean(Audit.Duration/60),2),
              median    = round(median(Audit.Duration/60),2),
              mad       = round(mad(Audit.Duration/60),2),
              sd        = round(sd(Audit.Duration/60),2),
              se        = round(sd(Audit.Duration/60) / sqrt(length(Audit.Duration/60)),2),
              min       = round(min(Audit.Duration/60),2),
              max       = round(max(Audit.Duration/60),2),
              range     = round(max(Audit.Duration/60) - min(Audit.Duration/60),2),
              skew      = round(skew(Score),2),
              kurtosis  = round(kurtosi(Score),2)
    )
  })
  
  # Duration describe reactive panel
  output$DurationDescribe <- renderTable({
    DurationDescribe.reactive()
  },include.rownames=FALSE)
  
  
  # ddply specialist reactive
  reactive.specialistPlyr <- reactive({
    if (is.null(roundFiltered_clientData()))
      return("No client selected")
    specialistPlyr <- ddply(roundFiltered_clientData(), .(Specialist), summarize, 
          n          = length(Score),
          #ScoMean    = round(mean(Score),2),
          ScoMedian  = round(median(Score),2),
          ScoSD      = round(sd(Score),2),
          ScoMad     = round(mad(Score),2),
          #ScoSE     = round(sd(Score) / sqrt(length(Score)),2),
          #DurMean    = round(mean(Audit.Duration/60),2),
          DurMedian  = round(median(Audit.Duration/60),2),
          DurSD      = round(sd(Audit.Duration/60),2),
          DurMad     = round(mad(Audit.Duration/60),2)     
          #DurSE      = round(sd(Audit.Duration/60) / sqrt(length(Audit.Duration/60)),2)                   
    )
    specialistPlyr$High.Variability <- specialistPlyr$ScoMedian > ( median(specialistPlyr$ScoMedian) + (2 * median(specialistPlyr$ScoMad))) |
        specialistPlyr$ScoMedian < (median(specialistPlyr$ScoMedian) - (2 * median(specialistPlyr$ScoMad)))
    
    return(specialistPlyr)
  
  })
  
  # ddply specialist gvis options
  gvistableOptions <- reactive({
    list(
      page = ifelse(input$pageable==TRUE,'enable','disable'),
      pageSize = 10,
      width = '100%',
      allowHtml = T,
      height = '100%',
      chartid = "gvis_specialistPlyr"
    )
  }) 
  
  # ddply gvis table
  output$specialistPlyr <- renderGvis({
    gvisTable(reactive.specialistPlyr(), options = gvistableOptions() )         
  })
  
  # ddply summary export csv button
  output$download.summary <- downloadHandler(    
    filename = function() {
      paste('amx_summary_export-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(reactive.specialistPlyr(),
                file, row.names = F
      )
    }
  )
  
  
  # control chart tenure/daypart ggplot
  output$ccPlot <- renderPlot({
    if (is.null(serviceFiltered_clientData()))
      return("No client selected")
    dat <- roundFiltered_clientData()
    median.spec  <- dat$median.spec   <- ave(dat$Score, dat$Specialist, FUN = mean)
    

    # Tenure
    if (input$cc.colorby == "tenure_mode"){ 
      dat$input_color <- ave(dat$Tenure, dat$Specialist, FUN = Mode)
      #inputdat <- data.frame(input_color = dat[[input$cc.colorby]])
      #dat <- cbind(dat,inputdat)
    }
   
    # Day-part
    else if (input$cc.colorby == "daypart_mode"){
      # Create time column then bin by day part
      split_date_columns <- data.frame(str_split_fixed(dat$Start.Date, " ", 2))
      dat <- cbind(dat,split_date_columns)
      colnames(dat)[19] <- "Day"
      colnames(dat)[20] <- "Time"
      dat$Time <- as.character(dat$Time)  
      #dat$Time <- substr(strptime(dat$Time,"%I:%M:%S %p"),12,19) #start at 12th character end at 19th
      dat$Time <- as.numeric(substr(strptime(dat$Time,"%I:%M:%S %p"),12,13)) #start at 12th character end at 19th
      dat$Time <- cut(dat$Time, breaks = c(04,10,13,23), labels = c("Breakfast",'Lunch','Dinner'))
      dat$input_color <- ave(dat$Time, dat$Specialist, FUN = Mode)
    }
    
    specialistCC <- ggplot(dat, aes(x = reorder(Specialist, median.spec), y = median.spec)) +   
      geom_segment(aes(xend = Specialist), color = "grey60", yend = 0, 
                   linetype = "solid", size = .3, alpha = .1) +
      geom_point(aes(color = input_color),shape = 16, size = 4, alpha = 1) +
      theme_bw() +
      theme(axis.text.x = element_text(size = rel(1.5), angle = 0, hjust = 1),
        axis.text.y = element_text(size = rel(1), angle = 0, hjust = 1),
        panel.grid.major.y = element_blank(),
        axis.ticks.length = unit(0.3, "lines"),
        axis.ticks.margin = unit(1.5, "lines"),
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        title = element_text(size = rel(1.2)),
        legend.justification=c(0,1), legend.position=c(0,1)
        #legend.background = element_rect(color = NA, size = 0, fill = NA)
      ) +
      geom_hline(yintercept = median(median.spec), size = 2, color = "dodgerblue",alpha=.5) +
      geom_hline(yintercept = (median(median.spec)) - (sd(median.spec)),size=1,linetype='solid',alpha=.3) +
      geom_hline(yintercept = (median(median.spec)) + (sd(median.spec)),size=1,linetype='solid',alpha=.3) +
      geom_hline(yintercept = (median(median.spec)) - (2*sd(median.spec)),size=1,linetype='solid',alpha=.3) +
      geom_hline(yintercept = (median(median.spec)) + (2*sd(median.spec)),size=1,linetype='solid',alpha=.3) +
      ggtitle(paste("Specialist median score colored by",input$cc.colorby)) + 
                
      coord_flip() 
    
    print(specialistCC)
    })
  
  
  # density plot graph
  output$densityPlot <- renderPlot({  
    if(is.null(input$client_select))
      return()
    dat <- roundFiltered_clientData()
    
    # Round
    if (input$graph_variable.X == "Round"){ 
      dat <- data.frame(Y = dat[[input$graph_variable.Y]], var = dat[[input$graph_variable.X]])
    }
    # State
    else if (input$graph_variable.X == "State"){
      dat$State.mean <- ave(dat$Score, dat$State, FUN = mean)
      dat <- data.frame(Y = dat[[input$graph_variable.Y]], var = reorder(dat[[input$graph_variable.X]],dat$State.mean))  
    }
    # Tenure (same as Round)
    else {
      dat <- data.frame(Y = dat[[input$graph_variable.Y]], var = dat[[input$graph_variable.X]])
    } 
    
    densityPlot <- ggplot(head(dat, n = abs(input$obs)), aes(x = Y, group = var, fill = var)) + 
      stat_density(position = "dodge", alpha = .3, size = .5, color = "black", aes(y = ..density..)) +
      guides(fill = guide_legend(nrow=20,label.position = "right", keywidth = 1, keyheight = 1)) +
      theme(legend.text = element_text(size = 12)) +
      theme(legend.background = element_rect(color = NA, size = 0, fill = NA)) +
      theme(legend.justification=c(0,1), legend.position=c(0,1)) +
      theme(axis.title.y=element_blank()) + theme(axis.title.x=element_blank()) +
      ggtitle(paste(input$graph_variable.Y,"by",input$graph_variable.X)) + 
      theme(title = element_text(size = rel(1.2)))
    print(densityPlot)
  })
  
  # boxplot plot graph
  output$boxPlot <- renderPlot({
    if(is.null(input$client_select))
      return()
    dat <- roundFiltered_clientData()
    
    # Round
    if (input$graph_variable.X == "Round"){ 
      dat <- data.frame(Y = dat[[input$graph_variable.Y]], var = dat[[input$graph_variable.X]])
    }
    # State
    else if (input$graph_variable.X == "State"){
      dat$State.mean <- ave(dat$Score, dat$State, FUN = mean)
      dat <- data.frame(Y = dat[[input$graph_variable.Y]], var = reorder(dat[[input$graph_variable.X]],dat$State.mean))  
    }
    # Tenure (same as Round)
    else {
      dat <- data.frame(Y = dat[[input$graph_variable.Y]], var = dat[[input$graph_variable.X]])
    } 
    
    boxPlot <- ggplot(head(dat, n = abs(input$obs)), aes(x = var, y = Y)) + 
      geom_boxplot(aes(fill = var), alpha = .7, outlier.size = ifelse(input$outliers, 2, NA)) + 
      stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 4, alpha = .6,fill= "white") +
      theme(axis.text.x = element_text(size= rel(1),angle=90, hjust=1)) +
      xlab(input$graph_variable.X) +
      theme(axis.title.y=element_blank()) + theme(axis.title.x=element_blank()) +
      ggtitle(paste(input$graph_variable.Y,"by",input$graph_variable.X)) + 
      theme(title = element_text(size = rel(1.2)))
    
    
    print(boxPlot)
  })
  
  # reactive f(x) for lm
  lmFormulaText <- reactive({
    paste(input$graph_variable.Y,"~", input$graph_variable.X) #make formula string
  })
  
  #linear model
  output$LM <- renderPrint({
    if(is.null(input$client_select))
      return()   
    dat <- roundFiltered_clientData()
    lm1 <- (lm(as.formula(lmFormulaText()), data = dat))
    summary(lm1)
  })
  
  # lm ggplot
  output$lmPlot <- renderPlot({
    if(is.null(input$client_select))
      return()
    dat <- roundFiltered_clientData()
    
    lm_labels <- function(placeholder) {
      mod <- lm(Audit.Duration ~ Score, data=dat)
      formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                         round(coef(mod)[1], 2), round(coef(mod)[2], 2))
      r <- cor(dat$Audit.Duration, dat$Score)
      r2 <- sprintf("italic(R^2)('Coefficient of determination') == %.2f", r^2) 
      data.frame(formula=formula, r2=r2, stringsAsFactors=FALSE)
    }
    #labels <- ddply(dat, "State",lm_labels) Only for facets
    
    linearplot <- ggplot(data = dat, aes(x = Audit.Duration, y = Score)) +   
      geom_point(aes(),shape = 1,size = 3.5, alpha = 1) +
      geom_smooth() +
      geom_text(x = mean(dat$Audit.Duration), y = 65, size = 7, fontface = "bold", 
                color = "black", aes(label=formula), data = lm_labels(), parse = TRUE, hjust = 0) + 
      geom_text(x=mean(dat$Audit.Duration), y = 70, size = 7, fontface = "bold",
                color = "black", aes(label = r2), data = lm_labels(), parse = TRUE, hjust = 0)
    
    print(linearplot)
  })
  

  # ddply reactive
  reactive.scatterplot <- reactive({
    if (is.null(roundFiltered_clientData()))
      return("No client selected")
    trend_dat <- roundFiltered_clientData()
    trend_dat <- data.frame(Score = trend_dat$Score, 
                            Audit.Duration = trend_dat$Audit.Duration,
                            Completion.Date = trend_dat$Completion.Date,
                            Specialist = trend_dat$Specialist)
    datesplit <- str_split_fixed(trend_dat$Completion.Date, " ", 2)
    trend_dat$converted.date  <- datesplit[,1]
    trend_dat$converted.date  <- as.Date(trend_dat$converted.date,"%m/%d/%Y")
    return(trend_dat)
  })

  # ddply gvis options
  Options.scatterplot <- reactive({
    list(allowHtml = T)
  }) 
  
  output$trendplot <- renderGvis({
    gvisScatterChart(reactive.scatterplot(), options = Options.scatterplot())
  })
  
  
  
  
})#END  


library(shiny)
library(shinyBS)
library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(DT)
library(RColorBrewer)
library(dplyr)
library(plyr)

shinyServer(function(input, output, session) {

  # Define color palette
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  nbp<-7
  bases<-c("A", "C", "G", "T")

  # Revcomp function
  reverse_chars <- function(string){
    string_split = strsplit(as.character(string), split = "")
    reversed_split = string_split[[1]][nchar(string):1]
    paste(reversed_split, collapse="")
  }

  # Read dataset from server
  # map_a1 <- read.table("data/map_a1.txt", header=T, sep="\t", stringsAsFactors=F)

  orderedcats <- c("AT_CG", "AT_GC", "AT_TA", "GC_AT", "GC_CG", "GC_TA")
  orderedcats1 <- c("AT_GC", "GC_AT", "AT_CG", "GC_CG", "AT_TA", "GC_TA")
  orderedcats2 <- c("A>C", "A>G", "A>T", "C>T", "C>G", "C>A")

  adj <- 3
  nbp <- adj*2+1
  rates <- read.table(paste0("data/", nbp, "bp_1000k_rates.txt"), header=T, stringsAsFactors=F)
  rates$v2 <- substr(rates$Sequence,1,adj)
  rates$v2a <- as.character(lapply(as.vector(rates$v2), reverse_chars))
  rates$v2a <- factor(rates$v2a)
  rates$v3 <- substr(rates$Sequence, adj+2, adj*2+1)
  rates$v4 <- rates$rel_prop
  rates$Category <- gsub("cpg_", "", rates$Category2)
  rates <- rates %>%
    mutate(Category =
      plyr::mapvalues(Category, orderedcats, orderedcats2))
  # rates$Category <- gsub("_", ">", rates$Category)
  rates$v5 <- factor(rates$Category)
  # f <- read.table("data/frame.txt", header=T, sep="\t", stringsAsFactors=F)
  levs_a <- as.character(lapply(as.vector(levels(rates$v2a)), reverse_chars))

  nbox <- length(unique(rates$v2a))
  nint <- nbox/(4^(adj-1))
  xhi <- rep(1:(4^(adj-1)),4^(adj-1))*nint+0.5
  xlo <- xhi-nint
  yhi <- rep(1:(4^(adj-1)),each=4^(adj-1))*nint+0.5
  ylo <- yhi-nint
  f7 <- data.frame(xlo,xhi,ylo,yhi)
  f7$xlab <- rep(bases, 16)
  f7$ylab <- rep(bases, each=16)

  nbox <- length(unique(rates$v2a))
  nint <- nbox/(4^(adj-2))
  xhi <- rep(1:(4^(adj-2)),4^(adj-2))*nint+0.5
  xlo <- xhi-nint
  yhi <- rep(1:(4^(adj-2)),each=4^(adj-2))*nint+0.5
  ylo <- yhi-nint
  f5 <- data.frame(xlo,xhi,ylo,yhi)
  f5$xlab <- rep(bases, 4)
  f5$ylab <- rep(bases, each=4)

  axlabels <- data.frame(xlab=rep(bases, 16), ylab=rep(bases, 16),
    xlo=seq(1,64,1), ylo=seq(1,64,1))

  #keeprows <- rep(FALSE, nrow(map_a1))

  # Initialize data objects for selections from heatmap
  vals <- reactiveValues()
  rows <- reactiveValues()

  selpts <- data.frame()
  selrow <- data.frame()

  #if(input$)
  # Select category subset from dropdown
  curdata <- reactive({

    dat <- switch(input$category,
      AT_CG=rates[rates$v5=="A>C",],
      AT_GC=rates[rates$v5=="A>G",],
      AT_TA=rates[rates$v5=="A>T",],
      GC_AT=rates[rates$v5=="C>T",],
      GC_CG=rates[rates$v5=="C>G",],
      GC_TA=rates[rates$v5=="C>A",]
    )

    # vals$keeprows <- rep(FALSE, nrow(dat)) # resets selections
    vals$keeprows <- rep(FALSE, 4096) # resets selections

    # rows$dat <- rep(FALSE, nrow(dat))
    rows$dat <- rep(FALSE, 4096)
    selpts <- data.frame()
    selrow <- data.frame()
    dat
  })

  # Track batch selected by motifs
  selbatch <- reactive({
    dat<-curdata()
    sub<-data.frame()
    tmpgrep <- c("^[A-Z]", rep("[A-Z]",2), "[Z]", rep("[A-Z]",3))

    #tmpgrep[4] <- "[A]"
    if(length(input$u1)>=1){
      tmpgrep[4] <- "[A]"
      tmpgrep[3] <- paste0("[", paste(input$u1, collapse=","), "]")
      sub <- dat[grep(paste(tmpgrep, collapse=""), dat$Sequence), ]
    }
    if(length(input$u2)>=1){
      tmpgrep[4] <- "[A]"
      tmpgrep[2] <- paste0("[", paste(input$u2, collapse=","), "]")
      sub <- dat[grep(paste(tmpgrep, collapse=""), dat$Sequence), ]
    }
    if(length(input$u3)>=1){
      tmpgrep[4] <- "[A]"
      tmpgrep[1] <- paste0("^[", paste(input$u3, collapse=","), "]")
      sub <- dat[grep(paste(tmpgrep, collapse=""), dat$Sequence), ]
    }

    if(length(input$d1)>=1){
      tmpgrep[4] <- "[A]"
      tmpgrep[5] <- paste0("[", paste(input$d1, collapse=","), "]")
      sub <- dat[grep(paste(tmpgrep, collapse=""), dat$Sequence), ]
    }
    if(length(input$d2)>=1){
      tmpgrep[4] <- "[A]"
      tmpgrep[6] <- paste0("[", paste(input$d2, collapse=","), "]")
      sub <- dat[grep(paste(tmpgrep, collapse=""), dat$Sequence), ]
    }
    if(length(input$d3)>=1){
      tmpgrep[4] <- "[A]"
      tmpgrep[7] <- paste0("[", paste(input$d3, collapse=","), "]")
      sub <- dat[grep(paste(tmpgrep, collapse=""), dat$Sequence), ]
    }

    sub
  })

  # Track data selected from sources
  seldata <- reactive({
    dat <- curdata()
    batch <- selbatch()
    selrow <- dat[input$x1_rows_selected, , drop=F]
    #selrow <- dat[rows$keeprows, , drop=F]
    #selrow <- dat[rows$dat, , drop=F]
    selpts <- dat[vals$keeprows, , drop=F]
    #plotpts <- nearPoints(curdata(), input$plot_click)
    sub <- rbind(selrow, selpts, batch)
    sub
  })

  # Render datatable of selected category
  output$x1 <- DT::renderDataTable(curdata(), options = list(
    lengthMenu = list(c(5, 15, 25), c('5', '15', '25')),
    pageLength = 5
  ), server=TRUE)
  output$sub <- DT::renderDataTable(seldata(), server=TRUE)
  #output$batch <- DT::renderDataTable(selbatch(), server=TRUE)
  #output$rows <- DT::renderDataTable(rows$dat, server=TRUE)

  # Define plotUI
  output$plotui <- renderUI({
    plotOutput("plot", height=800, width=1000,
               click = "plot_click",
               dblclick = dblclickOpts(
                 id = "plot_dblclick",
                 delay = input$dblclick_delay
               ),
               hover = hoverOpts(
                 id = "plot_hover",
                 delay = input$hover_delay,
                 delayType = input$hover_policy,
                 nullOutside = input$hover_null_outside
               ),
               brush = brushOpts(
                 id = "plot_brush",
                 delay = input$brush_delay,
                 delayType = input$brush_policy,
                 direction = input$brush_dir,
                 resetOnNew = input$brush_reset
               )
    )
  })

  # Main plotting function
  output$plot <- renderPlot({
      dat <- curdata()
      subpts <- seldata()
      #selrow <- input$x1_rows_selected
      #selpts <- dat[vals$keeprows, ]

      # p <- ggplot()+
      #   geom_tile(data=dat, aes(x=v2a, y=v3, fill=v4))+
      #   geom_rect(data=f, size=1.4, colour="grey30",
      #             aes(xmin=xlo, xmax=xhi, ymin=ylo, ymax=yhi), fill=NA)+
      #   scale_fill_gradientn("Relative Rate\n",
      #                        colours=myPalette((nbp-1)^4),
      #                        trans="log",
      #                        breaks=c(min(dat$v4), mean(dat$v4), max(dat$v4)),
      #                        labels=c(round(min(dat$v4), 5),
      #                                 round(mean(dat$v4), 4),
      #                                 round(max(dat$v4), 3)),
      #                        limits=c(min(dat$v4), max(dat$v4)))+
      #   xlab(paste(paste(rep(paste(bases, collapse="  "), 4), collapse="  "), "\n",
      #              paste(bases, collapse=paste(rep(" ", 15), collapse=""))))+
      #   ylab(paste(paste(bases, collapse=paste(rep(" ", 16), collapse="")), "\n",
      #     paste(rep(paste(bases, collapse="  "), 4), collapse="   ")))+
      #   theme(
      #     legend.position="none",
      #     legend.title = element_text(size=18),
      #     legend.text = element_text(size=16),
      #     strip.text.x = element_text(size=40),
      #     axis.title.x = element_text(size=25),
      #     axis.title.y = element_text(size=22),
      #     axis.text.y = element_text(size=11, colour="black", angle=90, hjust=.5),
      #     axis.text.x = element_text(size=11, colour="black"))+
      #   scale_x_discrete(labels=rep(bases, 16))+
      #   scale_y_discrete(labels=rep(bases, 16))+
      #   facet_wrap(~v5, ncol=3, scales="free_x")

      p <- ggplot()+
    	geom_tile(data=dat, aes(x=v2a, y=v3, fill=v4))+
    	geom_rect(data=f7, size=0.2, colour="grey10",
    		aes(xmin=xlo, xmax=xhi, ymin=ylo, ymax=yhi), fill=NA)+
      geom_rect(data=f5, size=1, colour="black",
    		aes(xmin=xlo, xmax=xhi, ymin=ylo, ymax=yhi), fill=NA)+
      geom_text(data=f5[1:4,], size=8, aes(x=xlo+8, y=-4, vjust=0, label=xlab))+
      geom_text(data=f7[1:16,], size=6, aes(x=xlo+2, y=-2, vjust=0, label=xlab))+
      geom_text(data=f5[f5$xlo==0.5,], size=8, aes(x=-4, y=ylo+8, hjust=0, label=ylab))+
      geom_text(data=f7[f7$xlo==0.5,], size=6, aes(x=-2, y=ylo+2, hjust=0, label=ylab))+
      geom_text(data=axlabels, size=3, aes(x=xlo, y=-.5, vjust=0, label=xlab))+
      geom_text(data=axlabels, size=3, aes(x=-.5, y=ylo, hjust=0, label=ylab))+
    	scale_fill_gradientn("Relative Rate",
        colours=myPalette(11),
    		trans="log10",
    		breaks=10^(seq(-3.65,-.84,0.281)),
    		labels=round(seq(-3.65,-.84,0.281), 2),
    		limits=c(0.0002, 0.2))+
    	xlab("5' flank")+
    	ylab("3' flank")+
      # xlab(paste(paste(rep(paste(bases, collapse="   "), 4), collapse="  "), "\n",
      #            paste(bases, collapse=paste(rep(" ", 15), collapse=""))))+
      # ylab(paste(paste(bases, collapse=paste(rep(" ", 16), collapse="")), "\n",
      #   paste(rep(paste(bases, collapse="  "), 4), collapse="   ")))+
      scale_x_discrete(labels=rep(bases, 16))+
      scale_y_discrete(labels=rep(bases, 16))+
      theme_classic()+
        theme(
          # legend.position="none",
          panel.margin = unit(3, "lines"),
          legend.title = element_text(size=18),
          legend.text = element_text(size=16),
          strip.text.x = element_text(size=40),
          axis.title.x = element_text(size=25),
          axis.title.y = element_text(size=22, margin=margin(0,20,0,0)),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          # axis.text.y = element_text(size=11, colour="black", angle=90, hjust=.5),
          # axis.text.x = element_text(size=11, colour="black"),
          legend.key.height = unit(1.5, "cm"))+
    	facet_wrap(as.formula(paste("~", "v5")), ncol=3, scales="free_x")

      if(nrow(subpts)>=1){
        p<-p+geom_point(data=subpts, shape=0, aes(x=v2a, y=v3, size=2))
      }

      #datf <- curframe()
      datf <- data.frame()
      if(nrow(datf)>=1){

        p<-p+geom_rect(data=datf, size=1.4, colour="red",
                  aes(xmin=xlo, xmax=xhi, ymin=ylo, ymax=yhi), fill=NA)
      }

      p
  })

  # Track clicked cells on heatmap
  observeEvent(input$plot_click, {
    res <- nearPoints(curdata(), input$plot_click, allRows = TRUE)

    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })

  # Track clicked rows on data
  observeEvent(input$x1_cell_clicked, {
    #currow <- input$x1_rows_selected
    info <- input$x1_cell_clicked
    ind <- info$row
    dat <- curdata()


    #rows$dat <- xor(rows$dat, ind)
  })

  output$downloadSubData <- downloadHandler(
    filename = paste0(input$category, "_data.csv"),
    content = function(file) {
      s=input$sub_rows_all
      write.csv(rates[s, , drop=F] , file)
    }
  )

  output$downloadData <- downloadHandler(
    filename = "data_full.csv",
    content = function(file) {
      write.csv(rates, file)
    }
  )

  output$hover_info <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      res <- nearPoints(curdata(), input$plot_hover)
      #dat <- curdata()
      #dist=sqrt((hover$x-mtcars$mpg)^2+(hover$y-mtcars$disp)^2)
      cat("Motif: ", paste(res), "\n")
      #if(min(dist) < 3)
       # mtcars$wt[which.min(dist)]
    }


  })

  # Show point selected from plot
  #output$info2 <- renderDataTable({
    # With base graphics, need to tell it what the x and y variables are.
    #nearPoints(curdata(), input$plot_click)
  #})



  ##########
  # Old code below here
  output$info <- renderDataTable({
    # With base graphics, need to tell it what the x and y variables are.
    brushedPoints(curdata(), input$plot_brush)
  })



  output$plot_clickinfo <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  output$plot_dblclickinfo <- renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })
  output$plot_hoverinfo <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  output$plot_brushinfo <- renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })

  #
  output$plot_clicked_points <- renderDataTable({
    dat <- curdata()

    res <- nearPoints(dat, input$plot_click,
                      threshold = input$max_distance, maxpoints = input$max_points,
                      addDist = TRUE)


    #res$dist_ <- round(res$dist_, 1)

    datatable(res)
  })
  output$plot_brushed_points <- renderDataTable({
    dat <- curdata()
    # With base graphics, we need to explicitly tell it which variables were
    # used; with ggplot2, we don't.
    if (input$plot_type == "base")
      res <- brushedPoints(dat, input$plot_brush, xvar(), yvar())
    else if (input$plot_type == "ggplot2")
      res <- brushedPoints(dat, input$plot_brush)

    datatable(res)
  })
})

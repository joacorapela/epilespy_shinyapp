
# processAll <- function() {
    require(ggplot2)
    require(plotly)
    require(shiny)

    seizure1StartTimeString <- "2012-Nov-05 07:21:09.144"
    seizure2StartTimeString <- "2012-Nov-05 09:08:41.713"
    seizure3StartTimeString <- "2012-Nov-05 11:58:24.244"
    formatString <- "%Y-%b-%d %H:%M:%OS"
    estTz <- "EST"
    lfpDownsampleFreq <- 80
    dim <- 3
    sizePoints <- 2
    freqChoicePattern <- "[%d %d]"
    scaleName <- "Time\n(sec)"
    lowScaleCol <- "yellow"
    midScaleCol <- "red"
    highScaleCol <- "green"
    xlab <- "Low Dimension 1"
    ylab <- "Low Dimension 2"
    zlab <- "Low Dimension 3"
    title <- "MG63: 05-103012-025 -- 05-103012-032"
    defaultChannelToPlot <- 1
    defaultLFPWinWidthSec <- 100
    largestLFPWinSizeSec <- 100*c(-1,1)
    defaultLFPWinSizeSec <- 50*c(-1,1)
    tsneColorbarTitle <- "Time (sec)"
    # tsneFs <- .5
    # prefix <- "05-103012-025_LFPMUA-v6_allDimReducedFeaturesPercVar0.90_l1_distances"
    featuresFs <- 2
    tsneFs <- 2
    tsneFilename <- "data/tsne.RData"
    lfpFilename <- "data/lfp.RData"
    muaCountFilename <- "data/muaCountTotal.RData"
    featuresTimeStampsFilename <- "data/timeStamps.RData"
    pxxFilename <- "data/pxx.RData"

    seizure1StartTimestamp <- as.POSIXct(x=seizure1StartTimeString,
                                          format=formatString, tz=estTz)
    seizure2StartTimestamp <- as.POSIXct(x=seizure2StartTimeString,
                                          format=formatString, tz=estTz)
    seizure3StartTimestamp <- as.POSIXct(x=seizure3StartTimeString,
                                          format=formatString, tz=estTz)

    defaultSelectedTimeStamp <- as.POSIXct(x="2012-Nov-05 07:21:09:144",
                                            format=formatString, tz=estTz)

    tsneLoadRes <- get(load(tsneFilename))
    tsneDF <- data.frame(x=tsneLoadRes$tsneResults$Y[,1], 
                          y=tsneLoadRes$tsneResults$Y[,2], 
                          z=tsneLoadRes$tsneResults$Y[,3],
                          time=(1:nrow(tsneLoadRes$tsneResults$Y))/tsneFs,
                          timeStamp=tsneLoadRes$timeStamps)

    sliderTitle <- paste("Start times: sz1=", 
                         strftime(seizure1StartTimestamp, format=formatString),
                         ", sz2=", 
                         strftime(seizure2StartTimestamp, format=formatString),
                         ", sz3=",
                         strftime(seizure3StartTimestamp, format=formatString)) 
    lfpLoadRes <- get(load(lfpFilename))
    lfpFs <- lfpLoadRes$Fs
    lfpTimeStamps <- lfpLoadRes$timeStamp
    if(is.numeric(lfpTimeStamps)) {
        lfpTimeStamps <- as.POSIXct(lfpTimeStamps, origin=sprintf("1970-01-01 00:00.00 %s", estTz))
    }
    lfpTimes <- seq(from=0, by=1/lfpFs, length.out=length(lfpTimeStamps))
    lfpDF <- data.frame(time=lfpTimes, timeStamps=lfpTimeStamps)
    lfpDF$LFP <- t(lfpLoadRes$LFP)
    channels <- 1:ncol(lfpDF$LFP)

    muaCount <- t(get(load(muaCountFilename)))
    featuresTimeStamps <- get(load(featuresTimeStampsFilename))
    featuresTimes <- (1:length(featuresTimeStamps))/featuresFs

    pxxDF <- get(load(pxxFilename))
    freqChoices <- list()
    for(i in 1:nrow(pxxDF$freq)) {
        freqChoices <- c(freqChoices, sprintf(freqChoicePattern, 
                                               pxxDF$freq[i,1],
                                               pxxDF$freq[i,2]))
    }
    getSlectedFreqIndex <- function(selectedText) {
        for(i in 1:length(freqChoices)) {
            if(selectedText==freqChoices[i]) {
                return(i)
            }
        }
        return(-1)
    }

    ui <- fluidPage(
        fluidRow(
            sidebarPanel(sliderInput("tsneTimeLimit", sliderTitle,
                                      min=min(tsneDF$timeStamp),
                                      max=max(tsneDF$timeStamp), 
                                      value=range(tsneDF$timeStamp)), width='100%')
        ),
        hr(),
        plotlyOutput("tsnePlot"),
        hr(),
        plotlyOutput("lfpPlot"),
        hr(),
        plotlyOutput("muaCountPlot"),
        hr(),
        plotlyOutput("pxxPlot"),
        hr(),
        fluidRow(
            sidebarPanel(sliderInput(inputId="detailWinSizeSec", 
                                      label="Detail Window Size", 
                                      min=largestLFPWinSizeSec[1], 
                                      max=largestLFPWinSizeSec[2], 
                                      value=defaultLFPWinSizeSec),
                          selectInput(inputId = "detailChannel", 
                                        label = "Channel Number:",
                                        choices = channels,
                                        selected = defaultChannelToPlot),
                          selectInput(inputId="detailFreq",
                                       label="Pxx Frequency",
                                       choices=freqChoices))
        )
    )
    server <- function(input, output) {
        output$tsnePlot <- renderPlotly({
            lowTSNSETimeLimit <- as.POSIXct(input$tsneTimeLimit[1],
                                             format=formatString, tz=estTz)
            highTSNSETimeLimit <- as.POSIXct(input$tsneTimeLimit[2],
                                              format=formatString, tz=estTz)
            fromSample <- which.min(abs(lowTSNSETimeLimit-tsneDF$timeStamp))
            toSample <- which.min(abs(highTSNSETimeLimit-tsneDF$timeStamp))
            p <- plot_ly(data=tsneDF[fromSample:toSample,], 
                          x=~x, y=~y, z=~z, key=~timeStamp,
                          type='scatter3d', 
                          mode='lines+markers',
                          line=list(color=~time), 
                          marker = list(size=sizePoints, 
                                         color=~time,
                                         showscale=TRUE,
                                         colorbar=
                                          list(title=tsneColorbarTitle)),
                          text= ~paste("time: ", time, "\ntimeStamp: ",
                                       format(timeStamp, tz=estTz))) %>%
            layout(scene=list(xaxis=list(title=xlab), yaxis=list(title=ylab),
                                                      zaxis=list(title=zlab)))
            p
        })
        output$lfpPlot <- renderPlotly({
            d <- event_data("plotly_click")
            if(is.null(d)) {
                selectedTimeStamp <- defaultSelectedTimeStamp
            } else {
                selectedTimeStamp <- as.POSIXct(x=d$key)
            }
show(paste("Updating LFP at timeStamp ", selectedTimeStamp))
            selectedSample <- which.min(abs(selectedTimeStamp-lfpDF$timeStamp))
            fromSample <- max(selectedSample+input$detailWinSizeSec[1]*lfpFs, 1)
            toSample <- min(selectedSample+input$detailWinSizeSec[2]*lfpFs, length(lfpDF$timeStamp))
            # p <- plot_ly(data=lfpDF[fromSample:toSample,], 
             #              x=~time, y=~LFP[,input$detailChannel], 
            timesToPlot <- lfpDF$time[fromSample:toSample]
            timeStampsToPlot <-  lfpDF$timeStamp[fromSample:toSample]
            # formattedTimeStampsToPlot <- format(x=lfpDF$timeStamp[fromSample:toSample], format="%Y-%b-%d %H:%M:%OS6")
            lfpToPlot <- as.vector(lfpDF$LFP[fromSample:toSample, as.numeric(input$detailChannel)])
            lfpDFToPlot <- data.frame(LFP=lfpToPlot, 
                                       time=timesToPlot,
                                       timeStamp=timeStampsToPlot)
                                      #  timeStamp=formattedTimeStampsToPlot)
            p <- plot_ly(data=lfpDFToPlot, x=~time, y=~LFP,
                          type='scatter', 
                          mode='lines+markers',
                          marker = list(size=sizePoints),
                          text= ~paste("time: ", time, "\ntimeStamp: ", format(timeStamp, tz=estTz))) %>%
            layout(xaxis=list(title="Time (sec)"), yaxis=list(title="LFP"))
            p
        })
        output$muaCountPlot <- renderPlotly({
            d <- event_data("plotly_click")
            if(is.null(d)) {
                selectedTimeStamp <- defaultSelectedTimeStamp
            } else {
                selectedTimeStamp <- as.POSIXct(x=d$key)
            }
show(paste("Updating muaCount at time ", selectedTimeStamp))
            selectedSample <- which.min(abs(selectedTimeStamp-featuresTimeStamps))
            fromSample <- max(selectedSample+input$detailWinSizeSec[1]*featuresFs, 1)
            toSample <- min(selectedSample+input$detailWinSizeSec[2]*featuresFs, length(lfpDF$timeStamp))
            # # p <- plot_ly(data=muaCountDF[fromSample:toSample,], 
             # #              x=~time, y=~muaCount[,input$detailChannel], 
            muaCountDFToPlot <- data.frame(time=featuresTimes[fromSample:toSample],
                                            timeStamp=featuresTimeStamps[fromSample:toSample],
                                            muaCount=muaCount[fromSample:toSample, as.numeric(input$detailChannel)])
            p <- plot_ly(data=muaCountDFToPlot, x=~time, y=~muaCount,
                          type='scatter', 
                          mode='lines+markers',
                          marker = list(size=sizePoints),
                          text= ~paste("time: ", time, "\ntimeStamp: ", format(timeStamp, tz=estTz))) %>%
            layout(xaxis=list(title="Time (sec)"), yaxis=list(title="MUA Count"))
            p
        })
        output$pxxPlot <- renderPlotly({
            d <- event_data("plotly_click")
            if(is.null(d)) {
                selectedTimeStamp <- defaultSelectedTimeStamp
            } else {
                selectedTimeStamp <- as.POSIXct(x=d$key)
            }
            # pxxFs <- 1/mean(pxxDF$time[2:length(pxxDF$time)]-
                            # pxxDF$time[1:(length(pxxDF$time)-1)])
show(paste("Updating Pxx at time ", selectedTimeStamp))
            selectedSample <- which.min(abs(selectedTimeStamp-featuresTimeStamps))
            fromSample <- max(selectedSample+input$detailWinSizeSec[1]*featuresFs, 1)
            toSample <- min(selectedSample+input$detailWinSizeSec[2]*featuresFs, length(lfpDF$timeStamp))
            # # p <- plot_ly(data=pxxDF[fromSample:toSample,], 
             # #              x=~time, y=~Pxx[,input$detailChannel],
            selectedFreqIndex <- getSlectedFreqIndex(selectedText=input$detailFreq)
            if(selectedFreqIndex>0) {
                pxxDFToPlot <- data.frame(time=featuresTimes[fromSample:toSample], 
                                           timeStamp=featuresTimeStamps[fromSample:toSample],
                                           pxx=10*log10(pxxDF$pxx[as.numeric(input$detailChannel),selectedFreqIndex,fromSample:toSample]))
                p <- plot_ly(data=pxxDFToPlot, x=~time, y=~pxx,
                                               mode="line+marker",
                                               text=~paste("time: ", time, "\ntimeStamp: ", format(timeStamp, tz=estTz))) %>%
                layout(xaxis=list(title="Time (sec)"), yaxis=list(title="Power (dB)"))
                p
            }
        })
    }
    shinyApp(ui=ui, server=server)
# }

# processAll()

# rm(processAll)

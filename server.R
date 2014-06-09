library(shiny)
library(shinyAce)
library(psych)
library(MASS)



shinyServer(function(input, output) {
    
    
    
    bs <- reactive({
        if (input$rowname == 1) {
            x <- read.csv(text=input$text, sep="\t")
            x <- x[, -1]
        }else{
            x <- read.csv(text=input$text, sep="\t")
        }
        describe(x)[2:13]
    })
    
    
    
    correl <- reactive({
        if (input$rowname == 1) {
            x <- read.csv(text=input$text, sep="\t")
            x <- x[, -1]
        }else{
            x <- read.csv(text=input$text, sep="\t")
        }
        round(cor(cbind(x), use = "complete"),3)
    })
    
    
    
    makecorPlot <- function(){
        if (input$rowname == 1) {
            x <- read.csv(text=input$text, sep="\t")
            x <- x[, -1]
        }else{
            x <- read.csv(text=input$text, sep="\t")
        }
        pairs.panels(x)
    }
    
    output$corPlot <- renderPlot({
        print(makecorPlot())
    })
 
 
    
    correspresult <- reactive({
        
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datca <- as.matrix(dat[,-1])
            results <- corresp(datca, nf=(min(nrow(datca),ncol(datca))-1))

            cat("First canonical correlation(s):", "\n")
            print(round(results$cor, 2))
            cat("\n", "Row scores:", "\n")
            print(round(results$rscore, 2))
            cat("\n", "Column scores:", "\n")
            print(round(results$cscore, 2))

            eigen <- results$cor^2
            eigen <- round(eigen, 3)
            
            Contribution <- 100*(results$cor^2)/sum(results$cor^2)
            
            cat("\n")
            data.frame("Eigen values"=eigen, Contribution)
        
        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datca <- as.matrix(dat)
            results <- corresp(datca, nf=(min(nrow(datca),ncol(datca))-1))
            
            cat("First canonical correlation(s):", "\n")
            print(round(results$cor, 2))
            cat("\n", "Row scores:", "\n")
            print(round(results$rscore, 2))
            cat("\n", "Column scores:", "\n")
            print(round(results$cscore, 2))
            
            eigen <- results$cor^2
            eigen <- round(eigen, 3)
            
            Contribution <- 100*(results$cor^2)/sum(results$cor^2)
            
            cat("\n")
            data.frame("Eigen values"=eigen, Contribution)
        }
    })
    
    
    
    makerowPlot <- function() {
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datca <- as.matrix(dat[,-1])
            results <- corresp(datca, nf=(min(nrow(datca),ncol(datca))-1))
        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datca <- as.matrix(dat)
            results <- corresp(datca, nf=(min(nrow(datca),ncol(datca))-1))
        }

        # Dim1のminとmax（x軸の最大・最小）
        PCLpc1min <- min(results$rscore[,1:2][,1])
        PCLpc1min <- PCLpc1min-(abs(PCLpc1min-PCLpc1min*1.25))
        PCLpc1max <- max(results$rscore[,1:2][,1])
        PCLpc1max <- PCLpc1max*1.25
        # Dim2のminとmax（y軸の最大・最小）
        PCLpc2min <- min(results$rscore[,1:2][,2])
        PCLpc2min <- PCLpc2min-(abs(PCLpc2min-PCLpc2min*1.25))
        PCLpc2max <- max(results$rscore[,1:2][,2])
        PCLpc2max <- PCLpc2max*1.25
        
        plot(results$rscore[,1:2], type="n",xlab="Dimension 1", ylab="Dimesnion 2", xlim=c(PCLpc1min, PCLpc1max), ylim=c(PCLpc2min, PCLpc2max), cex.axis=0.8, cex.lab=0.8)
        text(results$rscore[,1:2], labels=rowvar, cex=0.9, adj=c(0.25,1.5))
        abline(h=0,lty="dotted")
        abline(v=0,lty="dotted")
        title(main="Correspondence Analysis: Row Coordinates")
    }
    
    output$rowPlot <- renderPlot({
        print(makerowPlot())
    })
    
    

    makecolPlot <- function() {
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datca <- as.matrix(dat[,-1])
            results <- corresp(datca, nf=(min(nrow(datca),ncol(datca))-1))
        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datca <- as.matrix(dat)
            results <- corresp(datca, nf=(min(nrow(datca),ncol(datca))-1))
        }
        
        # PC1のminとmax（x軸の最大・最小）
        PCSpc1min <- min(results$cscore[,1:2][,1])
        PCSpc1min <- PCSpc1min-(abs(PCSpc1min-PCSpc1min*1.25))
        PCSpc1max <- max(results$cscore[,1:2][,1])
        PCSpc1max <- PCSpc1max*1.25
        # PC2のminとmax（y軸の最大・最小）
        PCSpc2min <- min(results$cscore[,1:2][,2])
        PCSpc2min <- PCSpc2min-(abs(PCSpc2min-PCSpc2min*1.25))
        PCSpc2max <- max(results$cscore[,1:2][,2])
        PCSpc2max <- PCSpc2max*1.25
        
        plot(results$cscore[,1:2], xlab="Dimension 1", ylab="Dimension 2", type="n", xlim=c(PCSpc1min, PCSpc1max), ylim=c(PCSpc2min, PCSpc2max), cex.axis=0.8,cex.lab=0.8)
        # points(results$cscore[,1:2],col="blue",pch="*",cex=2)
        text(results$cscore[,1:2], labels=rownames(results$cscore),cex=0.9,adj=c(0.25,1.5))
        abline(h=0,lty="dotted"); abline(v=0,lty="dotted")
        title(main="Correspondence Analysis: Column Coordinates")
    }
    
    
    output$colPlot <- renderPlot({
        print(makecolPlot())
    })
    
    
    
    makeBiPlot <- function() {
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datca <- as.matrix(dat[,-1])
            results <- corresp(datca, nf=(min(nrow(datca),ncol(datca))-1))
        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datca <- as.matrix(dat)
            results <- corresp(datca, nf=(min(nrow(datca),ncol(datca))-1))
        }
        
        
        # Dim1のminとmax（x軸の最大・最小）
        BiPlotXmin <- min(c(results$rscore[,1:2][,1], results$cscore[,1:2][,1]))
        BiPlotXmin <- BiPlotXmin*1.25
        BiPlotXmax <- max(c(results$rscore[,1:2][,1], results$cscore[,1:2][,1]))
        BiPlotXmax <- BiPlotXmax*1.25
        # Dim2のminとmax（y軸の最大・最小）
        BiPlotYmin <- min(c(results$rscore[,1:2][,2], results$cscore[,1:2][,2]))
        BiPlotYmin <- BiPlotYmin*1.25
        BiPlotYmax <- max(c(results$rscore[,1:2][,2], results$cscore[,1:2][,2]))
        BiPlotYmax <- BiPlotYmax*1.25
        
        # 主成分負荷量と主成分得点を同時にプロット
        biplot(results$rscore [,1:2], results$cscore [,1:2], xlab="Dimension 1", ylab="Dimension 2", var.axes = F, xlim=c(BiPlotXmin, BiPlotXmax), ylim=c(BiPlotYmin, BiPlotYmax))
        abline(v=0, lty=3) #0で縦に線を引き，破線（lty=3）を引く
        abline(h=0, lty=3) #0で横に線を引き，破線（lty=3）を引く
    }
    
    
    output$makeBiPlot <- renderPlot({
        print(makeBiPlot())
    })
    
    
    
    rscore <- reactive({
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datca <- as.matrix(dat[,-1])
            results <- corresp(datca, nf=(min(nrow(datca),ncol(datca))-1))
            
        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datca <- as.matrix(dat)
            results <- corresp(datca, nf=(min(nrow(datca),ncol(datca))-1))
            
        }
        
        results$rscore
    
    })
    
    
    
    cscore <- reactive({
        if (input$rowname == 1) {
            dat <- read.csv(text=input$text, sep="\t")
            
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datca <- as.matrix(dat[,-1])
            results <- corresp(datca, nf=(min(nrow(datca),ncol(datca))-1))
            
        }else{
            dat <- read.csv(text=input$text, sep="\t")
            rowvar <- rownames(dat)
            datca <- as.matrix(dat)
            results <- corresp(datca, nf=(min(nrow(datca),ncol(datca))-1))
            
        }
        
        results$cscore
        
    })
    
    
    
    
    
    info <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")# バージョン情報
        info2 <- paste("It was executed on ", date(), ".", sep = "")# 実行日時
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info.out <- renderPrint({
        info()
    })





    output$textarea.out <- renderPrint({
        bs()
    })

    output$correl.out <- renderPrint({
        correl()
    })
    
    output$correspresult.out <- renderPrint({
        correspresult()
    })
    
    output$downloadPlot1 <- downloadHandler(
    filename = function() {
        paste('RowPlot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makerowPlot())
		dev.off()
	}
    )
    
    output$downloadPlot2 <- downloadHandler(
    filename = function() {
        paste('ColumnPlot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makecolPlot())
		dev.off()
	}
    )
    
    output$downloadPlot3 <- downloadHandler(
    filename = function() {
        paste('BiPlot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makeBiPlot())
		dev.off()
	}
    )

    output$downloadCorPlot <- downloadHandler(
    filename = function() {
        paste('Corplot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makecorPlot())
		dev.off()
	}
    )
    
    output$downloadData1 <- downloadHandler(
    filename = function() {
        paste('rscore-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
        write.csv(rscore(), file)
    }
    )
    
    output$downloadData2 <- downloadHandler(
    filename = function() {
        paste('cscore-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
        write.csv(cscore(), file)
    }
    )





})

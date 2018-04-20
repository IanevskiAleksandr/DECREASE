options(stringsAsFactors = !1);

shinyServer(function(input, output, session) {
  
  shinyjs::runjs('$(".toHide2").hide(); $(".toHide5").hide();document.title = "DeCREaSE";'); shinyjs::disable("save_results");
  annot <<- NULL;
  
  toastr_info(paste0("This link will expire in ", 30 - abs(difftime(anytime::anydate(file.info(paste0(getwd(),"/ui.R"))$mtime), anytime::anydate(Sys.Date()), units = c("days"))[[1]]),
                     " days."),
              title = "Welcome to DeCREaSE!", closeButton = !0, newestOnTop = !0, progressBar = !0, position = c("top-right"), 
              preventDuplicates = !0, showDuration = 1000, hideDuration = 2000, timeOut = 7000, extendedTimeOut = 1000,
              showEasing = "swing", hideEasing = "swing", showMethod = "fadeIn", hideMethod = "fadeOut")
  
  
  ########################################    On getstarted click    ########################################   
  observeEvent(input$getstarted, {
    showModal(modalDialog(
      title = "DECREASE",
      fluidRow(
        column(5, div(id="tourinpfile",fileInput("fileinp", "Upload your data (*.xlsx, *.txt, *csv)", multiple = F, accept = NULL, width = NULL))),
        column(3, div(id="tourfiletype", selectInput("fileType", "Input file format", choices = c("Tabular","Matrix")))),
        column(3, div(id="tourreadout", selectInput("phenotypicResponse", "Choose readout", choices = c("% cell inhibition", "% cell viability"))))
      ),
	  div(id="tourexpdata",
		  tags$p(tags$b("OR"), "download example data"),
		  downloadButton(outputId = "loadExData", label = "Example data", class = "butEx")), 
      easyClose = !0,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("start", "Start", class = "btn-success")
      ), size = "l"
    ))
    shinyjs::disable("start");
  })
  
  ###########################################    On start click    ###########################################   
  observeEvent(input$start, {

    #source aditional functions
    source("additionalFunctions.R");
    
    # change view
    shinyjs::runjs('$(".hidden").removeClass("hidden");$(".toHide").hide();$(".toHide2").show(); ');
    removeModal();

    updateSelectInput(session, "Graphs", choices = paste0(annot$Drug1, " & ", annot$Drug2))
  })
  
  #######################################    On combination select    ########################################   
  observeEvent(input$Graphs, {
    
    if(input$Graphs != ""){
      # browser();
      
      # selected drugs
      drugs <- unlist(strsplit(input$Graphs, " & ")); 
      data_cell_pair <<- annot[annot$Drug1 == drugs[[1]] & annot$Drug2 == drugs[[2]],]; 
      pair_name <- paste0(data_cell_pair$Drug1[[1]], " & ", data_cell_pair$Drug2[[1]], ".RDS");
      
      if(pair_name %in% list.files()){
        shinyjs::runjs('$(".toHide4").hide();$(".toHide3").show(); ');
        output$plotFull <- renderPlot({
          finalpred <- readRDS(pair_name); finalpred$finalpred_ <- 100 - finalpred$finalpred_;
          PlotRespMatr(reshape2::acast(finalpred, Conc1~Conc2, value.var = "finalpred_"), name = "%inhibition", 
                       d1N = data_cell_pair$Drug1[[1]], d2N = data_cell_pair$Drug2[[1]])$pl
        })
      } else {
        shinyjs::runjs('$(".toHide3").hide(); $(".toHide4").show();');
      }
      # plot sparce matrix to screen
      output$plotSparse <- renderPlot({
        data_cell <- data_cell_pair; data_cell$Response <- 100 - data_cell$Response;
        PlotRespMatr(reshape2::acast(data_cell, Conc1~Conc2, value.var = "Response"), name = "%inhibition", 
                     d1N = data_cell_pair$Drug1[[1]], d2N = data_cell_pair$Drug2[[1]])$pl
      })
    }
  })
  
  #######################################    On start analysis click    ########################################   
  observeEvent(input$startanalysis, {

    shinyjs::runjs('$(".toHide3").hide(); $(".toHide4").hide(); $(".toHide5").show();');
    
    withProgress(message = 'Fitting cNMF and XGBoost', value = 0, {

    # Increment the progress bar, and update the detail text.
    incProgress(2/3, detail = "Running...");

    # call prediction model
    saveRDS(data_cell_pair, "annot.RDS"); source("DeCREaSE.R"); 
    saveRDS(matr_Out, paste0(data_cell_pair$Drug1[[1]], " & ", data_cell_pair$Drug2[[1]], ".RDS"))
    finalpred_ <<- matr_Out
   })
    
    shinyjs::runjs('$(".toHide5").hide(); $(".toHide3").show();');
    
    output$plotFull <- renderPlot({
      finalpred <- finalpred_; finalpred$finalpred_ <- 100 - finalpred$finalpred_;
      PlotRespMatr(reshape2::acast(finalpred, Conc1~Conc2, value.var = "finalpred_"), name = "%inhibition", 
                   d1N = data_cell_pair$Drug1[[1]], d2N = data_cell_pair$Drug2[[1]])$pl
    })
    shinyjs::enable("save_results");
  })
  
  observeEvent(input$startanalysisall, {
    
    shinyjs::runjs('$(".toHide3").hide(); $(".toHide4").hide(); $(".toHide5").show();');
    
    withProgress(message = 'Fitting cNMF and XGBoost', value = 0, {
      
      for(k in 1:length(unique(annot$PairIndex))){

        combiK <- annot[annot$PairIndex == unique(annot$PairIndex)[[k]], ];
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/length(unique(annot$PairIndex)), detail = paste0("Predicting ", combiK$Drug1[[1]], " & ", combiK$Drug2[[1]]));
        
        # call prediction model
        saveRDS(combiK, "annot.RDS"); source("DeCREaSE.R"); 
        saveRDS(matr_Out, paste0(combiK$Drug1[[1]], " & ", combiK$Drug2[[1]], ".RDS"))
      }
    })   
    
    finalpred_ <<- readRDS(paste0(data_cell_pair$Drug1[[1]], " & ", data_cell_pair$Drug2[[1]], ".RDS"))
  
    shinyjs::runjs('$(".toHide5").hide(); $(".toHide3").show();');
    
    output$plotFull <- renderPlot({
      finalpred <- finalpred_; finalpred$finalpred_ <- 100 - finalpred$finalpred_;
      PlotRespMatr(reshape2::acast(finalpred, Conc1~Conc2, value.var = "finalpred_"), name = "%inhibition", 
                   d1N = data_cell_pair$Drug1[[1]], d2N = data_cell_pair$Drug2[[1]])$pl
    })
    enable("save_results");
  })

  ############################################    On file load    ############################################   
  observeEvent(input$fileinp, {
    
    # Check that data object exists and is data frame.
    if (!is.null(input$fileinp$name)) {
      if (tools::file_ext(input$fileinp$name) %in% c("xlsx", "txt", "csv")) {
        
        # source file load functions
        source("getData.R")
        
        tryCatch({

          #browser();
          if(input$fileType == "Tabular" && tools::file_ext(input$fileinp$name) %in% c("txt", "csv", "xlsx"))
          {
            if (tools::file_ext(input$fileinp$name) == 'xlsx') annot <<- openxlsx::read.xlsx(input$fileinp$datapath)
            else if (tools::file_ext(input$fileinp$name) %in% c("txt", "csv")) annot <<- read.table(file = input$fileinp$datapath, header = T, sep=",", row.names = NULL, fill = T)

            # take care of NA's and empty rows/cols       
            annot <<- data.frame(lapply(annot, as.character), stringsAsFactors=F)
            annot <<- annot[!apply(is.na(annot) | annot == "", 1, all),] # rows with all NA
            annot <<- annot[,!apply(is.na(annot) | annot == "", 2, all)] # cols with all NA
            annot$Conc1 = as.numeric(as.character(annot$Conc1)); annot$Conc2 = as.numeric(as.character(annot$Conc2)); annot$Response = as.numeric(as.character(annot$Response));
            enable("start"); 
            
          } else if (input$fileType == "Matrix" && tools::file_ext(input$fileinp$name) %in% c("txt", "csv", ".xlsx")) {
            
            if (ext == 'xlsx') annot <<- openxlsx::read.xlsx(input$annotfile$datapath, colNames = F)
            else if (ext %in% c("txt", "csv")) annot <<- read.table(file = input$annotfile$datapath, header = F, sep =",",  row.names = NULL, fill = T)
            
            # take care of NA's and empty rows/cols       
            annot <<- data.frame(lapply(annot, as.character), stringsAsFactors=F)
            annot <<- annot[!apply(is.na(annot) | annot == "", 1, all),] # rows with all NA
            annot <<- annot[,!apply(is.na(annot) | annot == "", 2, all)] # cols with all NA
            
            D1 = sum(grepl("Drug1:", annot[,1])); D2= sum(grepl("Drug2:", annot[,1])); ConcUn = sum(grepl("ConcUnit:", annot[,1]))
            enable("start"); 
          }
          
          annot$Conc1 <<- as.numeric(annot$Conc1); annot$Conc2 <<- as.numeric(annot$Conc2); annot$Response <<- as.numeric(annot$Response);
        })
        
        # convert to viability
        if(input$phenotypicResponse == "% cell inhibition") annot$Response <<- 100 -annot$Response

        # check whether enable export button
        if(any(sapply(1:length(unique(annot$PairIndex)), function(k){
          combiK <- annot[annot$PairIndex == unique(annot$PairIndex)[[k]], ];
          file.exists(paste0(combiK$Drug1[[1]], " & ", combiK$Drug2[[1]], ".RDS"))
        }))){enable("save_results");}
        
      } else {
        warning("wrong file format")
      }
    }
  })
  
  # Upload example data
  output$loadExData <- downloadHandler(
    filename = function(){ paste0("ExampleData.zip") },
    content = function(file){ file.copy("ExampleData.zip", file)},
    contentType = NULL
  )
  
  observeEvent(input$save_results,{
    
    #browser();  browser(); browser(); browser();
    pairsCalculated_ <- as.character(na.omit(sapply(1:length(unique(annot$PairIndex)), function(k){ combiK <- annot[annot$PairIndex == unique(annot$PairIndex)[[k]], ];
      if(file.exists(paste0(combiK$Drug1[[1]], " & ", combiK$Drug2[[1]], ".RDS"))) {
        paste0(combiK$Drug1[[1]], " & ", combiK$Drug2[[1]])} else {NA}})))
    
    showModal(modalDialog(
      title = "Choose options",
      
      fluidRow(column(width = 8, offset = 1,
             selectizeInput("toExportDrugs","Choose drug pairs", choices = pairsCalculated_, multiple = T, selected = pairsCalculated_[1:length(pairsCalculated_)], width="100%")
      ), column(width = 2, offset = 0, br(), downloadButton("downloadPDF", label = "Download *.pdf"))), hr(),

      fluidRow(column(2, offset = 1, br(), downloadButton("downloadMatrices", label = "Download (.xlsx)")),
               column(2, offset = 0, br(), downloadButton("downloadMatrices2", label = "Download (.csv)")),
               column(2, offset = 0, br(), downloadButton("downloadMatrices3", label = "Download (.txt)")),
               column(3, offset = 1, selectInput("compatib", label = "Compatible with:", choices = c("SynergyFinder","Combenefit"), selected = "SynegyFinder"))
      ),

      easyClose = !0,
      size = "l"
    ))
    shinyjs::runjs('$("#downloadMatrices").removeClass("btn-default"); $("#downloadMatrices").addClass("btn-outline-secondary");
                     $("#downloadMatrices2").removeClass("btn-default"); $("#downloadMatrices2").addClass("btn-outline-secondary");
                     $("#downloadMatrices3").removeClass("btn-default"); $("#downloadMatrices3").addClass("btn-outline-secondary");
                     $("#downloadPDF").removeClass("btn-default"); $("#downloadPDF").addClass("btn-outline-secondary");')
    
    output$downloadPDF <- downloadHandler(
      filename <- function() paste0("result_", Sys.Date(),".pdf"),
      content <- function(file){

      pdf("fileOut.pdf", width = 12, height = 7, onefile = !0)
        for(k in 1:length(input$toExportDrugs)){
          combiK <- annot[annot$Drug1 == unlist(strsplit(input$toExportDrugs[k], " & "))[1] & annot$Drug2 == unlist(strsplit(input$toExportDrugs[k], " & "))[2], ];
          combiApprox <- readRDS(paste0(combiK$Drug1[[1]], " & ", combiK$Drug2[[1]], ".RDS"))
          
          gridExtra::grid.arrange(
           PlotRespMatr(100 - reshape2::acast(combiApprox, Conc1~Conc2, value.var = "Response"), name = "%inhibition",
                        d1N = data_cell_pair$Drug1[[1]], d2N = data_cell_pair$Drug2[[1]])$pl,
           PlotRespMatr(100 - reshape2::acast(combiApprox, Conc1~Conc2, value.var = "finalpred_"), name = "%inhibition",
                       d1N = data_cell_pair$Drug1[[1]], d2N = data_cell_pair$Drug2[[1]])$pl, ncol = 2)
        }
      dev.off()
        
      file.copy("fileOut.pdf", file)
      },
      contentType = NULL
    )
    
    # create output compatible with synergyfinder
    synergyMakeFinal <- function(){
      do.call("rbind", lapply(1:length(input$toExportDrugs), function(k){
        combiK <- annot[annot$Drug1 == unlist(strsplit(input$toExportDrugs[k], " & "))[1] & annot$Drug2 == unlist(strsplit(input$toExportDrugs[k], " & "))[2], ];
        combiApprox <- readRDS(paste0(combiK$Drug1[[1]], " & ", combiK$Drug2[[1]], ".RDS"))
        combiApprox$Drug1 = combiK$Drug1[[1]]; combiApprox$Drug2 = combiK$Drug2[[1]]; combiApprox$PairIndex = k; 
        combiApprox$Response = combiApprox$finalpred_; combiApprox$ConcUnit = combiK$ConcUnit[[1]]; combiApprox$Response = 100 - combiApprox$Response;
        combiApprox
      }))
    }
    
    output$downloadMatrices <- downloadHandler(
      filename <- function() paste0("result_", Sys.Date(),".xlsx"),
      content <- function(file){
        out <- synergyMakeFinal();
        openxlsx::write.xlsx(out[,c("PairIndex","Drug1","Drug2","Conc1","Conc2","Response","ConcUnit")], "fileOut.xlsx"); file.copy("fileOut.xlsx", file)
      },
      contentType = NULL
    )
    output$downloadMatrices2 <- downloadHandler(
      filename <- function() paste0("result_", Sys.Date(),".csv"),
      content <- function(file){
        out <- synergyMakeFinal();
        write.csv(out[,c("PairIndex","Drug1","Drug2","Conc1","Conc2","Response","ConcUnit")], "fileOut.csv", row.names=!1); file.copy("fileOut.csv", file)
      },
      contentType = NULL
    )
    output$downloadMatrices3 <- downloadHandler(
      filename <- function() paste0("result_", Sys.Date(),".txt"),
      content <- function(file){
        out <- synergyMakeFinal();
        write.csv(out[,c("PairIndex","Drug1","Drug2","Conc1","Conc2","Response","ConcUnit")], "fileOut.txt", row.names=!1); file.copy("fileOut.txt", file)
      },
      contentType = NULL
    )    
  })
  
  	#############################################################################################
	#### Tour
	########################
	
	observeEvent(input$inpfilepreloadtour,{
		name_ = "exampleTabular.xlsx"
		annot <<- openxlsx::read.xlsx("./ExampleData/exampleTabular/exampleTabular.xlsx")
		if(input$phenotypicResponse == "% cell inhibition") annot$Response <<- 100 - annot$Response
		shinyjs::runjs(paste0('$("#fileinp_progress").html(\'<div class="progress-bar" style="width: 100%;">Upload complete</div>\').css("visibility", "visible");
					   $(".form-control").val("', name_,'");'));
		enable("start");
	})
  
	observeEvent(input$showcomboplot,{
		source("additionalFunctions.R");
		finalpred = readRDS("examplecombi.RDS"); finalpred$finalpred_ = 100 - finalpred$finalpred_;
		output$plotFull <- renderPlot({
		  PlotRespMatr(reshape2::acast(finalpred, Conc1~Conc2, value.var = "finalpred_"), name = "%inhibition", 
					   d1N = data_cell_pair$Drug1[[1]], d2N = data_cell_pair$Drug2[[1]])$pl
		})
	})

	session$onSessionEnded(function(){
	  stopApp(); quit("no");
	})
})
shiny::onStop(function(){  quit("no"); })
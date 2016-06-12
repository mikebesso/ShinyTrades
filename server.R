


#EventTableRowsSelected <- LevelEditor$TableRowsSelected();




shinyServer(
  function(input, output, session) {



    # formData <- reactive({LevelEditor$ServerInitializeFormData(input)});



     # Click "Submit" button -> save data
      # observeEvent(
      #   input$submit,
      #   {
      #     ID <- isolate(input$data_ID);
      #     if (ID != "0") {
      #       LevelData$CrudUpdate(isolate(formData()), ID);
      #     } else {
      #       LevelData$CrudCreate(isolate(formData()));
      #       LevelEditor$ServerUpdateInputs(LevelData$DefaultAsList(), session)
      #     }
      #   },
      #   priority = 1
      # );



      #  observeEvent(
      #    input[[EventTableRowsSelected]],
      #   {
      #     if (length(input[[EventTableRowsSelected]]) > 0) {
      #       .rowSelected <- input[[EventTableRowsSelected]][1];
      #       data <- LevelData$CrudRead(.rowSelected);
      #       LevelEditor$serverUpdateInputs(data, session)
      #     }
      #   }
      # )
      #


#
#       # render table
#       # Note:  We need to process on the server so that rownames are returned
#       # as the selected rows.
#       output[[LevelEditor$TableID()]] <- DT::renderDataTable(
#         {
#            #update after submit is clicked
#            input$submit;
#
#            #update after delete is clicked
#            input$delete;
#
#            LevelData$FilterData(input$AssetClass, input$AsOfDate);
#          },
#          server = TRUE,
#          rownames = FALSE,
#          options = list(pageLength = 25, searching= FALSE),
#          selection = "single",
#          colnames = LevelData$MetaData()$Fields()
#       )
#
#
#       output[[LevelViewer$TableID]] <- DT::renderDataTable(
#         {
#           AsOfDate <- input$LevelViewer_AsOfDate;
#           df <- LevelData$Load(AsOfDate);
#           df
#         },
#         server = TRUE,
#         rownames = FALSE,
#         options = list(pageLength = 25, searching= FALSE),
#         selection = "single",
#         colnames = LevelData$MetaData()$Fields()
#       )


      #################################
      SideBar$Server(input, output, session);
      Scraper$Server(input, output, session);
      BackTest$Server(input, output, session);
      Console$Server(input, output, session);




  }




)




Panel <-
  list(
    Input = function(title = NULL, ...){
      titleTag <- NULL
      if (HasValue(title)) {
        titleTag <- h3(class = "box-title", title)
      }
      return(div(class = "shiny-input-panel", titleTag, flowLayout( ...)));
    }
  )



Box <-
  list(
    Input = function(
      ...,
      title = NULL,
      footer = NULL,
      status = NULL,
      solidHeader = FALSE,
      background = NULL,
      width = 6,
      height = NULL,
      collapsible = FALSE,
      collapsed = FALSE
    ){
      box(
        div(class = "shiny-input-panel", flowLayout( ...)),
        title = title,
        footer = footer,
        status = status,
        solidHeader = solidHeader,
        background = background,
        width = width,
        height = height,
        collapsible = collapsible,
        collapsed = collapsed
      )
    },

    Result = function(id, width = 4){
      valueBoxOutput(outputId = id, width);
    },

    ResultHTML = function(
      value,
      subtitle,
      icon = NULL,
      color = "aqua"
    ){
      valueBox(
        value = value,
        subtitle = subtitle,
        icon = icon,
        color = color,
        href = NULL
      )
    },

    Render = function(expr, env = parent.frame(), quoted = FALSE)
    {
      vbox_fun <- shiny::exprToFunction(expr, env, quoted)
      shiny::renderUI({
        vbox <- vbox_fun()
        shinydashboard:::tagAssert(vbox, type = "div")
        vbox$children[[1]]
      })
    }

  )





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
      infoBoxOutput(outputId = id, width);
    },

    ResultHTML = function(
      value,
      title = "",
      subtitle = NULL,
      icon = NULL,
      color = "aqua"
    ){
      infoBox(
        title = title,
        value = value,
        subtitle = subtitle,
        icon = icon,
        color = color,
        fill = TRUE,
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


  # http://fontawesome.io/icons/
  # http://getbootstrap.com/components/#glyphicons

  Icon <- list(
    thumbs = list(
      up = icon("thumbs-up"),
      down = icon("thumbs-down")
    ),
    hand = list(
      rock = icon("hand-rock"),
      paper = icon("hand-paper"),
      scissors = icon("hand-scissors"),
      spock = icon("hand-spock"),
      lizard = icon("hand-lizard")
    ),
    currency = list(
      money = icon("money"),
      dollar = icon("dollar"),
      usd = icon("usd"),
      yen = icon("yen"),
      euro = icon("eur"),
      gpb = icon("gbp")
    ),
    math = list(
      percent = icon("percent"),
      plus = icon("plus"),
      minus = icon("minus"),
      times = icon("times")
    )
  );



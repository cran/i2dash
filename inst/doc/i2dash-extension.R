## ----style, echo = FALSE, rcdesults = 'asis', include = FALSE-----------------
BiocStyle::markdown()

## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    error = FALSE,
    warning = FALSE,
    message = FALSE
)
stopifnot(requireNamespace("htmltools"))
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

library(magrittr)

## ----fig-1, fig.cap = "Figure 1: Final customized and reusable component with Shiny input widgets.", eval = TRUE, echo = FALSE, out.width = "80%"----
knitr::include_graphics("https://github.com/loosolab/i2dash/blob/master/vignettes/images/scatter3D_result_shiny.png?raw=true")

## ----fig-2, fig.cap = "Figure 2: Directory structure for an extension R package.", eval = TRUE, echo = FALSE, out.width = "80%"----
knitr::include_graphics("https://github.com/loosolab/i2dash/blob/master/vignettes/images/file_structure.png?raw=true")

## ----fig-3, fig.cap = "Figure 3:", eval = TRUE, echo = FALSE, out.width = "80%"----
knitr::include_graphics("https://github.com/loosolab/i2dash/blob/master/vignettes/images/RStudio_new_project.png?raw=true")

## ----fig-4, fig.cap = "Figure 4:", eval = TRUE, echo = FALSE, out.width = "80%"----
knitr::include_graphics("https://github.com/loosolab/i2dash/blob/master/vignettes/images/RStudio_new_project2.png?raw=true")

## ----fig-5, fig.cap = "Figure 5:", eval = TRUE, echo = FALSE, out.width = "80%"----
knitr::include_graphics("https://github.com/loosolab/i2dash/blob/master/vignettes/images/RStudio_new_project3.png?raw=true")

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages(c("i2dash","magrittr","dplyr","assertive.types","plotly"))

## ---- eval=FALSE--------------------------------------------------------------
#  #' magrittr forward-pipe operator
#  #'
#  #' See \code{\link[magrittr]{\%>\%}}.
#  #' @name %>%
#  #' @importFrom magrittr %>%
#  #' @export %>%
#  NULL
#  
#  #' magrittr forward-backward-pipe operator
#  #'
#  #' See \code{\link[magrittr]{\%<>\%}}.
#  #' @name %<>%
#  #' @importFrom magrittr %<>%
#  #' @export %<>%
#  NULL

## ----fig-6, fig.cap = "Figure 6: Concept of extending i2dash.", eval = TRUE, echo = FALSE, out.width = "80%"----
knitr::include_graphics("https://github.com/loosolab/i2dash/blob/master/vignettes/images/concept1.png?raw=true", error = FALSE)

## ----fig-7, fig.cap = "Figure 7: Our example project applied to this concept.", eval = TRUE, echo = FALSE, out.width = "80%"----
knitr::include_graphics("https://github.com/loosolab/i2dash/blob/master/vignettes/images/concept2.png?raw=true", error = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  #' Validate the data and create a Rmd string of a component containing a 3D scatter plot.
#  #'
#  #' @param i2dashboard The i2dashboard class object.
#  #' @param x Numeric observations mapped to the x-axis (numeric vector or data.frame).
#  #' @param y Numeric observations mapped to the y-axis (numeric vector or data.frame).
#  #' @param z Numeric observations mapped to the z-axis (numeric vector or data.frame).
#  #' @param x_title The title of the x-axis.
#  #' @param y_title The title of the y-axis.
#  #' @param z_title The title of the z-axis.
#  #' @param colour The title of the y-axis.
#  #' @param title The title of the component
#  #'
#  #' @return An Rmarkdown sting for the rendered component.
#  #' @export
#  scatter3D <- function(i2dashboard, x, y, z, colour = NULL, x_title = NULL, y_title = NULL, z_title = NULL, title = NULL){
#    # see contetent below
#  }
#  

## ---- eval=FALSE--------------------------------------------------------------
#    library(assertive.types)
#    library(dplyr)
#  
#    # validate required parameters
#  
#    # convert vectors to data.frames:
#    if(is.numeric(x)) x <- data.frame(X = x)
#    if(is.numeric(y)) y <- data.frame(Y = y)
#    if(is.numeric(z)) z <- data.frame(Z = z)
#    if(is.character(colour) | is.numeric(colour) | is.factor(colour)) colour <- data.frame("colour" <- colour)
#  
#    assertive.types::assert_is_data.frame(x)
#    assertive.types::assert_is_data.frame(y)
#    assertive.types::assert_is_data.frame(z)
#  
#    # select columns only containing numeric or integer values
#    x %<>%
#      as.data.frame() %>%
#      dplyr::select_if(function(col) is.integer(col) | is.numeric(col))
#    y %<>%
#      as.data.frame() %>%
#      dplyr::select_if(function(col) is.integer(col) | is.numeric(col))
#    z %<>%
#      as.data.frame() %>%
#      dplyr::select_if(function(col) is.integer(col) | is.numeric(col))
#  
#    # provide column names
#    if(is.null(colnames(x))) colnames(x) <- paste0("X_", 1:ncol(x))
#    if(is.null(colnames(y))) colnames(y) <- paste0("Y_", 1:ncol(y))
#    if(is.null(colnames(z))) colnames(z) <- paste0("Z_", 1:ncol(z))
#  
#    # check correct dimensions
#    if(nrow(x) != nrow(y)) stop("The number of rows in 'x' and 'y' is not equal.")
#    if(nrow(x) != nrow(z)) stop("The number of rows in 'x' and 'z' is not equal.")
#  
#    # validate optional parameters
#    if(!is.null(colour)){
#      assertive.types::assert_is_data.frame(colour)
#      colour %<>%
#        as.data.frame() %>%
#        dplyr::select_if(function(col) is.integer(col) | is.numeric(col) | is.factor(col))
#      if(is.null(colnames(colour))) colnames(colour) <- paste0("Color_", 1:ncol(colour))
#      if(nrow(x) != nrow(colour)) stop("The number of rows in 'x' and 'colour' is not equal.")
#    }
#    # check titles
#    if(!is.null(title)) assertive.types::is_character(title)
#    if(!is.null(x_title)) assertive.types::is_character(x_title)
#    if(!is.null(y_title)) assertive.types::is_character(y_title)
#    if(!is.null(z_title)) assertive.types::is_character(z_title)

## ---- eval=FALSE--------------------------------------------------------------
#    # Create random env id
#    env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))
#  
#    # Create component environment and save as .rds file
#    env <- new.env()
#    env$x <- x
#    env$y <- y
#    env$z <- z
#    env$colour <- colour
#    env$x_title <- x_title
#    env$y_title <- y_title
#    env$z_title <- z_title
#  
#    saveRDS(env, file = file.path(i2dashboard@datadir, paste0(env_id, ".rds")))

## ---- eval=FALSE--------------------------------------------------------------
#    # Generate the Rmarkdown string of the component based on the template
#    timestamp <- Sys.time()
#    expanded_component <- knitr::knit_expand(
#      file = system.file("templates", "scatter3D.Rmd", package = "MyExtension"),
#      title = title,
#      env_id = env_id,
#      date = timestamp)
#    return(expanded_component)

## ---- echo = FALSE------------------------------------------------------------
r_chunk <- "```"
htmltools::HTML(paste0(
  "<pre><code>
### {{ title }}

",r_chunk,"{r}
{{ env_id }} <- readRDS(file.path(datadir, '{{ env_id }}.rds'))
is_shiny <- identical(knitr::opts_knit$get('rmarkdown.runtime'), 'shiny')
library(magrittr)
",r_chunk,"
</code></pre>")
)

## ----fig-8, fig.cap = "Figure 8", eval = TRUE, echo = FALSE, out.width = "80%"----
knitr::include_graphics("https://github.com/loosolab/i2dash/blob/master/vignettes/images/scatter3D_result_static.png?raw=true")

## ---- echo = FALSE------------------------------------------------------------
r_chunk <- "```"
htmltools::HTML(paste0(
  "<pre>
",r_chunk,"{r, eval=!is_shiny}
# set variables 
# the first column is always used
x_{{ env_id }} <- {{ env_id }}$x[,1]
y_{{ env_id }} <- {{ env_id }}$y[,1]
z_{{ env_id }} <- {{ env_id }}$z[,1]
if(!is.null({{ env_id }}$x_title)) x_title_{{ env_id }} <- {{ env_id }}$x_title else x_title_{{ env_id }} <- colnames({{ env_id }}$x)[1]
if(!is.null({{ env_id }}$y_title)) y_title_{{ env_id }} <- {{ env_id }}$y_title else y_title_{{ env_id }} <- colnames({{ env_id }}$y)[1]
if(!is.null({{ env_id }}$z_title)) z_title_{{ env_id }} <- {{ env_id }}$z_title else z_title_{{ env_id }} <- colnames({{ env_id }}$z)[1]
if(!is.null({{ env_id }}$colour)) colour_{{ env_id }} <- {{ env_id }}$colour[,1] else colour_{{ env_id }} <- NULL

# creating the plot object
plot_{{ env_id }} <- plotly::plot_ly(
  x = x_{{ env_id }}, 
  y = y_{{ env_id }}, 
  z = z_{{ env_id }}, 
  color = colour_{{ env_id }}) %>%
  plotly::add_markers() %>% 
  plotly::layout(scene = list(
    xaxis = list(title = x_title_{{ env_id }}),
    yaxis = list(title = y_title_{{ env_id }}),
    zaxis = list(title = z_title_{{ env_id }})
  ))

# Provide data for download
if(is.null(colour_{{ env_id }})){
  df_{{ env_id }} <- data.frame(x = x_{{ env_id }}, 
                                y = y_{{ env_id }}, 
                                z = z_{{ env_id }})
} else {
  df_{{ env_id }} <- data.frame(x = x_{{ env_id }}, 
                                y = y_{{ env_id }}, 
                                z = z_{{ env_id }},
                                colour = colour_{{ env_id }})
}
htmltools::div(style='display:block;float:left;width:100%;height:90%;',
  htmltools::tags$button(i2dash::embed_var(df_{{ env_id }})), 
  plot_{{ env_id }}
)
",r_chunk,"
</pre>")
)

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::document(".")
#  devtools::install(".", quick = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  library(magrittr)
#  library(i2dash)
#  
#  # Create i2dashboard object
#  # & add the component to the default page
#  MyDashboard <- i2dashboard(
#    title= "Test extension",
#    datadir = ".",
#    interactive = FALSE
#  ) %>% add_component(
#    component = MyExtension::scatter3D,
#    x = mtcars$mpg,
#    y = mtcars$wt,
#    z = mtcars$hp,
#    colour = as.factor(mtcars$cyl),
#    title = "My component")
#  
#  # Assemble the dashboard
#  assemble(MyDashboard, file = "MyDashboard.Rmd")

## ----fig-9, fig.cap = "Figure 9: Final customized and reusable component with Shiny input widgets.", eval = TRUE, echo = FALSE, out.width = "80%"----
knitr::include_graphics("https://github.com/loosolab/i2dash/blob/master/vignettes/images/scatter3D_result_shiny.png?raw=true")

## ---- echo = FALSE------------------------------------------------------------
r_chunk <- "```"
htmltools::HTML(paste0(
  "<pre>
",r_chunk,"{r, eval=is_shiny}
#
# shiny input widgets
#
ui_list <- list()

# shiny input widget for x
if (ncol({{ env_id }}$x) > 1){
  ui_list <- rlist::list.append(ui_list,
                                selectInput('input_x_{{ env_id }}', label = 'Select data for x axis:',
                                            choices = colnames({{ env_id }}$x)))
}

# shiny input widget for y
if (ncol({{ env_id }}$y) > 1){
  ui_list <- rlist::list.append(ui_list,
                                selectInput('input_y_{{ env_id }}', label = 'Select data for y axis:',
                                            choices = colnames({{ env_id }}$y)))
}

# shiny input widget for size
if (ncol({{ env_id }}$z) > 1){
  ui_list <- rlist::list.append(ui_list,
                                selectInput('input_z_{{ env_id }}', label = 'Select data for the z axis:',
                                            choices = colnames({{ env_id }}$z)))
}

# shiny input widget for colour_by
if (!is.null({{ env_id }}$colour)){
  if(ncol({{ env_id }}$colour) > 1)
  ui_list <- rlist::list.append(ui_list,
                                selectInput('input_colour_{{ env_id }}', label = 'Select metadata for colouring:',
                                            choices = colnames({{ env_id }}$colour)))
}

#
# shiny download button
#
ui_list <- rlist::list.append(ui_list, tags$div(tags$br(), downloadButton('downloadData_{{ env_id }}', 'Download data')))

#
# Handle inputs
#
x_{{ env_id }} <- shiny::reactive({
  if(ncol({{ env_id }}$x) == 1){
    data <- {{ env_id }}$x[[1]]
    title <- colnames({{ env_id }}$x)[1]
    return(list(data = data, title = title))
  } else {
    data <- {{ env_id }}$x[[input$input_x_{{ env_id }}]]
    title <- input$input_x_{{ env_id }}
    return(list(data = data, title = title))
  }
})

y_{{ env_id }} <- shiny::reactive({
  if(ncol({{ env_id }}$y) == 1){
    data <- {{ env_id }}$y[[1]]
    title <- colnames({{ env_id }}$y)[1]
    return(list(data = data, title = title))
  } else {
    data <- {{ env_id }}$y[[input$input_y_{{ env_id }}]]
    title <- input$input_y_{{ env_id }}
    return(list(data = data, title = title))
  }
})

z_{{ env_id }} <- shiny::reactive({
  if(ncol({{ env_id }}$z) == 1){
    data <- {{ env_id }}$z[[1]]
    title <- colnames({{ env_id }}$z)[1]
    return(list(data = data, title = title))
  } else {
    data <- {{ env_id }}$z[[input$input_z_{{ env_id }}]]
    title <- input$input_z_{{ env_id }}
    return(list(data = data, title = title))
  }
})

colour_{{ env_id }} <- shiny::reactive({
  if(!is.null({{ env_id }}$colour)){
    if(ncol({{ env_id }}$colour) == 1){
      return({{ env_id }}$colour[[1]])
    } else {
      return({{ env_id }}$colour[[input$input_colour_{{ env_id }}]])
    }
  } else {
    return(NULL)
  }
})

#
# Download data.frame
#
output$downloadData_{{ env_id }} <- downloadHandler(
  filename =  paste('data-', Sys.Date(), '.csv', sep=''),
  content = function(file) {
    if(is.null(colour_{{ env_id }}()$colour)){
      df <- data.frame(
        x = x_{{ env_id }}()$data, 
        y = y_{{ env_id }}()$data, 
        z = z_{{ env_id }}()$data)
    } else {
      df <- data.frame(
        x = x_{{ env_id }}()$data, 
        y = y_{{ env_id }}()$data, 
        z = z_{{ env_id }}()$data, 
        colour = colour_{{ env_id }}())
    }
    write.csv(df, file)
  }
)

#
# reactive for plot creation
#
output$plot_{{ env_id }} <- plotly::renderPlotly({
  if(!is.null({{ env_id }}$y_title)) y_title <- {{ env_id }}$y_title else y_title <- y_{{ env_id }}()$title
  if(!is.null({{ env_id }}$x_title)) x_title <- {{ env_id }}$x_title else x_title <- x_{{ env_id }}()$title
  if(!is.null({{ env_id }}$z_title)) z_title <- {{ env_id }}$z_title else z_title <- z_{{ env_id }}()$title

  plotly::plot_ly(
  x = x_{{ env_id }}()$data, 
  y = y_{{ env_id }}()$data, 
  z = z_{{ env_id }}()$data, 
  color = colour_{{ env_id }}()) %>%
  plotly::add_markers() %>% 
  plotly::layout(scene = list(
    xaxis = list(title = x_title),
    yaxis = list(title = y_title),
    zaxis = list(title = z_title)
  ))
})

#
# Layout of component
#
shiny::fillRow(flex = c(NA, 1),
      shinyWidgets::dropdownButton(div(style='max-height: 350px; overflow-x: auto;',do.call(shiny::inputPanel, ui_list)),
                       circle = TRUE, status = 'danger', icon = icon('gear'), width = '300px',
                       tooltip = shinyWidgets::tooltipOptions(title = 'Click, to change plot settings:'))
      ,
      plotly::plotlyOutput('plot_{{ env_id }}', height = '100%')
)
",r_chunk,"</pre>")
)

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::document(".")
#  devtools::install(".", quick = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  library(magrittr)
#  library(i2dash)
#  
#  # Create i2dashboard object
#  # & add the component to the default page
#  MyDashboard <- i2dashboard(
#    title= "Test extension",
#    datadir = ".",
#    interactive = TRUE
#  ) %>% add_component(
#    component = MyExtension::scatter3D,
#    x = mtcars,
#    y = mtcars,
#    z = mtcars,
#    colour = as.factor(mtcars$cyl),
#    title = "My component")
#  
#  # Assemble the dashboard
#  assemble(MyDashboard, file = "MyDashboard.Rmd")

## ----sessioninfo--------------------------------------------------------------
sessionInfo()


#' Add content to an \linkS4class{i2dashboard} object.
#'
#' Content can be added to the dashboards pages, the sidebar or the navigation bar.
#'
#' The options to add content in detail:
#' \itemize{
#'   \item \strong{'\code{add_component()}'} adds content to a page of the dashboard by evaluating a function, or by including an object, a text or image file.
#'   \item \strong{'\code{add_to_sidebar()}'} adds content to the dashboards global sidebar or to a pages local sidebar.
#'   \item \strong{'\code{add_link()}'} adds a link to the navigation bar.
#'   \item \strong{'\code{add_colormap()}'} adds a global color mapping to the dashboards colormaps.
#' }
#'
#' The mechanism to add different types of content to a dashboards page or sidebar depends on the class of the object passed to the function '\code{add_component()}' or '\code{add_to_sidebar()}':
#'
#' \itemize{
#'   \item A function will be evaluated and its return value is used as content.
#'   \item A string that ends with \code{.md} or \code{.txt} will be used to open a file and use its content.
#'   \item A string that ends with \code{\\.[png|jpg|jpeg|gif]} will be used to include an image as content.
#'   \item An R object (e.g. an 'htmlwidget') will be included if a suitable signature method is implemented.
#' }
#'
#' @param dashboard A \linkS4class{i2dashboard}.
#' @param component An R object, function, or string.
#' @param page The name of the page to add the component or sidebar to.
#' @param copy Whether or not to copy images to \code{dashboard@datadir}.
#' @param ... Additional parameters passed to the components render function. In case of an image, parameters \code{height} and \code{width} can be used to define the dimensions of the image with CSS or provide an alternative text with \code{image_alt_text}.
#'
#' @return Returns the modified \linkS4class{i2dashboard} object.
#' \itemize{
#'   \item '\code{add_component()}' extends the list of components of the respective page, stored in the '\code{pages}' slot, by an R Markdown string containing the provided content.
#'   \item '\code{add_to_sidebar()}' extends either the '\code{sidebar}' slot or the '\code{sidebar}' entry of a single page by an R Markdown string containing the provided content.
#'   \item '\code{add_link()}' extends the '\code{navbar}' slot by a list of link properties.
#'   \item '\code{add_colormap()}' extends the '\code{colormaps}' slot by the new color mapping.
#' }
#' If something went wrong during the addition, the unmodified \linkS4class{i2dashboard} object is returned.
#'
#' @rdname i2dashboard-content
#' @examples
#' library(magrittr)
#' i2dashboard() -> dashboard
#' myFunction <- function(dashboard) paste0("### Generate component\n\n",
#'     "Lorem ipsum dolor sit amet\n")
#'
#' dashboard %<>% add_component(component = myFunction)
#' dashboard %<>% add_component(component = plotly::plot_ly(mtcars, x=~wt, y=~hp),
#'     title = "Include htmlwidget")
#'
#' ## Similarly, it is possible to insert files and images into components:
#' ## dashboard %<>% add_component(component = "sample.txt", title = "Include text")
#' ## dashboard %<>% add_component(component = "sample.jpg", title = "Include image")
#'
#' dashboard %<>% add_to_sidebar(component = myFunction)
#'
#' ## Similarly, it is possible to insert files and images into the sidebar:
#' ## dashboard %<>% add_to_sidebar(component = "sample.txt", package="i2dash")
#' ## dashboard %<>% add_to_sidebar(component = "sample.jpg", package="i2dash")
#'
#' colors <- c("l1" = "#F7FCFD", "l2" ="#E5F5F9", "l3" = "#CCECE6")
#' dashboard %<>% add_colormap(map = colors, name = "test")
#'
#' dashboard %<>% add_link(href = "www.sample_url.net", title = "MyLink", align = "left")
setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "character"),
          function(dashboard, component, page = "default", copy = FALSE, ...) {
            # Logic to guess intended usage
            mode <- NULL
            if(stringr::str_detect(tolower(component), "\\.[md|txt]+$")) {
              mode <- "text"
            }
            if(stringr::str_detect(tolower(component), "\\.[png|jpg|jpeg|gif]+$")) {
              if(copy) {
                location <- file.path(dashboard@datadir, basename(component))
                file.copy(component, location)
                component <- location
              }
              mode <- "image"
            }

            # validate "page" input
            name <- .create_page_name(page)
            if (!(name %in% names(dashboard@pages))) {
              warning(sprintf("i2dashboard dashboard does not contain a page named '%s'", name))
              return(dashboard)
            }

            if(length(dashboard@pages[[name]]$components) + 1 > dashboard@pages[[name]]$max_components) {
              warning(sprintf("Not enough space left on page '%s'", name))
              return(dashboard)
            }

            component <- switch(mode,
              "text" = render_text(component, ...),
              "image" = render_image(component, ...))

            return(.add_component(dashboard, name, component))
          })

#' @rdname i2dashboard-content
setMethod("add_component", signature(dashboard = "i2dashboard", component = "function"),
          function(dashboard, component, page = "default", ...) {
            # validate "page" input
            name <- .create_page_name(page)
            if (!(name %in% names(dashboard@pages))) {
              warning(sprintf("i2dashboard dashboard does not contain a page named '%s'", name))
              return(dashboard)
            }

            if(length(dashboard@pages[[name]]$components) + 1 > dashboard@pages[[name]]$max_components) {
              warning(sprintf("Not enough space left on page '%s'", name))
              return(dashboard)
            }

            content <- component(dashboard, ...)
            return(.add_component(dashboard, name, content))
          })

#' @rdname i2dashboard-content
setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "gg"),
          definition = function(dashboard, component, page = "default", ...) {
            add_vis_object(dashboard, component, "ggplot2", page, ...) })

#' @rdname i2dashboard-content
setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "gt_tbl"),
          definition = function(dashboard, component, page = "default", ...) {
            add_vis_object(dashboard, component,"gt", page, ...) })

#' @rdname i2dashboard-content
setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "kableExtra"),
          definition = function(dashboard, component, page = "default", ...) {
            add_vis_object(dashboard, component, "kableExtra", page, ...) })

#' @rdname i2dashboard-content
setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "Heatmap"),
          definition = function(dashboard, component, page = "default", ...) {
            add_vis_object(dashboard, component, "ComplexHeatmap", page, ...) })

#' @rdname i2dashboard-content
setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "ANY"),
          definition = function(dashboard, component, page = "default", ...) {

            # HTMLWIDGETS
            if(inherits(component, "htmlwidget")) {
              package <- methods::packageSlot(component)

              if(is.null(package)) {
                warning("No component added, since the package name of the HTML widget could not be determined.")
                return(dashboard)
              }

              return(add_vis_object(dashboard, component, package, page, ...))
            }

            # OTHER
            warning("The component did not inherit from any of the currently supported classes ('htmlwidget').")
            return(dashboard)
          })

#' @param href The target of the link.
#' @param title The link title.
#' @param icon An optional link icon (see https://pkgs.rstudio.com/flexdashboard/using.html#icon-sets)
#' @param align Optional argument that can be “left” or “right” (defaults = “right”) defining the alignment of the links in the navigation bar
#' @param target An optional target (e.g. "_blank")
#'
#' @rdname i2dashboard-content
setMethod("add_link", "i2dashboard", function(dashboard, href, title = NULL, icon = NULL, align = c("right","left"), target = NULL) {
  align <- match.arg(align)
  if(is.null(title) & is.null(icon)) {
    warning("Both, title and icon, cannot be NULL when adding a link.")
    return(dashboard)
  }

  # Workaround for NULL values
  if(is.null(icon)) {
    icon <- ""
  }
  if(is.null(title)) {
    title = ""
  }
  if(is.null(target)) {
    target = ""
  }

  dashboard@navbar <- append(dashboard@navbar, list(list("href" = href, "title" = title, "icon" = icon, "align" = align, "target" = target)))
  dashboard
})

#' A method to embed tabular data into an HTML link for download.
#'
#' @param x Data, which will be written to the embedded file.
#' @param ... Additional parameters.
#'
#' @return HTML code of a hyperlink containing the base64 encoded data.
#' @export
#' @examples
#' embed_var(mtcars)
embed_var <- function(x, ...) {
  f = tempfile(fileext = '.csv')
  utils::write.csv(x, f)
  xfun::embed_file(f, text = 'Download data', ...)
}

#' Method to embed content from a text file in a component/sidebar
#'
#' @param file The file containing the text content.
#' @param title The components title.
#' @param raw Whether or not to emit raw file content
#'
#' @return A character string containing the evaluated component
render_text <- function(file, title = NULL, raw = FALSE) {
  readLines(con = file) %>%
    paste(collapse = "\n") -> content

  if(raw) return(content)
  knitr::knit_expand(file = system.file("templates", "component.Rmd", package = "i2dash"),
                     delim = c("<%", "%>"),
                     content = content,
                     title = title)
}

#' Method to embed an image file in a component
#'
#' @param image The path to the image file.
#' @param image_alt_text The alt text of the image.
#' @param title The components title.
#' @param raw Whether or not to emit solely the markdown image code.
#' @param width Width defined with CSS in the HTML img-tag.
#' @param height Height defined with CSS in the HTML img-tag.
#' @param in_component Whether the image belongs to a component or the sidebar
#'
#' @return A character string containing the evaluated component
render_image <- function(image, image_alt_text = NULL, title = NULL, raw = FALSE, width = "100%", height = "auto", in_component=TRUE) {
  if(is.null(image_alt_text)) {
    image_alt_text <- image
  }
  if(in_component){
    content <- glue::glue("![{image_alt_text}]({image})\n", image_alt_text = image_alt_text, image = image)
  } else {
    content <- glue::glue(as.character(
      htmltools::img(
        src = image,
        alt = image_alt_text,
        style = paste0('height:', height, ';width:', width)
      )),as.character(htmltools::br()))
  }
  if(raw) return(content)
  knitr::knit_expand(file = system.file("templates", "component.Rmd", package = "i2dash"),
                     delim = c("<%", "%>"),
                     content = content,
                     title = title)
}

#' Helper function to add components to the dashboard
#'
#' @param dashboard A \linkS4class{i2dashboard}.
#' @param page The name of the page to add the component to.
#' @param component A string or list.
#'
#' @return The dashboard with added component.
.add_component <- function(dashboard, page, component) {
  if(is.list(component)) {
    assertive.sets::is_subset(c("appendix", "component", "sidebar"), names(component))
    dashboard@pages[[page]]$components <- append(dashboard@pages[[page]]$components, component$component)
    dashboard@pages[[page]]$sidebar <- paste0(dashboard@pages[[page]]$sidebar, component$sidebar)
    # ToDo: Handle appendix
  } else {
    dashboard@pages[[page]]$components <- append(dashboard@pages[[page]]$components, component)
  }
  return(dashboard)
}

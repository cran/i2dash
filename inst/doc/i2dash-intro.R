## ----style, echo = FALSE, results = 'asis', include = FALSE-------------------
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

## ---- eval=FALSE--------------------------------------------------------------
#  remotes::install_git(url = "https://gitlab.gwdg.de/loosolab/software/i2dash.git", repos = BiocManager::repositories())

## ---- eval=TRUE---------------------------------------------------------------
library(i2dash)

## ---- eval=TRUE---------------------------------------------------------------
i2dashboard(
  title = "Dashboard title",
  author = "John Doe",
  interactive = FALSE,
  theme = "yeti") -> dashboard

## ---- eval=TRUE---------------------------------------------------------------
dashboard

## ---- eval=FALSE--------------------------------------------------------------
#  interactivity(dashboard) <- TRUE
#  datadir(dashboard) <- "getwd()"
#  theme(dashboard) <- "cosmo"
#  author(dashboard) <- "John Doe, MaxMustermann"
#  title(dashboard) <- "New dashboard title"

## ---- eval=TRUE---------------------------------------------------------------
dashboard %<>% 
  add_page(
    page = "page1",
    title = "Page 1",
    layout = "focal_left",
    menu = NULL) %>%
  add_page(
    page = "page2",
    title = "Page 2",
    layout = "focal_left",
    menu = NULL)

## ---- eval=TRUE---------------------------------------------------------------
dashboard %<>% 
  add_page(
    page = "page3",
    title = "Lemurs",
    layout = "focal_left",
    menu = "Animals") %>%
  add_page(
    page = "page4",
    title = "Tigers",
    layout = "focal_left",
    menu = "Animals")

## ----fig-1, fig.cap = "Figure 1: Navigation bar of the dashboard after adding several pages.", eval = TRUE, echo = FALSE----
knitr::include_graphics("./images/navigation.png")

## ---- eval = TRUE-------------------------------------------------------------
dashboard %<>%
  remove_page(page = "page2")

## ---- eval=TRUE---------------------------------------------------------------
library(leaflet)
leaflet() %>%
  addTiles() %>%  
  addMarkers(lng=174.768, lat=-36.852,
             popup="The birthplace of R") -> leaftlet_map

dashboard %<>%
  add_component(leaftlet_map,
                page = "page1",
                title = "A map from leaflet")

library(plotly)
plot_ly(mtcars, x = ~wt, y = ~mpg) -> plotly_object

library(ggplot2)
mtcars %>%
  ggplot(aes(x=mpg)) +
  geom_density(fill="darkgrey") -> ggplot2_object

dashboard %<>%
  add_component(plotly_object,
                page = "page1",
                title = "A plot from plotly") %>%
  add_component(ggplot2_object,
                page = "page1",
                title = "A plot from ggplot2")

## ----fig-2, fig.cap = "Figure 2: The resulting page with three components added after assembly of the dashboard.", eval = TRUE, echo = FALSE----
knitr::include_graphics("./images/example_page1.png")

## ---- eval = TRUE-------------------------------------------------------------
dashboard %<>%
  add_component(file.path("images/lemur.jpg"),
                page = "page3",
                title = "A picture of a Lemur") %>%
  add_component(file.path("texts/lemurs.md"),
                page = "page3",
                title = "About Lemurs")

## ----fig-3, fig.cap = "Figure 3: The resulting page with an image and text from a file.", eval = TRUE, echo = FALSE----
knitr::include_graphics("./images/example_page3.png")

## ---- eval = TRUE-------------------------------------------------------------
text_generator <- function(dashboard, n) {
  stringi::stri_rand_lipsum(nparagraphs = n) %>%
    paste(collapse = "\n\n") -> content
  paste0("### Generated content\n\n", content)
}

## ---- eval = TRUE-------------------------------------------------------------
dashboard %<>%
  add_component(text_generator,
                page = "page4",
                n = 4)

## ----fig-4, fig.cap = "Figure 4: The resulting page with content generated from a function.", eval = TRUE, echo = FALSE----
knitr::include_graphics("./images/example_page4.png")

## ---- eval = TRUE-------------------------------------------------------------
# load the `txhousing` dataset
data(txhousing, package = "ggplot2")

# declare `city` as the SQL 'query by' column
tx <- highlight_key(txhousing, ~city)

## ---- eval = TRUE-------------------------------------------------------------
# initiate a plotly object
base <- plot_ly(tx, color = I("black")) %>%
  group_by(city)

# create a time series of median house price
time_series <- base %>%
  group_by(city) %>%
  add_lines(x = ~date, y = ~median)

dot_plot <- base %>%
  summarise(miss = sum(is.na(median))) %>%
  filter(miss > 0) %>%
  add_markers(
    x = ~miss,
    y = ~forcats::fct_reorder(city, miss),
    hoverinfo = "x+y"
  ) %>%
  layout(
    xaxis = list(title = "Number of months missing"),
    yaxis = list(title = "")
  )

## ---- eval = TRUE-------------------------------------------------------------
dashboard %<>%
  add_page(page="page5", layout="2x2_grid", title = "Linked components") %>%
  add_component(page="page5", component=dot_plot) %>%
  add_component(page="page5", component=time_series)

## ----fig-5, fig.cap = "Figure 5: The resulting page with two linked components.", eval = TRUE, echo = FALSE----
knitr::include_graphics("./images/example_page5.png")

## ---- eval = FALSE------------------------------------------------------------
#  year <- factor(c(2014, 2014, 2015, 2017, 2019, 2019), levels = c(2014:2021))

## ---- eval = FALSE------------------------------------------------------------
#  colors <- RColorBrewer::brewer.pal(8, "BuGn")
#  names(colors) <- levels(year)

## ---- eval = FALSE------------------------------------------------------------
#  dashboard %<>%
#    add_colormap(map = colors,
#                 name = "year")

## ---- eval = FALSE------------------------------------------------------------
#  dashboard %>%
#    assemble(file = "MyDashboard.Rmd", pages = c("page1", "page3", "page4", "page5"))

## ----sessioninfo--------------------------------------------------------------
sessionInfo()


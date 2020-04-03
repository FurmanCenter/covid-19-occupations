
theme_fc <- function(...) {

  hrbrthemes::theme_ipsum(
      axis_title_just = "mc",
      subtitle_margin = 10,
      base_size = 11, 
      plot_title_size = 14, 
      subtitle_size = 11, 
      strip_text_size = 11,
      axis_text_size  = 8, 
      axis_title_size = 11
    ) %+replace%
    ggplot2::theme(

      plot.title.position = "plot",

      plot.caption.position = "plot",
      plot.caption = ggplot2::element_text(
        size = 9,
        face = "italic",
        color = "grey40",
        hjust = 0,
        margin = ggplot2::margin(10, 0, 0, 0)
      ),

      legend.position =      "top",
      legend.justification = "left",
      legend.direction =     "horizontal",

      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),

      complete = TRUE
    )
}


# Automate the creation of column plots with error bars. 

col_plot_si <- function(.data, x, y, y_limits, ymin = NULL, ymax = NULL, accuracy = 1) {
  
  number_si <- label_number_si(accuracy)  
  
  p <- ggplot2::ggplot(.data) +
    ggplot2::aes(x = {{x}}, y = {{y}}, ymin = {{ymin}}, ymax = {{ymax}}) +
    ggplot2::geom_col(fill = "#2c7fb8") +
    ggplot2::geom_text(
      aes(y = {{ymax}}, label = number_si({{y}})), 
      vjust = -0.5, size = 3.5
    ) +
    ggplot2::scale_y_continuous(limits = y_limits, labels = label_number_si(accuracy)) +
    theme_fc()
  
  if (!missing(ymin) && !missing(ymax)) {
    p <- p + ggplot2::geom_errorbar()
  }
  
  p
}

col_plot_dollar <- function(.data, x, y, y_limits, ymin = NULL, ymax = NULL, accuracy = 1) {
  
  p <- ggplot2::ggplot(.data) +
    ggplot2::aes(x = {{x}}, y = {{y}}, ymin = {{ymin}}, ymax = {{ymax}}) +
    ggplot2::geom_col(fill = "#2c7fb8") +
    ggplot2::geom_text(
      aes(y = {{ymax}}, label = scales::dollar({{y}}, accuracy = accuracy)), 
      vjust = -0.5, size = 3.5
    ) +
    ggplot2::scale_y_continuous(limits = y_limits, labels = scales::dollar_format(accuracy)) +
    theme_fc()
  
  if (!missing(ymin) && !missing(ymax)) {
    p <- p + ggplot2::geom_errorbar()
  }
  
  p
}

col_plot_pct <- function(.data, x, y, y_limits, ymin = NULL, ymax = NULL, accuracy = 1) {
  
  p <- ggplot2::ggplot(.data) +
    ggplot2::aes(x = {{x}}, y = {{y}}, ymin = {{ymin}}, ymax = {{ymax}}) +
    ggplot2::geom_col(fill = "#2c7fb8") +
    ggplot2::geom_text(
      aes(y = {{ymax}}, label = scales::percent({{y}}, accuracy = accuracy)), 
      vjust = -0.5, size = 3.5
    ) +
    ggplot2::scale_y_continuous(limits = y_limits, labels = scales::percent_format(accuracy)) +
    theme_fc()
  
  if (!missing(ymin) && !missing(ymax)) {
    p <- p + ggplot2::geom_errorbar()
  }
  
  p
}

# Save last plot, then include for markdown

plot_save_include <- function(filename, p = ggplot2::last_plot(), height = 5, width = 9) {

  ggplot2::ggsave(filename, height = height, width = width)
  
  knitr::include_graphics(filename)
}
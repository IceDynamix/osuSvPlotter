source("lib/osu.R")


geom_osuBookmarks <- function(osu) {
  plotElement <- geom_vline(
    color = "dodgerblue",
    xintercept = osu.getBookmarks(osu),
    alpha = 0.5
  )

  return(plotElement)
}


geom_osuBpmPoints <- function(osu) {
  svPoints <- osu.getBpmPoints(osu)
  plotElement <- geom_vline(
    color = "red",
    xintercept = svPoints$time,
    alpha = 0.15
  )
  return(plotElement)
}


geom_osuKiais <- function(osu) {

  svPoints <- osu.getSvPoints(osu)

  # grabs all kiais and orders them into a data frame with start, end
  kiaiSwitches <- osu$timingPoints %>%
    as_tibble() %>%
    filter(effects != lag(effects)) %>%
    pull(time) %>%
    as.integer() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as.data.frame()

  if (nrow(kiaiSwitches) == 0) {
    return()
  }

  colnames(kiaiSwitches) <- c("x1", "x2")

  # The kiai highlights on the output graph are nothing but rectangles
  plotElement <- geom_rect(
    data = kiaiSwitches,
    mapping = aes(
      xmin = x1,
      xmax = x2,
      ymin = 0,
      ymax = max(10e10) # Using pseudo-infinity
    ),
    alpha = 0.1,
    fill = "orange",
    inherit.aes = FALSE
  )

  return(plotElement)
}


geom_osuSvRollingAvg <- function(osu) {
  svPoints <- osu.getSvPoints(osu)
  plotElement <- geom_line(
    data = osu.getSvRollingAverage(svPoints),
    mapping = aes(time, rollingAvg),
    color = "orange"
  )

  return(plotElement)
}
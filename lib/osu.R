source("lib/util.R")
require("dplyr")

osu.parseMap <- function(path) {

  osuFile <- readLines(path)

  # Looks for all lines containing a .ini header and puts them in a vector
  # Also adds the final line to the end
  lineNumbers <- c(grep("^\\[.*\\]", osuFile), length(osuFile))

  # initialize all return values
  mapData <- data.frame()
  timingPoints <- data.frame()
  hitObjects <- data.frame()

  for (section in 1:(length(lineNumbers) - 1)) {

    title <- osuFile[lineNumbers[section]]
    sectionStartLine <- lineNumbers[section] + 1
    sectionEndLine <- lineNumbers[section + 1] - 1
    sectionContent <- osuFile[sectionStartLine:sectionEndLine]

    isKeyValueSection <- title %in% c("[General]", "[Metadata]", "[Editor]", "[Difficulty]")

    if (isKeyValueSection) {
      sectionMapData <- osu.getTable(
          sectionContent = sectionContent,
          separator = ":",
          headers = c("key", "value")
      )

      mapData <- rbind(
          mapData,
          sectionMapData
      )

    } else if (title == "[TimingPoints]") {

      timingPointColumnNames <- c(
          "time", "beatLength", "meter", "sampleSet",
          "sampleIndex", "volume", "uninherited", "effects"
      )

      timingPoints <- osu.getTable(
          sectionContent = sectionContent,
          separator = ",",
          headers = timingPointColumnNames
      )

      timingPoints <- mutate(
        timingPoints,
        value = ifelse(
            uninherited == 0,
            -100 / beatLength,
            60000 / beatLength
          )
        )

    } else if (title == "[HitObjects]") {

      hitObjectColumnNames <- c(
          "x", "y", "time", "type",
          "hitSound", "objectParams"
      )

      hitObjects <- osu.getTable(
          sectionContent = sectionContent,
          separator = ",",
          headers = hitObjectColumnNames
      )
    }


  }

  return(
    list(
      mapData = mapData,
      timingPoints = timingPoints,
      hitObjects = hitObjects
    )
  )

}

osu.getTable <- function(sectionContent, separator, headers) {

  table <- read.table(
        text = sectionContent, # content to parse
        header = FALSE, # our content doesnt have a header
        col.names = headers, # set custom headers
        sep = separator, # separator (":" for map data, "," for timing points and hitobjects)
        strip.white = TRUE, # whitespace should be trimmed
        as.is = TRUE # parse to appropriate data type?
    )

  return(table)
}

osu.getBaseBpm <- function(osu) {

  baseBPM <- osu$timingPoints %>%
    filter(uninherited == 1) %>%
    mutate(duration = lead(time) - time) %>%
    group_by(value) %>%
    summarise(totalDuration = sum(duration)) %>%
    arrange(desc(totalDuration)) %>%
    slice(1) %>%
    select(value) %>%
    as.numeric()

  return(baseBPM)
}


osu.getBookmarks <- function(osu) {
  return(util.parseStringAsNumericVector(util.lookup("Bookmarks", osu$mapData)))
}

osu.getBpmPoints <- function(osu) {
  bpmPoints <- osu$timingPoints %>%
    filter(uninherited == 1) %>%
    select(time, value)
  return(bpmPoints)
}

osu.getSvPoints <- function(osu) {
  bpmPoints <- osu.getBpmPoints(osu)
  svPoints <- osu$timingPoints %>%
    filter(uninherited == 0) %>%
    select(time, value)

  # each bpm point that doesnt have a starting sv should be considered 1x (before unnormalizing ofc)
  for (time in setdiff(bpmPoints$time, svPoints$time)) {
    svPoints <- rbind(svPoints, data.frame(time = time, value = 1))
  }

  # order svPoints after adding new ones
  svPoints <- arrange(svPoints, time)

  # building a vector thats supposed to be a column of svPoints,
  # containing the current bpm for each sv point
  currentBpmList <- NULL

  for (n in 1:length(bpmPoints$time)) {

    left <- bpmPoints$time[n]
    right <- bpmPoints$time[n + 1] # imagine lag()
    currentBPM <- bpmPoints$value[n]

    numberOfAffectedSvPoints <- length(svPoints$time[svPoints$time >= left & svPoints$time < right])
    currentBpmList <- c(currentBpmList, rep(currentBPM, numberOfAffectedSvPoints))
  }

  svPoints$currentBPM <- currentBpmList

  # uses the ratio of base bpm and current bpm to get the base sv value from the normalized value
  svPoints <- svPoints %>%
    mutate(unnormalizedSv = value / (osu.getBaseBpm(osu) / currentBPM))

  return(svPoints)
}

osu.getSvRollingAverage <- function(svPoints) {
  start <- min(svPoints$time)
  end <- max(svPoints$time)

  # increase these sizes to have a more/less sensitive line graph
  # reason im splitting these two variables is to open up the possibility of
  # a rolling shutter thingy

  # using 500 as the base value because its around the timeframe of a note scrolling
  # from top to receptor at scroll 26-28 (iirc)
  binSize <- 500
  step <- 500

  bins <- seq(start, end, by = step)

  rollingAvg <- NULL
  currentSv <- 1

  for (bin in bins) {

    # i know its a useless assignment but helps readability
    startTime <- bin
    endTime <- startTime + binSize

    localSvPoints <- svPoints %>%
      filter(time >= startTime & time < endTime) %>%
      mutate(duration = diff(c(time, endTime)) / binSize) # grab the ratio of duration time to binsize
    # adding the endtime at the end in case theres no final sv at the end time

    # in case there arent any svs during the given bin just use the current sv
    if (nrow(localSvPoints) == 0) {

      average <- currentSv

    } else {

      # %*% multiplies all values in each column with the corresponding value in the
      # other column and then sums everything up
      # basically sum(col1*col2) in a nutshell
      average <- localSvPoints$duration %*% localSvPoints$unnormalizedSv +
      (1 - sum(localSvPoints$duration)) * currentSv

      # used in case there isnt any timing point at the beginning of the bin
      # (refer to line above)
      currentSv <- tail(localSvPoints$unnormalizedSv, 1)

    }

    rollingAvg <- c(rollingAvg, average) # append()

  }

  return(data.frame(time = bins, rollingAvg = rollingAvg))
}

osu.getTitle <- function(osu) {

  title <- paste0(
    util.lookup("Artist", osu$mapData),
    " - ",
    util.lookup("Title", osu$mapData),
    " [",
    util.lookup("Version", osu$mapData),
    "]"
  )

  return(title)
}
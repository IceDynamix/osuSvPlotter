source("lib/util.R")
require("dplyr")

#' Data of the parsed .osu file
#'
#' No validation checking first so you'll have to live without it for now
#'
#' @param path Path to the .osu file
#' @return A list containing the file's map data, timing points and hit objects, all parsed as data.frames
#' @examples
#' osu.parseMap("myFolder/osuMap.osu")
osu.parseMap <- function(path) {

  osuFile <- readLines(path)

  # Instead of iterating over each line, I iterate over each section and then decide how to parse each section

  # Looks for all lines containing a .ini header and puts them in a vector
  # Also adds the final line to the end of the vector
  lineNumbers <- c(grep("^\\[.*\\]", osuFile), length(osuFile))

  # Initialize all return values
  mapData <- data.frame()
  timingPoints <- data.frame()
  hitObjects <- data.frame()

  for (section in 1:(length(lineNumbers) - 1)) {

    title <- osuFile[lineNumbers[section]]
    sectionStartLine <- lineNumbers[section] + 1
    sectionEndLine <- lineNumbers[section + 1] - 1
    sectionContent <- osuFile[sectionStartLine:sectionEndLine]

    # Extraction of the condition for readability
    # Even though this is a osu!mania focused tool, I'm adding [Colours] for good measure
    isKeyValueSection <- title %in% c("[General]", "[Metadata]", "[Editor]", "[Difficulty]", "[Colours]")

    if (isKeyValueSection) {

      # Doesn't seem obvious at first, but a "key: value\n" pair is nothing else but a
      # 2-column table, or not? Imagine replacing the colon with a comma and parsing as a cSV...
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

      # Adding the values of the timing points as 0.01-10 for SVs or as any integer for BPMs
      # for convenience when working with the timing point data
      timingPoints <- mutate(
        timingPoints,
        value = ifelse(
            uninherited == 0,
            -100 / beatLength,
            60000 / beatLength
          )
        )

    } else if (title == "[HitObjects]") {

      # Oh hell no I am NOT dealing with gamemodes other than mania
      # because sliders break EVERYTHING
      mode <- util.lookup("Mode", mapData)
      if (mode == 3) {

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

  }

  return(
    list(
      mapData = mapData,
      timingPoints = timingPoints,
      hitObjects = hitObjects
    )
  )

}


#' Parses a table with preset options for convenience
#' @param sectionContent Text content to parse
#' @param separator Which character splits the text into columns
#' @param headers Vector of column names, must be the same length as the column count
#' @return Data frame of the parsed text
osu.getTable <- function(sectionContent, separator, headers = NULL) {
  # Using my own table wrapper for convenience

  table <- read.table(
    text = sectionContent, # content to parse
    header = FALSE, # our content doesnt have a header
    col.names = headers, # set custom headers
    sep = separator, # separator (":" for map data, "," for timing points and hitobjects)
    strip.white = TRUE, # whitespace should be trimmed
    as.is = TRUE # don't parse to appropriate data type
  )

  return(table)
}

#' Gets the BPM which is present in the song for the longest and therefore considered the base BPM
#' for all scroll speed calculations
#' @param osu Return value of osu.parseMap()
#' @return Base BPM of a song as a number
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

#' Gets all bookmarks from an osu file as a vector
#' @param osu Return value of osu.parseMap()
#' @return Numeric vector of all bookmarks
osu.getBookmarks <- function(osu) {
  return(
    util.parseStringAsNumericVector(
      util.lookup("Bookmarks", osu$mapData)
    )
  )
}

#' Gets all BPM Points from an osu file as a vector
#' @param osu Return value of osu.parseMap()
#' @return Data frame containing the time and the value of each BPM Point
osu.getBpmPoints <- function(osu) {
  bpmPoints <- osu$timingPoints %>%
    filter(uninherited == 1) %>%
    select(time, value)
  return(bpmPoints)
}

#' Gets all SV Points from an osu file as a vector and adds some extra data
#' @param osu Return value of osu.parseMap()
#' @return Data frame containing the time, the value and the current BPM of each SV Point
osu.getSvPoints <- function(osu) {

  bpmPoints <- osu.getBpmPoints(osu)
  svPoints <- osu$timingPoints %>%
    filter(uninherited == 0) %>%
    select(time, value)

  # Each BPM point that doesn't have a starting SV should be considered 1x (before unnormalizing)
  for (time in setdiff(bpmPoints$time, svPoints$time)) {
    svPoints <- rbind(svPoints, data.frame(time = time, value = 1))
  }

  # order SVPoints after adding new ones
  svPoints <- arrange(svPoints, time)

  # Building a vector that's supposed to be a column of SVPoints,
  # containing the current BPM for each SV point
  currentBpmList <- NULL

  # Instead of going through each SV point to check which BPM it has, I think it's more effective
  # to loop through each BPM point to count how many SV points are between the current BPM point
  # and the next one
  for (n in 1:length(bpmPoints$time)) {
    left <- bpmPoints$time[n]
    right <- bpmPoints$time[n + 1] # Imagine the lag() function in this place instead
    currentBPM <- bpmPoints$value[n]

    numberOfAffectedSvPoints <- length(svPoints$time[svPoints$time >= left & svPoints$time < right])
    currentBpmList <- c(currentBpmList, rep(currentBPM, numberOfAffectedSvPoints))
  }

  svPoints$currentBPM <- currentBpmList

  # Uses the ratio of base bpm and current bpm to get the base SV value from the normalized value
  svPoints <- svPoints %>%
    mutate(unnormalizedSv = value / (osu.getBaseBpm(osu) / currentBPM))

  return(svPoints)
}


#' Adds the rolling average for each svPoint
#' @param svPoints Return value of osu.getSvPoints()
#' @returns SV Points data frame with a new rolling average column
osu.getSvRollingAverage <- function(svPoints) {
  start <- min(svPoints$time)
  end <- max(svPoints$time)

  # Increase these sizes to have a more/less sensitive line graph
  # Reason im splitting these two variables is to open up the possibility of
  # a rolling shutter thingy

  # Using 500ms as the base value, because it's around the timeframe of a note scrolling
  # from top to receptor at scroll 26-28 (iirc)
  binSize <- 500 # in ms
  step <- 500 # in ms

  bins <- seq(start, end, by = step)

  rollingAvg <- NULL
  currentSv <- 1

  for (bin in bins) {

    # I know its a useless assignment but helps readability
    startTime <- bin
    endTime <- startTime + binSize

    localSvPoints <- svPoints %>%
      filter(time >= startTime & time < endTime) %>%
      mutate(duration = diff(c(time, endTime)) / binSize) # Ratios of duration time to binsize
    # Adding the endtime at the end in case there's no final SV at the end time

    # In case there arent any SVs during the given bin just use the current SV
    if (nrow(localSvPoints) == 0) {

      average <- currentSv

    } else {

      distanceTraveledWithoutStartingSv <- (1 - sum(localSvPoints$duration)) * currentSv
      # %*% multiplies all values in each column with the corresponding value in the
      # other column and then sums everything up, basically sum(col1*col2) in a nutshell
      average <- localSvPoints$duration %*% localSvPoints$unnormalizedSv +
        distanceTraveledWithoutStartingSv

      # Used in case there isnt any timing point at the beginning of the bin
      # (refer to line above)
      currentSv <- tail(localSvPoints$unnormalizedSv, 1)

    }

    rollingAvg <- c(rollingAvg, average)

  }

  return(data.frame(time = bins, rollingAvg = rollingAvg))
}

#' Standardized map string
#' @param osu Return value of osu.parseMap()
#' @return String in "artist - title [diff] (mapper)" format
osu.getTitle <- function(osu) {

  artist <- util.lookup("Artist", osu$mapData)
  songTitle <- util.lookup("Title", osu$mapData)
  difficultyName <- util.lookup("Version", osu$mapData)
  mapper <- util.lookup("Creator", osu$mapData)

  title <- paste0(
    artist, " - ", songTitle, " [", difficultyName, "] (", mapper, ")"
  )

  return(title)
}
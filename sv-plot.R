source("lib/util.R")
source("lib/osu.R")
source("lib/osuPlotElements.R")

require("ggplot2") # main plotting library
require("dplyr") # data manip library (introduces the pipe %>% and more)
require("ggdark") # dark theme for ggplot

path <- file.choose()
osu <- osu.parseMap(path) # parsing function defined in osu.R

bpmPoints <- osu.getBpmPoints(osu)
svPoints <- osu.getSvPoints(osu)
title <- osu.getTitle(osu)

ggplot(data = svPoints, mapping = aes(time, unnormalizedSv)) +
  geom_point(size = 0.5) +
  # Greenyellow looks the closest to the color used in the osu editor
  geom_step(color = "greenyellow", alpha = 0.25) +
  # Made own functions for custom ggplot elements related to osu
  geom_osuSvRollingAvg(osu) +
  geom_osuBookmarks(osu) +
  geom_osuBpmPoints(osu) +
  geom_osuKiais(osu) +
  # Draws the bounds of the normal SV limits (10^(+-1)) as a comparison
  geom_hline(yintercept = 1, color = "white") +
  geom_hline(yintercept = c(0.1, 10), color = "white", alpha = 0.25) +
  # Using log10 scale because a normal scale wouldn't emphasize the impact of <1x SV points
	scale_y_log10() +
	coord_cartesian(ylim = c(0.1, 10)) +
	dark_theme_gray() +
	ggtitle(paste0(osu.getTitle(osu), " - SV pattern"))

# "cairo-png" adds antialiasing to the image from the cairo package
ggsave("output.png", width = 20, height = 10, type = "cairo-png")

# This line of code installs the pacman page if you do not have it installed - if you do, it simply loads the package
if (!require(pacman))
	install.packages("pacman")

# If bbplot is not installed, run:
# install.packages("devtools")
# devtools::install_github('bbc/bbplot')

pacman::p_load("dplyr", "tidyr", "gapminder", "ggplot2",
	"ggalt", "forcats", "R.utils", "png", "grid",
	"ggpubr", "scales", "bbplot", "stringr")

setwd("~/code/learning-r-stats/")

source("mlb-functions.R")

data <- read.table(file = "data/baseball_prospectus/salaries/MLB-Salaries 2017.xls - MLB-Salaries 2017.xls.csv",
	sep = ",", header = TRUE, quote = "\"")
salaries <- clean_salaries(data)

# Make plot

title_font <- "Sentinel"
subtitle_font <- "Avenir"
mono_font <- "InputSans"

# Point plot of salaries by year in league
plot_point <- function(salaries, title_font, subtitle_font,
	mono_font) {

	line <- ggplot(salaries, aes(x = AnnualSalary,
		y = MLS)) + geom_point(colour = "#1380A1",
		alpha = 0.3, size = 4)
	line <- line + bbc_style()
	line <- style_scales(line)
	line <- set_titles(line, "MLB Salaries 2017",
		"Salary by year in league (outliers omitted)")
	line <- style_lines(line)
	line <- style_fonts(line, title_font, subtitle_font,
		mono_font)

	finalise_plot(plot_name = line, source = "Source: Baseball Prospectus",
		save_filepath = "~/Downloads/mlb_salaries_point.png",
		width_pixels = 720, height_pixels = 480)
}

# Histogram of salaries
style_scales_comma <- function(my_plot) {
	my_plot + scale_x_continuous(labels = comma, limits = c(575000, 22500000)) +
		scale_y_continuous()
}

plot_hist <- function(salaries, title_font, subtitle_font,
	mono_font) {
	line <- ggplot(salaries, aes(AnnualSalary)) +
		geom_histogram(binwidth = 500000, fill = "#1380A1",
			colour = "white")
	line <- line + bbc_style()
	line <- style_scales_comma(line)
	line <- set_titles(line, "MLB Salaries 2017",
		"Salary histogram (minimum salary and high end outliers omitted)")
	line <- style_lines(line)
	line <- style_fonts(line, title_font, subtitle_font,
		mono_font)

	finalise_plot(plot_name = line, source = "Source: Baseball Prospectus",
		save_filepath = "~/Downloads/mlb_salaries_hist.png",
		width_pixels = 720, height_pixels = 480)
}

# Generate charts

plot_point(salaries, title_font, subtitle_font,
	mono_font)
plot_hist(salaries, title_font, subtitle_font,
	mono_font)

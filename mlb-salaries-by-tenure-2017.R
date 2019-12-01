# This line of code installs the pacman page if you do not have it installed - if you do, it simply loads the package
if (!require(pacman)) 
	install.packages("pacman")

# If bbplot is not installed, run:
# install.packages("devtools")
# devtools::install_github('bbc/bbplot')

pacman::p_load("dplyr", "tidyr", "gapminder", "ggplot2", "ggalt", "forcats", "R.utils", "png", "grid", "ggpubr", "scales", "bbplot", "stringr")

setwd("~/code/learning-r-stats/data/baseball_prospectus/")

salaries <- read.table(file = "salaries/MLB-Salaries 2017.xls - MLB-Salaries 2017.xls.csv", sep = ",", header = TRUE, quote = "\"")

# Rename column
names(salaries)[4] <- "AnnualSalary"

# Clean salary dollar values and convert to numeric
n_salary <- as.numeric(gsub("[$,]", "", salaries$AnnualSalary))
salaries$AnnualSalary <- n_salary

# Omit blank salaries
salaries <- salaries[complete.cases(salaries[, 4]), ]

# Make plot

title_font <- "Sentinel"
subtitle_font <- "Avenir"
mono_font <- "InputSans"

style_fonts <- function(my_plot, title_font, subtitle_font, mono_font) {
	my_plot + theme(plot.title = ggplot2::element_text(family = title_font, size = 36, face = "bold", color = "#222222")) + theme(plot.subtitle = ggplot2::element_text(family = subtitle_font,
		size = 22, margin = ggplot2::margin(7, 0, 9, 0))) + theme(legend.text = ggplot2::element_text(family = subtitle_font, size = 18, color = "#222222"), axis.text = ggplot2::element_text(family = mono_font,
		size = 12, color = "#222222"))
}

style_lines <- function(my_plot) {
	my_plot + theme(panel.grid.major.x = element_line(color = "#cbcbcb")) + geom_hline(yintercept = 0, size = 1, colour = "#333333")
}

set_titles <- function(my_plot) {
	my_plot + labs(title = "MLB Salaries 2017", subtitle = str_interp("Salary by year in league (outliers omitted)"))
}

style_scales <- function(my_plot) {
	my_plot + scale_x_continuous(labels = dollar, limits = c(0, 2.5e+07), breaks = c(0, 5e+06, 1e+07, 2e+07)) + scale_y_continuous(limits = c(0, 15))
}

line <- ggplot(salaries, aes(x = AnnualSalary, y = MLS)) + geom_jitter(colour = "#1380A1", alpha = 0.3, size = 4)
line <- line + bbc_style()
line <- style_scales(line)
line <- set_titles(line)
line <- style_lines(line)
line <- style_fonts(line, title_font, subtitle_font, mono_font)

finalise_plot(plot_name = line, source = "Source: Baseball Prospectus", save_filepath = "~/Downloads/mlb_salaries.png", width_pixels = 720, height_pixels = 480)


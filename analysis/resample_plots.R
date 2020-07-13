# script for plotting data from the PRICLE after cessation of treatments

## Load packages and functions
library(lme4)
library(car)
library(emmeans)
library(tidyverse)
library(gtable)
library(grid)

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}

## read in data
diversity = subset(read.csv('../data/diversity_data.csv'))
cover_sum = subset(read.csv('../data/cover_pft_data.csv'))

## create factors
diversity$nitrogenfac = as.factor(diversity$nitrogen)
diversity$precipitationfac = as.factor(diversity$precipitation)
diversity$blockfac = as.factor(diversity$block)
diversity$yearfac = as.factor(diversity$year)
diversity$plotfac = as.factor(diversity$plot)
cover_sum$nitrogenfac = as.factor(cover_sum$nitrogen)
cover_sum$precipitationfac = as.factor(cover_sum$precipitation)
cover_sum$blockfac = as.factor(cover_sum$block)
cover_sum$yearfac = as.factor(cover_sum$year)
cover_sum$plotfac = as.factor(cover_sum$plot)

## create treatment variable
diversity$treatment[diversity$precipitationfac == '0' & diversity$nitrogenfac == '0'] = 'Ambient'
diversity$treatment[diversity$precipitationfac == '1' & diversity$nitrogenfac == '0'] = 'VR'
diversity$treatment[diversity$precipitationfac == '0' & diversity$nitrogenfac == '1'] = 'N'
diversity$treatment[diversity$precipitationfac == '1' & diversity$nitrogenfac == '1'] = 'VR+N'
cover_sum$treatment[cover_sum$precipitationfac == '0' & cover_sum$nitrogenfac == '0'] = 'Ambient'
cover_sum$treatment[cover_sum$precipitationfac == '1' & cover_sum$nitrogenfac == '0'] = 'VR'
cover_sum$treatment[cover_sum$precipitationfac == '0' & cover_sum$nitrogenfac == '1'] = 'N'
cover_sum$treatment[cover_sum$precipitationfac == '1' & cover_sum$nitrogenfac == '1'] = 'VR+N'

## create summarized dataset (for time plotting)
diversity_group_by = group_by(diversity, yearfac, treatment)
diversity_year_mean = summarise(diversity_group_by,
                                n = n(),
                                D_mean = mean(D, na.rm = T),
                                D_se = sd(D, na.rm = T)/sqrt(n),
                                E_mean = mean(E, na.rm = T),
                                E_se = sd(E, na.rm = T)/sqrt(n),
                                R_mean = mean(R, na.rm = T),
                                R_se = sd(R, na.rm = T)/sqrt(n))
diversity_year_mean$year = as.numeric(as.character(diversity_year_mean$yearfac))

cover_sum_group_by = group_by(cover_sum, yearfac, treatment, pft)
cover_sum_year_mean = summarise(cover_sum_group_by,
                                n = n(),
                                Sum_mean = mean(Sum, na.rm = T),
                                Sum_se = sd(Sum, na.rm = T)/sqrt(n))
cover_sum_year_mean$year = as.numeric(as.character(cover_sum_year_mean$yearfac))

## create figures

### diversity figure
d_plot = ggplot(data = subset(diversity, year > 2015), 
                aes(y = D, x = treatment, fill = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(3), colour = 'black'),
        axis.title.x=element_text(size=rel(3), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30)) +
  geom_boxplot(position = 'dodge', colour = 'black', lwd = 1) +
  scale_fill_manual(values = c('white', 'red', 'blue', 'purple')) +
  ylim(c(0, 2)) +
  ylab('') +
  xlab('') +
  annotate('text', x = 1.5, y = c(1.9, 1.75, 1.6), 
           label = c('N (ns)', 'VR (ns)', 'N x VR (ns)'), size = 5) +
  labs(tag = "B")

d_plot_time = ggplot(data = diversity_year_mean, 
                     aes(y = D_mean, x = year, color = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(3), colour = 'black'),
        axis.title.x=element_text(size=rel(3), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30)) +
  geom_line(size = 2, alpha = 0.7) +
  geom_point(size = 6, alpha = 0.7) +
  scale_color_manual(values = c('black', 'red', 'blue', 'purple')) +
  ylim(c(0, 8)) +
  xlim(c(2012, 2020)) +
  ylab('Diversity') +
  xlab('') +
  labs(tag = "A")

e_plot = ggplot(data = subset(diversity, year > 2015), 
                aes(y = E, x = treatment, fill = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(3), colour = 'black'),
        axis.title.x=element_text(size=rel(3), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30)) +
  geom_boxplot(position = 'dodge', colour = 'black', lwd = 1) +
  scale_fill_manual(values = c('white', 'red', 'blue', 'purple')) +
  ylim(c(0, 0.6)) +
  ylab('') +
  xlab('') +
  annotate('text', x = 1.5, y = c(1.9/3.33, 1.75/3.33, 1.6/3.33), 
           label = c('N†', 'VR (ns)', 'N x VR (ns)'), size = 5) +
  labs(tag = "D")

e_plot_time = ggplot(data = diversity_year_mean, 
                     aes(y = E_mean, x = year, color = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(3), colour = 'black'),
        axis.title.x=element_text(size=rel(3), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30)) +
  geom_line(size = 2, alpha = 0.7) +
  geom_point(size = 6, alpha = 0.7) +
  scale_color_manual(values = c('black', 'red', 'blue', 'purple')) +
  ylim(c(0, 0.8)) +
  xlim(c(2012, 2020)) +
  ylab('Evenness') +
  xlab('') +
  labs(tag = "C")

r_plot = ggplot(data = subset(diversity, year > 2015), 
                aes(y = R, x = treatment, fill = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(3), colour = 'black'),
        axis.title.x=element_text(size=rel(3), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30)) +
  geom_boxplot(position = 'dodge', colour = 'black', lwd = 1) +
  scale_fill_manual(values = c('white', 'red', 'blue', 'purple')) +
  ylim(c(0, 15)) +
  ylab('') +
  xlab('Historical Treatment') +
  annotate('text', x = 1.5, y = c(1.9*7.5, 1.75*7.5, 1.6*7.5), 
           label = c('N†', 'VR (ns)', 'N x VR (ns)'), size = 5) +
  labs(tag = "F")

r_plot_time = ggplot(data = diversity_year_mean, 
                     aes(y = R_mean, x = year, color = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(3), colour = 'black'),
        axis.title.x=element_text(size=rel(3), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30)) +
  geom_line(size = 2, alpha = 0.7) +
  geom_point(size = 6, alpha = 0.7) +
  scale_color_manual(values = c('black', 'red', 'blue', 'purple')) +
  ylim(c(0, 15)) +
  xlim(c(2012, 2020)) +
  ylab('Richness') +
  xlab('Year') +
  labs(tag = "E")

jpeg(filename = "plots/diversity_plot_v2.jpeg", 
     width = 15, height = 15, units = 'in', res = 600)
multiplot(d_plot_time, d_plot, e_plot_time, e_plot, r_plot_time, r_plot, cols = 2)
dev.off()

### plant type figure
cf_plot = ggplot(data = subset(cover_sum, pft == 'cf' & year > 2015), 
                 aes(y = Sum, x = treatment, fill = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(2.6), colour = 'black'),
        axis.title.x=element_text(size=rel(2.6), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30)) +
  geom_boxplot(position = 'dodge', colour = 'black', lwd = 1) +
  scale_fill_manual(values = c('white', 'red', 'blue', 'purple')) +
  ylim(c(0, 200)) +
  ylab('') +
  xlab('') +
  annotate('text', x = 1.5, y = c(1.9*10, 1.75*10, 1.6*10), 
           label = c('N (ns)', 'VR (ns)', 'N x VR (ns)'), size = 5) +
  labs(tag = "B")

cf_plot_time = ggplot(data = subset(cover_sum_year_mean, pft == 'cf'), 
                     aes(y = Sum_mean, x = year, color = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(3), colour = 'black'),
        axis.title.x=element_text(size=rel(3), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30)) +
  geom_line(size = 2, alpha = 0.7) +
  geom_point(size = 6, alpha = 0.7) +
  scale_color_manual(values = c('black', 'red', 'blue', 'purple')) +
  ylim(c(0, 200)) +
  xlim(c(2012, 2020)) +
  ylab('CF cover (%)') +
  xlab('') +
  labs(tag = "A")

c4g_plot = ggplot(data = subset(cover_sum, pft == 'c4g' & year > 2015), 
                  aes(y = Sum, x = treatment, fill = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(2.6), colour = 'black'),
        axis.title.x=element_text(size=rel(2.6), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30)) +
  geom_boxplot(position = 'dodge', colour = 'black', lwd = 1) +
  scale_fill_manual(values = c('white', 'red', 'blue', 'purple')) +
  ylim(c(0, 60)) +
  ylab('') +
  xlab('') +
  annotate('text', x = 1.5, y = c(1.9*30, 1.75*30, 1.6*30), 
           label = c('N†', 'VR†', 'N x VR (ns)'), size = 5) +
    labs(tag = "D")

c4g_plot_time = ggplot(data = subset(cover_sum_year_mean, pft == 'c4g'), 
                      aes(y = Sum_mean, x = year, color = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(3), colour = 'black'),
        axis.title.x=element_text(size=rel(3), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30)) +
  geom_line(size = 2, alpha = 0.7) +
  geom_point(size = 6, alpha = 0.7) +
  scale_color_manual(values = c('black', 'red', 'blue', 'purple')) +
  ylim(c(0, 60)) +
  xlim(c(2012, 2020)) +
  ylab('C4G cover (%)') +
  xlab('') +
  labs(tag = "C")

nf_plot = ggplot(data = subset(cover_sum, pft == 'nf' & year > 2015), 
                 aes(y = Sum, x = treatment, fill = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(2.6), colour = 'black'),
        axis.title.x=element_text(size=rel(2.6), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30)) +
  geom_boxplot(position = 'dodge', colour = 'black', lwd = 1) +
  scale_fill_manual(values = c('white', 'red', 'blue', 'purple')) +
  ylim(c(0, 40)) +
  ylab('') +
  xlab('Historical Treatment') +
  annotate('text', x = 1.5, y = c(1.9*20, 1.75*20, 1.6*20), 
           label = c('N†', 'VR (ns)', 'N x VR (ns)'), size = 5) +
    labs(tag = "F")

nf_plot_time = ggplot(data = subset(cover_sum_year_mean, pft == 'nf'), 
                       aes(y = Sum_mean, x = year, color = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(3), colour = 'black'),
        axis.title.x=element_text(size=rel(3), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.tag = element_text(size = 30)) +
  geom_line(size = 2, alpha = 0.7) +
  geom_point(size = 6, alpha = 0.7) +
  scale_color_manual(values = c('black', 'red', 'blue', 'purple')) +
  ylim(c(0, 50)) +
  xlim(c(2012, 2020)) +
  ylab('NF cover (%)') +
  xlab('Year') +
  labs(tag = "E")

jpeg(filename = "plots/pft_plot_v2.jpeg", 
     width = 15, height = 15, units = 'in', res = 600)
multiplot(cf_plot_time, cf_plot, c4g_plot_time, c4g_plot, nf_plot_time, nf_plot, cols = 2)
dev.off()


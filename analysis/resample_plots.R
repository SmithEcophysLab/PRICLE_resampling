# script for plotting data from the PRICLE after cessation of treatments

## Load packages
library(lme4)
library(car)
library(emmeans)
library(tidyverse)
library(gtable)
library(grid)

## read in data
diversity = subset(read.csv('../data/diversity_data.csv'), year > 2015)
cover_sum = subset(read.csv('../data/cover_pft_data.csv'), year > 2015)

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

## create figures

### diversity figure
d_plot = ggplot(data = diversity, aes(y = D, x = treatment, fill = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(2), colour = 'black'),
        axis.title.x=element_text(size=rel(2), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "white"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  geom_boxplot(position = 'dodge', colour = 'black') +
  scale_fill_manual(values = c('white', 'red', 'blue', 'purple')) +
  ylim(c(0, 2)) +
  ylab('Diversity') +
  xlab('') +
  annotate('text', x = 1, y = c(2, 1.8, 1.6), 
           label = c('N (ns)', 'VR (ns)', 'N x VR (ns)'))

e_plot = ggplot(data = diversity, aes(y = E, x = treatment, fill = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(2), colour = 'black'),
        axis.title.x=element_text(size=rel(2), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "white"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  geom_boxplot(position = 'dodge', colour = 'black') +
  scale_fill_manual(values = c('white', 'red', 'blue', 'purple')) +
  ylim(c(0, 0.5)) +
  ylab('Evenness') +
  xlab('') +
  annotate('text', x = 1, y = c(2/4, 1.8/4, 1.6/4), 
           label = c('N†', 'VR (ns)', 'N x VR (ns)'))

r_plot = ggplot(data = diversity, aes(y = R, x = treatment, fill = treatment)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(2), colour = 'black'),
        axis.title.x=element_text(size=rel(2), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "white"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  geom_boxplot(position = 'dodge', colour = 'black') +
  scale_fill_manual(values = c('white', 'red', 'blue', 'purple')) +
  ylim(c(0, 16)) +
  ylab('Richness') +
  xlab('') +
  annotate('text', x = 1, y = c(2*8, 1.8*8, 1.6*8), 
           label = c('N†', 'VR (ns)', 'N x VR (ns)'))

d_plot_g = ggplotGrob(d_plot)
e_plot_g = ggplotGrob(e_plot)
r_plot_g = ggplotGrob(r_plot)

diversity_plots = rbind(d_plot_g, e_plot_g, r_plot_g, size = 'last')
diversity_plots$widths = unit.pmax(d_plot_g$widths, e_plot_g$widths, r_plot_g$widths)
grid.newpage()
grid.draw(diversity_plots)

### plant type figure

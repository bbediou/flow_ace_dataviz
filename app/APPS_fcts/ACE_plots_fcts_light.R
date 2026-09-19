# ==============================================================================
# ACE_plots_fcts_light.R  —  DEPLOY-LIGHT copy of ACE_plots_fcts.R
#
# Contains ONLY the plotting functions used by Harmony_ACE_dataviz.R, and loads
# ONLY the packages those functions need. This avoids pulling heavy, unused
# dependencies (corrplot, corrr, igraph, qgraph, bootnet, PerformanceAnalytics,
# psych) into the shinyapps.io build.
#
# Functions are copied verbatim from APPS_fcts/ACE_plots_fcts.R. If you change a
# function there, mirror the change here (or vice-versa).
# ==============================================================================

library(ggplot2)
library(plotly)
library(dplyr)
# ggsci:: and ggpubr:: are called with :: below (no library() needed).

plot_ntrials.subj <- function(dat){
  p <- dat %>%
    group_by(Sid) %>%
    summarise(n_trials = n()) %>%
    ggplot(aes(y=Sid, x=n_trials, text=Sid))+
    geom_point()+
    theme_classic()
  plotly::ggplotly(p)
}

plot_ntrials.distrib <- function(dat){
  tmp <- dat %>%
    group_by(Sid) %>%
    summarise(n_trials = n())

  p <-  ggplot(tmp,aes(x=n_trials))+
    geom_histogram()+
    theme_classic()
  ggplotly(p)
}

plot_rt.distrib <- function(dat){
  p <- dat %>%
    ggplot(aes(x = Response.Time, fill = factor(Correct.Button)))+
    geom_histogram(binwidth = 50, position = "stack")+
    labs(x="RT (correct trials)")+
    scale_fill_manual(values=c("#db1d04","#0595ab"),
                       name="", labels = c("Error","Correct"))+
    theme_classic()
  ggplotly(p)
}

plot_rt.trials <- function(dat){
  dat %>%
    ggplot(aes(x=Trial.Number, y = Response.Time,
               col = factor(Correct.Button), text=Sid)) +
    geom_point(size = 3, alpha = .6,
               position = position_jitter(width = .1, seed = 123))+
    geom_line(aes(group = Sid), col = "gray", alpha = .3,
              position = position_jitter(width = .1, seed = 123))+
    scale_color_manual(values=c("#db1d04","#0595ab"),
                              name="", labels = c("Error","Correct"))+
    labs(title = "RT over time (trials)")+
    theme_classic()
}

plot_rw.trials <- function(dat){
  p <- dat %>% mutate(Response.Window = as.numeric(Response.Window)) %>%
    ggplot(aes(x=Trial.Number, y = Response.Window,
               col = factor(Correct.Button), text=Sid)) +
    geom_point(size = 3, alpha = .6,
               position = position_jitter(width = .1, seed = 123))+
    geom_line(aes(group = Sid), col = "gray", alpha = .3,
              position = position_jitter(width = .1, seed = 123))+
    scale_color_manual(values=c("#db1d04","#0595ab"),
                       name="", labels = c("Error","Correct"))+
    labs(title = "Response Window over time (trials)")+
    theme_classic()
  ggplotly(p)
}

plot_task.perf.var <- function(dat_agg_cond, var="rt_corr_mean"){
  dat_agg_cond$var <- dat_agg_cond[[var]]
  p <- dat_agg_cond %>%
    ggplot(aes(x = Condition, y = var,
               text = paste0("Sid: ",Sid,"\n",
                             "var:", var))) +
    geom_violin(aes(group = Condition), alpha = .001)+
    geom_boxplot(aes(group = Condition),width = .2)+
    geom_point(size = 3, alpha = .6,
               position = position_jitter(width = .05, seed = 123))+
    geom_line(aes(group = Sid),
              linewidth = .5, alpha = .3,
              position = position_jitter(width = .05, seed = 123))+
    labs(x="",y="", title = var)+
    ggsci::scale_color_aaas()+
    theme_classic()
  ggplotly(p)
}

plot_task.corrwithage.var <- function(data_merged, var = "rt_corr_mean"){
  tmp <- data_merged
  tmp$yvar <- tmp[[var]]
  cor.pos <- tmp %>%
    group_by(Condition) %>%
    summarise(ypos = max(yvar, na.rm = TRUE))

  p <- tmp %>%
    ggplot(aes(x = Age, y = yvar, col = Condition,
               group = Condition,
               text = Sid))+
    geom_point(aes(group = Sid), size = 3, alpha = .6,
               position = position_jitterdodge(jitter.width = .1, dodge.width = .6))+
    geom_smooth(method = "glm", se = FALSE)+
    ggpubr::stat_cor(aes(group = Condition,
                         label = paste(..r.label.., ..p.label.., sep = ",")),
                     method = "spearman",
                     p.accuracy = 0.001,
                     r.accuracy = 0.01,
                     digits = 2, na.rm = TRUE,
                     label.x= 14,
                     label.y = cor.pos$ypos,
                     output.type="text")+
    ggsci::scale_color_aaas()+
    labs(y=var)+
    theme_classic()
  ggplotly(p, tooltip = "text")
}

# extract csv files into a list of dataframes
ace_data.extract <- function(data_path, list_of_tasks){
  combined_df <- tibble()
  csv_files <- list.files(path = data_path, pattern = "*.csv", full.names = TRUE, recursive = FALSE)
  nested_df <- tibble(filename = character(), data = list())
    for (csv_file in csv_files) {
      # filename <- basename(csv_file)
      data <- read.csv(csv_file)
      # data <- load_ace_file(csv_file, app_type = "explorer")
      
      nested_df <- nested_df %>% add_row(filename = csv_file, data = list(data))
    }
    combined_df <- combined_df %>% bind_rows(nested_df)
    
    # Create a list to store the resulting dataframes
    filtered_dfs <- list()
    
    # Extract and assign each element of the filtered_dfs list as a separate dataframe
    for (string in list_of_tasks) {
      filtered_df <- combined_df %>%
        filter(grepl(string, filename)) %>%
        pull(data) %>%
        map(~ mutate_all(.x, as.character)) %>%
        bind_rows()
      
      filtered_dfs[[string]] <- filtered_df
    }
    return(filtered_dfs)
}

# Function to extract game name and play count
# input_string = demographics$Games.Play.Count[8]
## Games played
# Pattern for extracting game names
game_name_pattern = "'gameName': '\\w+'"
# Pattern for extracting play counts
play_count_pattern = "'playCount': \\d+"
# Pattern for Game scores
game_score_pattern = "'score': -?\\d+\\.\\d+"

extract_info <- function(input_string) {
  game_names <- regmatches(input_string, gregexpr(game_name_pattern, input_string))[[1]]
  game_names <- gsub("'gameName': '", "", game_names)
  game_names <- gsub("'", "", game_names)  
  
  play_counts <- regmatches(input_string, gregexpr(play_count_pattern, input_string))[[1]]
  play_counts <- gsub("'playCount': ", "", play_counts) %>%
    as.numeric()
  
  # game_scores <- regmatches(input_string, gregexpr(game_score_pattern, input_string))[[1]]
  # game_scores <- gsub("'score': ", "", game_scores) %>%
  #   as.numeric()
  
  data.frame(GameName = unlist(game_names), 
             PlayCount = unlist(play_counts), 
             # GameScore = unlist(game_scores),
             stringsAsFactors = FALSE)
}

# extract game scores
# input_string = demographics$Games.Scores[13]
extract_score <- function(input_string) {
  game_names <- regmatches(input_string, gregexpr(game_name_pattern, input_string))[[1]]
  game_names <- gsub("'gameName': '", "", game_names)
  game_names <- gsub("'", "", game_names)  
  
  game_scores <- regmatches(input_string, gregexpr(game_score_pattern, input_string))[[1]]
  game_scores <- gsub("'score': ", "", game_scores) %>%
    as.numeric()
  
  data.frame(GameName = unlist(game_names),
             GameScore = unlist(game_scores),
             stringsAsFactors = FALSE)
}

# process task
task.process <- function(dat, numcols = c("Trial.Number","Response.Time", "Correct.Button","Response.Window")){
  dat_name <- deparse(substitute(dat))
  dat <- dat %>%
    mutate(Sid = Participant.Id,
           # Sid = sub(x = Sid, pattern = "BY_", replacement =""),
           Sid = sub(x = Sid, pattern = "micah-t1_", replacement ="")) %>%
    mutate_at(numcols, as.numeric) %>%
    mutate(rt_corr = ifelse(Correct.Button==1, Response.Time, NA),
           rt_err = ifelse(Correct.Button==0, Response.Time, NA),
           response.type = ifelse(Correct.Button==1, "correct","error")) %>%
    mutate(response.type = ifelse(Response.Time < fastRT, "impulsive response",
                                  ifelse(Response.Time > slowRT, "slow response",
                                         response.type))
           ) 
  return(dat)
}
# stroop_raw <- task.process(STROOP)
# table(stroop_raw$data.clean, stroop_raw$Condition)

agg.subj.cond <- function(dat){
  dat %>%
    group_by(Participant.Id, Sid, Condition, .groups = "drop") %>%
    summarise(n_trials = n(),
              n_impulse = sum(Response.Time < 100, na.rm = TRUE),
              n_correct = sum(Correct.Button, na.rm = TRUE),
              n_error = sum(Correct.Button==0),
              rt_corr_mean = mean(rt_corr, na.rm = TRUE),
              rt_corr_median = median(rt_corr, na.rm = TRUE),
              rt_corr_sd = sd(rt_corr, na.rm = TRUE),
              rt_err_mean = mean(rt_err, na.rm = TRUE),
              rt_err_sd = sd(rt_err, na.rm = TRUE),
              pcorr = n_correct/n_trials,
              rcs = n_correct/(rt_corr_mean * n_trials)*1000,
              rcs_median = n_correct/(rt_corr_median * n_trials)*1000,
              rcs_ln = n_correct/(log(rt_corr_mean) * n_trials)*1000,
              rcs_ben = pcorr*100/rt_corr_mean,
              resp_window_mean = mean(Response.Window),
              resp_window_sd = sd(Response.Window)
              ) 
}

agg.subj <- function(dat){
  dat %>% 
    group_by(Participant.Id,Sid,.groups = "drop") %>%
    summarise(n_trials = n(),
              n_hits = sum(Correct.Button, na.rm = TRUE),
              rt_corr_mean = mean(rt_corr, na.rm = TRUE),
              rt_corr_median = median(rt_corr, na.rm = TRUE),
              rt_corr_sd = sd(rt_corr, na.rm = TRUE),
              rt_corr_var = sd(rt_corr, na.rm = TRUE)^2,
              pcorr = n_hits/n_trials,
              rcs = n_hits/(rt_corr_mean * n_trials)*1000,
              rcs_median = n_hits/(rt_corr_median * n_trials)*1000,
              rcs_ln = n_hits/(log(rt_corr_mean) * n_trials)*1000,
              rcs_ben = pcorr*100/rt_corr_mean,
              resp_window_mean = mean(Response.Window),
              resp_window_sd = sd(Response.Window))
}


# Edge correction for accuracy = 0, 0.5 or 1
edge.corr <- function(dat=brt_agg) {
  dat <- dat %>% 
    mutate(pcorr_edge = pcorr) %>%
    mutate(pcorr_edge = ifelse(pcorr==0, 1/(2*n_trials),
                               ifelse(pcorr==1, 1 - 1/(2*n_trials), 
                                      ifelse(pcorr == 0.5, 0-5 - 0.5/(2*n_trials), 
                                             pcorr))))
  dat
}

# Estimation of EZ-DM parameters
get.vaTer = function(Pc, VRT, MRT, s=0.1){
  s2 = s^2
  # The default value for the scaling parameter s equals 0.1
  L = qlogis(Pc)
  # The function “qlogis” calculates the logit.
  x = L*(L*Pc^2 -L*Pc + Pc - .5)/VRT
  v = sign(Pc-.5)*s*x^(1/4)
  # This gives drift rate.
  a = s2*qlogis(Pc)/v
  # This gives boundary separation.
  y = -v*a/s2
  MDT = (a/(2*v)) * (1-exp(y))/(1+exp(y))
  Ter = MRT - MDT
  # This gives nondecision time.
  return(list(v, a, Ter))
}

ddm.extract <- function(dat_agg){
  # extract DDM parameters
  Pc = dat_agg$pcorr_edge
  VRT = dat_agg$rt_corr_var
  MRT = dat_agg$rt_corr_mean
  pars <- get.vaTer(Pc, VRT, MRT, s=0.1) 
  
  dat_ddm <- as.data.frame(do.call(cbind,pars)) %>%
    dplyr::rename("v" = "V1", "a" = "V2", "ter" = "V3") %>%
    cbind(dat_agg,.)
}


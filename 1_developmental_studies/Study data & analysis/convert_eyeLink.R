## Extracts the synchronization time message that is sent to the eye tracking at the beginning of the study
## in order to sync the eye tracking data with input & video data
get_sync_time <- function(file_name, dir_name)
{
  ### Messages data include the following columns: "RECORDING_SESSION_LABEL",
  # "TRIAL_INDEX", "CURRENT_MSG_TEXT", "CURRENT_MSG_TIME", "TRIAL_START_TIME"
  d <-
    read_tsv(file.path(dir_name, file_name, "Output", "messages.xls"), col_types = "ccccc")
  d <- d %>% filter(CURRENT_MSG_TEXT == "SYNCTIME")
  return(as.numeric(d$CURRENT_MSG_TIME) + as.numeric(d$TRIAL_START_TIME))
}

## Uses the synchronization time to unify timestamps across input, video & eye tracking data
## Then merges all data to get input, video, gaze & fixation data in one unique data file
merge_inputs_ET <- function()
{
  list_files <-
    list.files(file.path("..", "Eye tracking script", "Data"),
               pattern = "*.csv$",
               full.names = TRUE)
  for (file in list_files)
  {
    file_name <- strsplit(basename(file), ".", fixed = TRUE)[[1]][1]
    dir_name <- dirname(file)
    sync_time <- get_sync_time(file_name, dir_name)
    display_data <-
      read_csv(file.path(file)) %>% mutate(Timestamp = round(Timestamp)) %>% select(Timestamp, Source, Data)
    sync_time_display <-
      display_data$Timestamp[which(grepl("SYNCTIME", display_data$Data, fixed =
                                           TRUE))]
    sync_time_shift <- sync_time - sync_time_display
    
    ### Gaze data include the following columns: "RECORDING_SESSION_LABEL",
    # "TRIAL_INDEX", "RIGHT_GAZE_X", "RIGHT_GAZE_Y", "RIGHT_FIX_INDEX",
    # "TIMESTAMP", "TRIAL_START_TIME"
    gaze_data <-
      read_tsv(file.path(dir_name, file_name, "Output", "gaze.xls"), col_types = "ccccccc")
    ### Fixation data include the following columns: "RECORDING_SESSION_LABEL",
    # "TRIAL_INDEX", "CURRENT_FIX_X", "CURRENT_FIX_Y", "CURRENT_FIX_START"
    # "CURRENT_FIX_END", "CURRENT_FIX_DURATION", "CURRENT_FIX_INDEX"
    fix_data <-
      read_tsv(file.path(dir_name, file_name, "Output", "fixations.xls"),
               col_types = "cccccccc")
    
    trial_start_time <-
      as.numeric(sub(",", ".", gaze_data$TRIAL_START_TIME[1], fixed = TRUE))
    merged_data <-
      gaze_data %>% left_join(
        fix_data %>% select(
          CURRENT_FIX_X,
          CURRENT_FIX_Y,
          CURRENT_FIX_START,
          CURRENT_FIX_END,
          CURRENT_FIX_INDEX
        ),
        by = join_by(RIGHT_FIX_INDEX == CURRENT_FIX_INDEX)
      )
    
    eyes_data <-
      tibble(
        Timestamp = as.numeric(sub(
          ",", ".", merged_data$TIMESTAMP, fixed = TRUE
        )) - sync_time_shift,
        Gaze.X = as.numeric(sub(
          ",", ".", merged_data$RIGHT_GAZE_X, fixed = TRUE
        )),
        Gaze.Y = as.numeric(sub(
          ",", ".", merged_data$RIGHT_GAZE_Y, fixed = TRUE
        )),
        Fixation.X = as.numeric(sub(
          ",", ".", merged_data$CURRENT_FIX_X, fixed = TRUE
        )),
        Fixation.Y = as.numeric(sub(
          ",", ".", merged_data$CURRENT_FIX_Y, fixed = TRUE
        )),
        Fixation.Start = as.numeric(sub(
          ",", ".", merged_data$CURRENT_FIX_START, fixed = TRUE
        )) + trial_start_time - sync_time_shift,
        Fixation.End = as.numeric(sub(
          ",", ".", merged_data$CURRENT_FIX_END, fixed = TRUE
        )) + trial_start_time - sync_time_shift,
        Fixation.Index = as.numeric(sub(
          ",", ".", merged_data$RIGHT_FIX_INDEX, fixed = TRUE
        ))
      )
    d <-
      display_data %>% full_join(eyes_data, by = "Timestamp") %>% arrange(Timestamp)
    start_time <- (d %>% filter(Data == "SYNCTIME"))$Timestamp
    end_time <-
      (d %>% filter(Data == "Key: 's' (Released)"))$Timestamp
    d <- d %>% filter(Timestamp <= end_time, Timestamp >= start_time)
    if (!dir.exists(file.path("raw_data", "ET_raw"))) {dir.create(file.path("raw_data", "ET_raw"), recursive = TRUE)}
    write_csv(d, file = file.path("raw_data", "ET_raw", basename(file)), na = "")
  }
}
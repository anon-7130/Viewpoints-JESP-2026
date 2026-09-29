pick_fixation_time <- function()
{
  if (!dir.exists(file.path("..", "Fixation check", "order_files"))) {dir.create(file.path("..", "Fixation check", "order_files"), recursive = TRUE)}
  if (!dir.exists(file.path("..", "Fixation check", "ET_check"))) {dir.create(file.path("..", "Fixation check", "ET_check"), recursive = TRUE)}
  ET_files         <- list.files(file.path("intermediate_data", "ET"), pattern="*.csv", full.names = TRUE)
  d_eye_tracking   <- lapply(ET_files, read_csv, show_col_types = FALSE)
  idx_name <- 1
  for(d in d_eye_tracking)
  {
    file_name <- basename(ET_files[[idx_name]])
    response_file <- list.files(file.path("raw_data", "responses"), pattern=file_name, full.names = TRUE)
    d_fixations <- d %>% filter(!is.na(Corrected.Fixation.Y)) %>%
      group_by(Corrected.Fixation.X, Corrected.Fixation.Y) %>%
      summarise(Duration = last(Timestamp) - first(Timestamp), Timestamp = median(Timestamp), .groups = "keep") %>%
      ungroup() %>%
      filter(Timestamp>((first(Timestamp) + last(Timestamp))/2)) %>%
      filter(Duration>150) %>%
      slice_sample(n = 3) %>%
      arrange(Timestamp)
    idx_name <- idx_name + 1
    d_fixations$img <- ifelse(d_fixations$Corrected.Fixation.Y <28800, "top", "bottom")
    d_fixations$Corrected.Fixation.Y.img <- ifelse(d_fixations$Corrected.Fixation.Y <28800, d_fixations$Corrected.Fixation.Y, d_fixations$Corrected.Fixation.Y-28800)
    N_frames_video <- c()
    for(time in d_fixations$Timestamp)
    {
      N_frames_video <- c(N_frames_video, (d %>% filter(grepl("Frame", Data, fixed=TRUE), Timestamp<time) %>% slice_tail(n=1))$Data)
    }
    d_fixations$N_frame_video <- N_frames_video
    write.csv(d_fixations, file = file.path("..", "Fixation check", "ET_check", file_name), row.names = FALSE)
    file.copy(response_file, file.path("..", "Fixation check", "order_files", basename(response_file)))
  }
}

extract_dwell_times <- function(dataset, size, start_coordinate, shift_coordinate, x_left, x_right, y_size)
{
  dwell_time <- c()
  
  pb <- txtProgressBar(min = -1, max = size-1, style = 3, width = 50, char = "=")
  
  for (j in (seq_len(size) - 1))
  {
    d_match <- dataset %>%
      mutate(Match = ifelse(Corrected.Gaze.X >= x_left & Corrected.Gaze.X <= x_right & Corrected.Gaze.Y >= floor(start_coordinate + shift_coordinate * j) & Corrected.Gaze.Y <= floor(start_coordinate + shift_coordinate * j) + y_size & !is.na(Corrected.Gaze.Y) & !is.na(Corrected.Gaze.X), TRUE, FALSE))
    match_first <- (d_match %>%
      filter(Match == TRUE) %>% first())[["Timestamp"]]
    match_last <- (d_match %>%
      filter(Match == TRUE) %>% last())[["Timestamp"]]
    d_match <- d_match %>% filter(Timestamp <= match_last,
                                  Timestamp >= match_first)
      acc <- 0
      ts <- NA
      
      for(i in seq_len(nrow(d_match)))
      {
        if(d_match[[i, "Match"]] == TRUE)
        {
          if(!is.na(ts))
          {
            acc <- acc + (d_match[[i, "Timestamp"]] - ts)
          }
          else
          {
            ## Some hits will be missed because samples are not continuous.
            ## The refresh rate is 1000Hz (one image every 1ms), so on
            ## average, we can consider that any period of viewing started 
            ## 0.5ms earlier, and ended 0.5ms later. This allows us to
            ## not have a 0ms time for a single quick gaze (saccade) in the area
            acc <- acc + 1
          }
          ts <- d_match[[i, "Timestamp"]]
        }
        else
        {
          ts <- NA
        }
      }
      dwell_time <- c(dwell_time, acc)
    setTxtProgressBar(pb, j)
  }
  return(dwell_time)
}

datasets_merge <- function()
{
  if (!dir.exists(file.path("final_data"))) {dir.create(file.path("final_data"))}
  start_coordinate <- 274
  shift_coordinate <- 380
  x_left <- 316
  x_right <- 1602
  y_shift <- 295
  
  ET_files         <- list.files(file.path("intermediate_data", "ET"), pattern="*.csv", full.names = TRUE)
  d_eye_tracking   <- lapply(ET_files, read_csv, show_col_types = FALSE)
  ET_files_names <- basename(ET_files)
  responses_files  <- list.files(file.path("raw_data", "responses"), pattern="*.csv", full.names = TRUE)
  d_responses      <- lapply(responses_files, read_csv, show_col_types = FALSE)
  
  for (i in seq_len(length(d_eye_tracking)))
  {
    cat(paste(" File:", ET_files_names[[i]]))
    d_responses[[i]]$Dwell.Time <- extract_dwell_times(d_eye_tracking[[i]], max(d_responses[[i]]$Order), start_coordinate, shift_coordinate, x_left, x_right, y_shift)
    write.csv(d_responses[[i]], file = file.path("final_data", ET_files_names[i]), row.names = FALSE)
  }
  cat("\nDone!")
}

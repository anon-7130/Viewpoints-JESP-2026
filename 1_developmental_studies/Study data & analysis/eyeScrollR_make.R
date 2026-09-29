eyeScrollR_make <- function()
{
  if (!dir.exists(file.path("intermediate_data", "ET"))) {dir.create(file.path("intermediate_data", "ET"), recursive = TRUE)}
  ET_files <- list.files(path=file.path("raw_data", "ET_raw"), pattern="*.csv", full.names = TRUE, recursive = TRUE)
  ET_files_names <- basename(ET_files)
  datasets <- lapply(ET_files, read_csv, show_col_types = FALSE)
  
  img_height <- 38579
  img_width <- 1920
  
  source("eyeScrollR_make_first_frame.R")
  
  calib_img <- readPNG("calibration_image.png")
  calibration <- scroll_calibration_auto(calib_img, 100)
  
  for (i in seq_len(length(datasets)))
  {
    start <- (datasets[[i]] %>% filter(Data == paste0("Frame ", frame_start[[ET_files_names[[i]]]])))$Timestamp
    d_ET <- eye_scroll_correct(eyes_data = datasets[[i]],
                               timestamp_start = start,
                               timestamp_stop = max(datasets[[i]]$Timestamp),
                               image_width = img_width,
                               image_height = img_height,
                               calibration = calibration,
                               scroll_lag = get_scroll_lag(refresh_rate = 60, n_frame = 2))
    
    d_ET_end <- d_ET %>% filter(grepl("MouseEvent:Button.left.Pressed", Data, fixed=TRUE)) %>% mutate(mouse_X = as.numeric(str_extract_all(Data, "\\d+", simplify=TRUE)[,1]), mouse_Y = as.numeric(str_extract_all(Data, "\\d+", simplify=TRUE)[,2]))
    d_ET_end <- d_ET_end %>% rowwise() %>% mutate(mouse_Y = mouse_Y + (Corrected.Fixation.Y - Fixation.Y))
    
    # Get the last click on coordinates on the webpage corresponding to the "Finished reading" button
    ET_end <- (d_ET_end %>% filter(mouse_Y>=38387, mouse_Y<=38474, mouse_X>=816, mouse_X<=1103) %>% slice_tail(n=1))$Timestamp
    
    if(length(ET_end)==0)
    {
      warning(paste0("No click on 'Finished reading' button detected for participant ", ET_files_names[[i]]))
    }
    else
    {
      d_ET <- d_ET %>% filter(Timestamp<ET_end) %>%
        select(Timestamp = Timestamp.Shifted, Source, Data, Gaze.X, Gaze.Y, Corrected.Gaze.X, Corrected.Gaze.Y, Corrected.Fixation.X, Corrected.Fixation.Y, Scroll)
    }
    write_csv(d_ET, file = file.path("intermediate_data", "ET", ET_files_names[i]), na = "")
  }
}

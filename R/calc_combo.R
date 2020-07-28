# Function to calculate Yankee score with input of year and ct_mfi data
calc_combo <- function(dt){
  
  # Function to add scores for arc, gf, lto, enemp, and homeval.
  arc <- function(A, arc_min, arc_slp, arc_wt)
    min(max(0, (A - arc_min) / arc_slp), arc_wt)
  gf <- function(B, gf_min, gf_slp, gf_wt)
    min(max(0, (B - gf_min) / gf_slp), gf_wt)
  lto <- function(C, ltoblig_min, ltoblig_slp, ltoblig_wt)
    min(max(0, (C - ltoblig_min) / ltoblig_slp), ltoblig_wt)
  unemp <- function(D, unemp_min, unemp_slp, unemp_wt)
    min(max(0, (D - unemp_min) / unemp_slp), unemp_wt)
  homeval <- function(E, homeval_min, homeval_slp, homeval_wt)
    min(max(0, (E - homeval_min) / homeval_slp), homeval_wt)
  
  # unify names
  setnames(dt, "Municipality", "municipality")
  
  # load Yankee score calculation frame from xlsx, make and clean names
  frame <- readxl::read_xlsx("/Volumes/davidlucey/aDL/data/ct_muni_data/yankee-frame.xlsx")
  names(frame)[1] <- "municipality"
  names(frame) <- str_extract(names(frame), "\\w{1,}")
  frame <- frame[!str_detect(names(frame), "\\d")][, c(1:21)]
  names(frame)[2:6] <- paste0(names(frame)[2:6], "_wt")
  names(frame)[7:11] <- paste0(names(frame)[7:11], "_min")
  names(frame)[12:16] <- paste0(names(frame)[12:16], "_max")
  names(frame)[17:21] <- paste(names(frame)[17:21], "_slp")
  frame <- janitor::clean_names(frame)
  frame <- setDT(frame)
  
  frame[,`:=`(arc_min=.20,arc_max=0.15)][
  ][,arc_slp:=(arc_max-arc_min)/arc_wt]
  
  # combine yankee score calc frame with
  combo <- dt[frame, on = "municipality"]
  combo[, arc_score := mapply(arc, A, arc_min, arc_slp, arc_wt)]
  combo[, gf_score := mapply(gf, B, gf_min, gf_slp, gf_wt)]
  combo[, lto_score := mapply(lto, C, ltoblig_min, ltoblig_slp, ltoblig_wt)]
  combo[, unemp_score := mapply(unemp, D, unemp_min, unemp_slp, unemp_wt)]
  combo[, homeval_score := mapply(homeval, E, homeval_min, homeval_slp, homeval_wt)]
  combo[, score := mapply(sum, arc_score, gf_score, lto_score, unemp_score, homeval_score)]
  
  return(combo)
}
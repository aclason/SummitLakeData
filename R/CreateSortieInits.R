# 2021-12-16
# By: Leah Walker & Alana Clason

# Create sortie initial conditions from 92/94 data

#' Title
#'
#' @param dbh_size_class
#' @param minDBH
#' @param maxDBH
#' @param plot_area
#' @param raw_data
#' @param output_dir
#'
#' @return
#' @export
#'
#' @examples
SORTIEinits <- function(dbh_size_class = 2,minDBH = 0,maxDBH,
                        plot_area = 0.05,
                        raw_data = "./data-raw/Trees/SummitLakeData.csv",
                        output_dir) {

  sl_dat <- SummitLakeData::clean_tree_data(raw_data = raw_data)

  range(sl_dat$DBH, na.rm = TRUE)
  # Create a vector of DBH size classes, by 2 cm increments
  diam_classes <- seq(minDBH,(maxDBH + dbh_size_class),
                     by = dbh_size_class)

  # Replace old tree tag numbers with new, if applicable
  #raw_data$TreeID <- as.numeric(raw_data$TreeID)
  #raw_data$TreeID_new <- as.numeric(raw_data$TreeID_new)
  #raw_data <- raw_data %>%
  #  dplyr::mutate(TreeID = ifelse(is.na(TreeID_new), TreeID, TreeID_new))
  #raw_data$TreeID_new <- NULL


  # Create column for and fill with DBH bins
  for(j in 1:length(diamClasses)){
    raw_data[DBH <= diamClasses[j] & DBH > diamClasses[j]-dbhClSize, DBH_bin := diamClasses[j]]
  }



  # Create data table of the diameter classes
  diamDT <- data.table()
  diamDT[,DBH_bin := diamClasses]



  # Create a label for each diameter class of each species of each plot
  labels.summit.spD <- as.data.table(merge(labels.summit.sp, diamDT, fill=TRUE))



  # Merge labels with data, possibly an unnecessary step
  dat.summit.m <- merge(labels.summit.spD, raw_data, all = TRUE)
  dat.summit.m <- subset(dat.summit.m, DBH != "NA")



  # Count number of trees per DBH bin, per species, per unit
  dat.summit.m.s <- dat.summit.m %>%
    dplyr::group_by(unit, Spp, DBH_bin) %>%
    dplyr::mutate(count = n())
  dat.summit.m.s <- as.data.table(dat.summit.m.s)
  # Calculate stems per hectare
  dat.summit.m.s[, SPH := count/ PlotArea]



  # Remove unnecessary columns
  #dat.summit.m.s$TreeID <- NULL
  dat.summit.m.s$DBH <- NULL
  dat.summit.m.s$count <- NULL



  # Merge labels with data set including SPH
  dat.summit.SPH <- merge(labels.summit.spD, dat.summit.m.s, all = T)
  cols <- "SPH"
  # Now that the counts are done, fill in empty DBH bins with zero
  dat.summit.SPH[,(cols) := lapply(.SD,nafill, fill = 0), .SDcols = cols]
  # Eliminate duplicates
  dat.summit.SPH <- unique(dat.summit.SPH)



  # Create new column that names the DBH bins as they need to be for SORTIE
  for(i in 1:nrow(dat.summit.SPH)){
    dat.summit.SPH[i, variable := paste0("Init.Dens_",dat.summit.SPH[i,DBH_bin], ".0"),]
  }




  # Create a data table with all the species headings, but one row of NAs
  DT <- data.table(1)[, `:=`(c("variable", "Sx", "Pl", "Bl",
                               "At", "Lw", "Fd", "Ac", "Ep"), NA)][,V1 := NULL]

  TS92 <- data.table("variable" = "Timesteps",
                     "Sx" = 27)



  # Translate the data from long to short and create a csv for each unit (plot)
  for(ii in unique(dat.summit.SPH$unit)){
    dat.unit <- dat.summit.SPH[unit == ii]
    dat.unit$unit <- NULL
    dat.unit$DBH_bin <- NULL
    dat.unit <- dcast(dat.unit, variable ~ Spp, value.var = "SPH")
    dat.unit <- as.data.table(dat.unit)
    dat.unit[, c("Pl", "At", "Ac") := 0]
    setcolorder(dat.unit, c("variable", "Sx", "Pl", "Bl", "At", "Lw", "Fd", "Ac", "Ep"))
    dat.unit <- rbindlist(list(DT, dat.unit[1:nrow(dat.unit)]), use.names = TRUE, fill = TRUE)
    dat.unit <- rbindlist(list(dat.unit, TS92), use.names = TRUE, fill = TRUE)
    setnames(dat.unit, c("variable", "Sx", "Pl", "Bl", "At", "Lw", "Fd", "Ac", "Ep"),
             c(" ", "Interior_Spruce", "Lodgepole_Pine", "Subalpine_Fir", "Trembling_Aspen",
               "Western_Larch", "Douglas_Fir", "Black_Cottonwood", "Paper_Birch"))

    if(dir.exists(inits_dir)){
      write.csv(dat.unit, paste0(inits_dir,"summit_",ii,".csv"), row.names = FALSE)
    }else{
      dir.create(inits_dir)
      write.csv(dat.unit, paste0(inits_dir,"summit_",ii,".csv"), row.names = FALSE)
    }


  }

}

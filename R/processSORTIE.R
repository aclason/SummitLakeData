#' import and combine sortie outputs
#'
#'
#' @export
#'
#' @param out_path
#' @param run_name
#' @param Units_path
#' @param yrs #might take this away - get it from the data
#' @param Units_to_output a vector of character names for which units to include for subplotting outputs
#' @param dist_edge [numeric()] how far from unit boundary to allow subplots (in m)
#' @param num_subplots [numeric()] how many subplots
#' @param size_subplot [numeric()] radius of plot (standard is 7.98m)
#' @param plotting TRUE/FALSE - whether or not to display plots with the unit and subplot location
#'
#' @details
#' to do: make this more generic where you pass the sortie output (not Date Creek specific) and
#' subplot based on different patterns
#'
#'
rdSortieOutputs <- function(out_path, run_name, yrs=NULL){
  #raw output files - if csvs exist already - ignore
  outputs <- grep(".csv",list.files(out_path, pattern = run_name),
                  invert=TRUE, value = TRUE)
  rootnames <- str_split(outputs, "_det_", simplify = TRUE)
  posPlots <- unique(rootnames[,1])

  #how many years
  posYrs <- range(as.numeric(rootnames[,2]))
  if(!is.null(yrs)){
    extrYrs <- yrs
  }else{
    extrYrs <- seq(posYrs[1],posYrs[2])
  }

  #check that all posPlots have all posYears in the sequence
  print(paste0("plot(s) ",posPlots[!paste0(posPlots,"_det_",posYrs[2]) %in% outputs],
               " do not contain year ", posYrs[2],". Check SORTIE runs."))

  for(j in 1:length(posPlots)){
    dt_table <- data.table()

    for(i in 1:length(extrYrs)){

      file_to_read <- paste0(out_path,"/",posPlots[j],"_det_",extrYrs[i])

      if(file.exists(file_to_read)){
        dt <- fread(file_to_read,sep="\t", header=T,na.strings = "--", skip=1)
        dt[, ':='(timestep = extrYrs[i],Plot = posPlots[j])]
                #clip out 1 ha from centre of simulation
        #dt <- dt[X >50 & X <150 & Y >50 & Y <150]
        dt_table <- rbind(dt_table,dt)

      }else{
        print(paste(file_to_read,"does not exist"))
        dt_table <- dt_table
      }

    }
    data.table::fwrite(dt_table,paste0(out_path,"/",posPlots[j],".csv"))

  }

}


SLtreat <- function(){

}







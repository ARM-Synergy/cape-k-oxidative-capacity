#' Function to command HYSPLIT (Hybrid Single Particle Lagrangian Integrated 
#' Trajectory Model) from R. 
#' 
#' \code{run_hysplit} will return a data frame which is ready for use in 
#' \strong{openair}'s \code{traj*} functions. 
#' 
#' @param latitude Latitude of a receptor location. 
#' 
#' @param longitude Longitude of a receptor location. 
#' 
#' @param start What is the start date of data to be returned? Ideally, the 
#' date format should be \code{yyyy-mm-dd}, but the UK locale convention of 
#' \code{dd/mm/yyyy} will also work. Years as strings or integers work too and
#' will floor-rounded. 
#' 
#' @param end What is the end date of data to be returned? Ideally, the 
#' date format should be \code{yyyy-mm-dd}, but the UK locale convention of 
#' \code{dd/mm/yyyy} will also work. Years as strings or integers work too and 
#' will be ceiling-rounded. 
#' 
#' @param interval What interval should the trajectories be run at? Default is 
#' \code{"3 hour"}, as in \strong{openair}.
#' 
#' @param runtime How many hours from time zero should the trajectory be run? 
#' Default is \code{-96}, i.e., a back trajectory of 96 hours, as in 
#' \strong{openair}. 
#' 
#' @param start_height Height of receptor location. Default is \code{10} metres, 
#' as in \strong{openair}. 
#' 
#' @param model_height Height of model domain. Default is \code{10000} metres,
#' as in \strong{openair}. 
#' 
#' @param hysplit_exec Location of HYSPLIT's executable files. Ensure executable
#' permisions have been set for the \code{hyts_std} application if on a Unix 
#' system. HYSPLIT's \code{bdyfiles} directory with the \code{ASCDATA.CFG} will
#' also need to be in the correct location. 
#' 
#' @param hysplit_input Location of input meteorological files for HYSPLIT. 
#' 
#' @param hysplit_output Location of where HYSPLIT should write its trajectory 
#' outputs while working. 
#' 
#' @param delete Should temporary files be silently deleted before and after the
#' model runs? Default is \code{FALSE}. 
#' 
#' @param drop Should the \code{"year"}, \code{"month"}, \code{"day"}, 
#' \code{"hour"}, and \code{"receptor"} variables be dropped from the bound
#' object? Default is \code{TRUE}. 
#' 
#' @param site An optional site string to be added to the returned data frame. 
#' 
#' @param source An optional source string to be added to the returned data frame. 
#' 
#' @param verbose Should the function give messages on what trajectory is being
#' processed. Default is \code{FALSE} and will estimate the time until 
#' completion.
#' 
#' @seealso \code{\link{importTraj}}, \code{\link{trajPlot}}, 
#' \url{http://ready.arl.noaa.gov/HYSPLIT.php}
#' 
#' @author Stuart K. Grange and David Carslaw
#' 
#' @import lubridate
#' @import stringr
#' 
#' @examples 
#' \dontrun{
#' 
#'   
#' # Run back trajectories for first 10 days at 1500 metres and extended run time for
#' # analysis of air aloft or Gibraltar
#' data_gibraltar <- run_hysplit(
#'   latitude = 36.134, 
#'   longitude = -5.347, 
#'   runtime = -96, 
#'   start_height = 1500, 
#'   model_height = 10000, 
#'   start = 2015,
#'   end = "2015-01-10",
#'   hysplit_exec = "~/Hysplit/exec", 
#'   hysplit_input = "~/TrajData", 
#'   hysplit_output = "~/temp",
#'   site = "gibraltar")
#'   
#' # On a Windows system with a default hysplit installation
#' data_gibraltar <- run_hysplit(
#'   latitude = 36.134, 
#'   longitude = -5.347, 
#'   runtime = -96, 
#'   start_height = 10, 
#'   model_height = 10000, 
#'   start = 2015,
#'   end = "2015-01-10",
#'   hysplit_exec = "~/hysplit4/exec", 
#'   hysplit_input = "~/trajData", 
#'   hysplit_output = "~/temp",
#'   site = "gibraltar")
#'   
#' }
#' 
#' @export
run_hysplit <- function (latitude = 51.5, 
                         longitude = -0.1, 
                         start = NA, 
                         end = NA, 
                         interval = "3 hour", 
                         runtime = -96, 
                         start_height = 10, 
                         model_height = 10000,
                         hysplit_exec = "exec/", 
                         hysplit_input, 
                         hysplit_output = "hysplit_output/",
                         delete = TRUE,
                         drop = TRUE,
                         site = NA, 
                         source = NA, 
                         verbose = FALSE) {
  
  require(stringr)
  require(lubridate)
  require(dplyr)
  require(purrr)
  require(readr)
  
  # Parse arguments
  # Expand paths
  hysplit_exec <- path.expand(hysplit_exec)
  hysplit_input <- path.expand(hysplit_input)
  hysplit_output <- path.expand(hysplit_output)
  
  # check these places exist
  if (file.access(hysplit_exec, mode = 0) != 0) 
    stop(paste("File path", hysplit_exec, "does not exist"))
  
  if (file.access(hysplit_input, mode = 0) != 0) 
    stop(paste("File path", hysplit_input, "does not exist"))
  
  if (file.access(hysplit_output, mode = 0) != 0) 
    stop(paste("File path", hysplit_output, "does not exist"))
  
  if (file.access(hysplit_exec, mode = 2) != 0) 
    stop(paste("File path", hysplit_exec, "is not writable"))
  
  # write default config file that returns met data
  set_config() %>% 
    write_config_list(dir = hysplit_exec)
  
  # Add final path separator
  hysplit_exec <- str_c(hysplit_exec, .Platform$file.sep)
  hysplit_input <- str_c(hysplit_input, .Platform$file.sep)
  hysplit_output <- str_c(hysplit_output, .Platform$file.sep)
  
  # Selection after testing if directory is empty
  files_old <- list.files(hysplit_output, full.names = TRUE, 
                          pattern = "hysplit_output.txt")
  
  # Delete old files
  if (delete) {
    
    # Delete
    message("Deleting old files...")
    file.remove(files_old)
    
  } else {
    
    # Selection control
    while (length(files_old) > 1) {
      
      # Get input
      input <- readline("There are hysplit output files in the output directory. \nShould these be deleted (y/n)? \n")
      
      # Parse
      input <- str_to_upper(input)
      input <- ifelse(
        input %in% c("YES", "Y", "T", "TRUE", "TR", "TRU"), TRUE, FALSE)
      
      if (input) {
        
        message("Deleting old files...")
        file.remove(files_old)
        break
        
      } else {
        
        break
        
      }
      
    }
    
  }
  
  # Dates
  # Start and end dates
  start <- parse_date_arguments(start, "start")
  end <- parse_date_arguments(end, "end")
  
  # Receptor location and starting height
  coordinates <- str_c(latitude, longitude, start_height, sep = " ")
  
  # Set up where the control file is to be written
  control_file <- file.path(hysplit_exec, "CONTROL")
  
  # Store working directory because this will be changed
  wd <- getwd()
  
  # Create date sequence
  date_sequence <- seq(start, end, interval)
  
  # Apply function which runs the model multiple times
  message(str_c("Running ", length(date_sequence), " HYSPLIT trajectories..."))
  
  # For plyr's progress bar
  #progress <- ifelse(verbose, "none", "time")
  
  # Run model
  purrr::walk(date_sequence, run_trajectory, 
              hysplit_exec = hysplit_exec, 
              hysplit_input = hysplit_input, 
              hysplit_output = hysplit_output, 
              control_file = control_file, 
              coordinates = coordinates, 
              runtime = runtime, 
              model_height = model_height)
  
  # Change working directory back to original after system calls
  setwd(wd)
  
  # Bind output files
  message("Binding HYSPLIT files...")
  
  # Get file list
  file_list <- list.files(hysplit_output, "hysplit_output.txt", full.name = TRUE)
  
  # Load files as in openair, but drop some things usually
  df <- purrr::map_df(file_list, read_hysplit_file, drop = drop)
  
  # Add variables which are not in openair's files
  df$start_height <- start_height
  if (!is.na(site)) df$site <- site
  if (!is.na(source)) df$source <- source
  
  # Delete files
  if (delete) file.remove(file_list)
  
  # Return
  df
  
}


# Define the function which creates a control file and calls the hy_std 
# application. 
# 
# No export
run_trajectory <- function (date, 
                            hysplit_exec, 
                            hysplit_input, 
                            hysplit_output, 
                            control_file, 
                            coordinates, 
                            runtime, 
                            model_height) {
  
  # Get pieces of the date
  date_year <- year(date)
  date_month <- month(date)
  date_day <- day(date)
  date_hour <- hour(date)
  
  # Pad zeros
  date_month <- str_pad(date_month, width = 2, pad = "0")
  date_day <- str_pad(date_day, width = 2, pad = "0")
  date_hour <- str_pad(date_hour, width = 2, pad = "0")
  
  # Format for control file
  date_control <- str_c(date_year, date_month, date_day, date_hour, sep = " ")
  
  # Use date to create a file name
  file_name_export <- str_c(
    str_replace_all(date_control, " ", ""), "_hysplit_output.txt")
  
  # Get date string
  current_month_pattern <- str_c(date_year, date_month)
  past_month_pattern <- get_year_and_month(current_month_pattern, - 1)
  future_month_pattern <- get_year_and_month(current_month_pattern, 1)
  
  # Add other pieces of the file names
  past_month_pattern <- str_c("RP", past_month_pattern , ".gbl")
  current_month_pattern <- str_c("RP", current_month_pattern , ".gbl")
  future_month_pattern <- str_c("RP", future_month_pattern , ".gbl")
  
  # Create the file and directory list for the control file
  file_list <- c(past_month_pattern, current_month_pattern, future_month_pattern)
  
  # For back trajectories, do not use the future month
  if (runtime < 0) {
    
    # But only if not the final day of the month, otherwise a stop error occurs
    if (!floor_date(date, "day") == ceiling_date(date, "month") - days(1)) 
      file_list <- file_list[1:2]
    
  }
  
  # Add directory to file names
  file_dir_list <- str_c(hysplit_input, file_list, sep = "\n")
  
  # Write control file
  # This will replace the contents of the current file if it exists
  write_to_control_file(date_control, control_file, append = FALSE)
  
  # Starting locations
  write_to_control_file("1", control_file)
  
  # Write coordinates and starting height of model
  write_to_control_file(coordinates, control_file)
  
  # Write runtime of model, hours forward or backwards for trajectories
  write_to_control_file(runtime, control_file)
  
  # Vertical motion option, top of model, and input grids (number of files)
  write_to_control_file(str_c("0\n", model_height, "\n", length(file_list)),
                        control_file)
  
  # Write input directory and file names
  write_to_control_file(file_dir_list, control_file)
  
  # Output directory
  write_to_control_file(hysplit_output, control_file)
  
  # Output file
  write_to_control_file(file_name_export, control_file)
  
  # Change working directory to hysplit application
  setwd(hysplit_exec)
  
  # Message file name
  # if (verbose) message(file_name_export)
  
  # System call, quiet arguments are os specific
  if (.Platform$OS.type == "windows") {
    
    # Run
    system("./hyts_std", show.output.on.console = TRUE)
    
    
  } else {
    
    # Run, ensure executable permissions are set
    system("./hyts_std", ignore.stdout = TRUE)
    
  }
  
}


# Three files are needed for the model to create a back trajectory,
# the current month, previous month, and future month
get_year_and_month <-  function (pattern, difference = 1) {
  
  # Do some date things
  date <- ymd(str_c(pattern, "01")) + months(difference)
  year <- year(date)
  month <- month(date)
  month <- str_pad(month, 2, pad = "0")
  
  # Combine
  pattern <- str_c(year, month)
  
  # Return
  pattern 
  
}


# Write table function
write_to_control_file <- function(string, file, append = TRUE) {
  
  # Write to file
  write.table(string, file, col.names = FALSE, row.names = FALSE, quote = FALSE, 
              append = append)
  
}


# Function to read hysplit files
# 
# Taken from openair manual to keep bound trajectory files in the same format
# as the openair package. 
# 
# No export
read_hysplit_file <- function (file, drop) {
  
  # Load file, error catching is for when two or three input met files are used
  # and results in a different length file header
  
  tryCatch({
    
    if (grep("PRESSURE", read_lines(file)[6]) == 1) {
      
      df <- read.table(file, header = FALSE, skip = 6) 
      
    }
    
    if (grep("PRESSURE", read_lines(file)[7]) == 1) {
      
      df <- read.table(file, header = FALSE, skip = 7) 
      
    } 
    
  }, error = function (e) {
    
    
    return()
    
  }
  )
  
  if (is.null(nrow(df))) return()
  
  # Drop
  # df <- subset(df, select = -c(V2, V7, V8))
  df <- df %>% 
    select(!any_of(c("V2", "V7", "V8")))
  
  # Rename
  df <- dplyr::rename(df, c(receptor = V1, year = V3, month = V4, 
                            day = V5, hour = V6, hour.inc = V9, 
                            lat = V10, lon = V11,
                            height = V12, pressure = V13,
                            theta = V14, air_temp = V15,
                            rainfall = V16, mixdepth = V17,
                            rh = V18, sp_humidity = V19,
                            h2o_mixrate = V20, terr_msl = V21,
                            sun_flux = V22))
  
  # Clean two digit years
  df$year <- ifelse(df$year < 50, df$year + 2000, df$year + 1900)
  
  # Transform pieces of date to date
  df$date2 <- with(df, ISOdatetime(year, month, day, 
                                   hour, min = 0, sec = 0, 
                                   tz = "GMT"))
  
  # Drop variables
  if (drop) df <- subset(df, select = -c(year, 
                                         month, 
                                         day, 
                                         hour, 
                                         receptor))
  
  # Transform arrival time, minus hours from hour.inc variable
  df$date <- df$date2 - 3600 * df$hour.inc
  
  # Return
  df
  
}

# No export
parse_date_arguments <- function (date, what) {
  if (what == "start") {
    if (!is.na(date) & nchar(date) == 4) 
      date <- stringr::str_c(date, "-01-01")
    date <- ifelse(is.na(date), as.character(lubridate::floor_date(Sys.Date(), 
                                                                   "year")), date)
  }
  if (what == "end") {
    if (!is.na(date) & nchar(date) == 4) 
      date <- stringr::str_c(date, "-12-31 18:00")
    date <- ifelse(is.na(date), as.character(lubridate::ceiling_date(Sys.Date(), 
                                                                     "year")), date)
  }
  date <- lubridate::parse_date_time(date, c("ymd", "dmy", "dmy_HM", "ymd_HM"))
  date
}

# Hysplit config settings from https://github.com/rich-iannone/splitr, modified to return met variables by default

#' Create a configuration list for a trajectory or dispersion model
#'
#' @param tratio The advection stability ratio. By default, this is set to
#'   `0.75`.
#' @param initd The initial distribution. Set to `0` by default.
#' @param kpuff The horizontal puff dispersion growth. Setting to `0` results in
#'   linear growth whereas option `1` uses an empirical growth scheme. By
#'   default, this is set to `0` (linear growth).
#' @param khmax The maximum duration (in hours) for a particle or trajectory.
#'   This is set to the absolute maximum by default, which is `9999` hours.
#' @param kmixd The methodology for modeling the mixed layer depth. There are
#'   three options: `0` for using the input (the default), `1` for using
#'   temperature, and `2` for using the TKE.
#' @param kmix0 The minimum mixing depth in meters. This is `250` by default.
#' @param kzmix How to perform vertical mixing adjustments. There are three
#'   options: `0` for not doing this at all (the default), `1` uses the PBL
#'   average, and `3` scales with *TVMIX*.
#' @param kdef The modeling method for horizontal turbulence. We can use the
#'   vertical with option `0` (the default), or, use the deformation method with
#'   option `1`.
#' @param kbls How to derive the boundary layer stability. Two options are
#'   available for this. We can use fluxes (option `1`, default) or use wind
#'   temperature (option `2`).
#' @param kblt The boundary layer turbulence parameterization to use. This can
#'   either be Beljaars (option `1`), Kanthar (option `2`, default), or TKE
#'   (option `3`).
#' @param conage Particle to- or from-puff conversion at *CONAGE*. In units of
#'   hours, with a default of `48`.
#' @param numpar The number of puffs or particles released per cycle. By default
#'   this is set to `2500`.
#' @param qcycle An optional cycling of emissions, in units of hours.
#' @param efile An absolute path to an optional temporal emissions file.
#' @param tkerd The unstable turbulent kinetic energy ratio. This is set to
#'   `0.18`.
#' @param tkern The stable turbulent kinetic energy ratio. This is set to
#'   `0.18`.
#' @param ninit How to do particle initialization; with `0` no particle
#'   initialization is done, with `1` this is done once, options `2` and `3` are
#'   the *add* and *replace* methods.
#' @param ndump Should the particles be dumped to a file and, if so, how often?
#'   Using `0` results in no writing particles to file (the default), and any
#'   non-zero value performs this writing once per number of hours specified.
#' @param ncycl The *PARDUMP* output cycle time.
#' @param pinpf The particle input file name (default is `"PARINIT"`). This is
#'   useful for initialization or boundary conditions.
#' @param poutf The particle output  file name (default is `"PARDUMP"`).
#' @param mgmin The minimum meteorological subgrid size. The default value is
#'   `10`.
#' @param kmsl The starting height reference. If it is to signify a distance
#'   above ground level (AGL) then use option `0` (the default). If it is
#'   instead relative to mean sea level, then option `1` should be used.
#' @param maxpar The maximum number of particles to be carried in simulation. By
#'   default this is `10000`.
#' @param cpack The binary concentration packing. Here are the options: `0` for
#'   none, `1` for nonzero, `2` for points, and `3` for polar. Option `1` is the
#'   default option.
#' @param cmass Informs grid computation. The two options are to compute grid
#'   concentrations (with `0`, the default) or to compute grid mass (option
#'   `1`).
#' @param dxf,dyf The horizontal x- and y-grid adjustment factors for an
#'   ensemble. By default, these are both set to `1.0`.
#' @param dzf The vertical factor for an ensemble. This is `0.01` by default.
#' @param ichem The chemistry conversion module to employ. Option `0` does no
#'   chemistry (the default). With option `1` we use a matrix method, option `2`
#'   does conversion, and option `3` works on dust.
#' @param maxdim The maximum number of pollutants to carry on one particle. By
#'   default, this is `1`.
#' @param kspl The standard splitting interval in units of hours. By default,
#'   this is `1` hour.
#' @param krnd The enhanced merge interval in hours. By default this is `6`
#'   hours.
#' @param frhs The standard horizontal puff rounding fraction for the merge
#'   process. This is `1.0` by default.
#' @param frvs The vertical puff rounding fraction, which is `0.01` by default.
#' @param frts The temporal puff rounding fraction, which is `0.10` by default.
#' @param frhmax The maximum value for the horizontal rounding parameter. This
#'   is `3.0` by default.
#' @param splitf The automatic size adjustment factor for horizontal splitting.
#'   By default, the value is `1.0`.
#' @param tm_pres,tm_tpot,tm_tamb,tm_rain,tm_mixd,tm_relh,tm_sphu,tm_mixr,tm_dswf,tm_terr
#'   Options to include meteorology along trajectory points. These are the
#'   pressure variable marker flag (`tm_pres`), the potential temperature
#'   (`tm_tpot`), the ambient temperature (`tm_tamb`), the rainfall rate
#'   (`tm_rain`), the mixed layer depth (`tm_mixd`), the relative humidity
#'   (`tm_relh`), the specific humidity (`tm_sphu`), the mixing rate
#'   (`tm_mixr`), the downward short-wave flux (`tm_dswf`), and the terrain
#'   height (`tm_terr`). Setting any of these to `0` disables output, whereas
#'   `1` will enable output of these data points. By default, all are set to
#'   `0`.
#'
#' @export
set_config <- function(tratio = 0.75,
                       initd = 0,
                       kpuff = 0,
                       khmax = 9999,
                       kmixd = 0,
                       kmix0 = 25,
                       kzmix = 0,
                       kdef = 0,
                       kbls = 1,
                       kblt = 2,
                       conage = 48,
                       numpar = 2500,
                       qcycle = 0.0,
                       efile = NULL,
                       tkerd = 0.18,
                       tkern = 0.18,
                       ninit = 1,
                       ndump = 1,
                       ncycl = 1,
                       pinpf = "PARINIT",
                       poutf = "PARDUMP",
                       mgmin = 10,
                       kmsl = 0,
                       maxpar = 10000,
                       cpack = 1,
                       cmass = 0,
                       dxf = 1.0,
                       dyf = 1.0,
                       dzf = 0.01,
                       ichem = 0,
                       maxdim = 1,
                       kspl = 1,
                       krnd = 6,
                       frhs = 1.0,
                       frvs = 0.01,
                       frts = 0.10,
                       frhmax = 3.0,
                       splitf = 1.0,
                       tm_pres = 1,
                       tm_tpot = 1,
                       tm_tamb = 1,
                       tm_rain = 1,
                       tm_mixd = 1, 
                       tm_relh = 1,
                       tm_sphu = 1,
                       tm_mixr = 1,
                       tm_dswf = 1,
                       tm_terr = 1) {
  
  arg_names <- formals(set_config) %>% names()
  arg_vals <- mget(arg_names)
  
  if (is.null(arg_vals$efile)) {
    arg_vals$efile <- "''"
  } else if (!is.null(arg_vals$efile)) {
    arg_vals$efile <- paste0("'", arg_vals$efile, "'")
  }
  
  if (is.null(arg_vals$pinpf)) {
    arg_vals$pinpf <- "''"
  } else if (!is.null(arg_vals$pinpf)) {
    arg_vals$pinpf <- paste0("'", arg_vals$pinpf, "'")
  }
  
  if (is.null(arg_vals$poutf)) {
    arg_vals$poutf <- "''"
  } else if (!is.null(arg_vals$poutf)) {
    arg_vals$poutf <- paste0("'", arg_vals$poutf, "'")
  }
  
  arg_vals[!vapply(arg_vals, FUN = is.null, FUN.VALUE = logical(1))]
}

write_config_list <- function(config_list, dir) {
  
  paste0(
    "&SETUP\n",
    paste0(names(config_list), " = ", config_list, ",\n", collapse = ""),
    "/\n"
  ) %>%
    cat(file = file.path(dir, "SETUP.CFG"))
}


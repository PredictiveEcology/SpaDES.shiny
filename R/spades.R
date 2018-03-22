#' Shiny module to initialize a \code{SpaDES} simulation
#'
#' Simply a wrapper around \code{\link[SpaDES.core]{simInit}} to be called as a shiny module.
#'
#' @note This is a server-only module with no UI component.
#'
#' @param input    Shiny server input object.
#' @param output   Shiny server output object.
#' @param session  Shiny server session object.
#' @param ...      Additional arguments passed to \code{simInit}.
#'
#' @return A reactive \code{simList} object
#' @seealso \code{\link[SpaDES.core]{simInit}}
#'
#' @author Alex Chubaty
#' @export
#' @importFrom SpaDES.core simInit
#'
spades_simInit <- function(input, output, session, ...) {
  reactive({
    do.call(simInit, list(...))
  })
}

#' Shiny module to run a \code{SpaDES} simulation experiment
#'
#' Simply a wrapper around \code{\link[SpaDES.core]{experiment}} to be called as a shiny module.
#'
#' @note This is a server-only module with no UI component.
#'
#' @param input    Shiny server input object.
#' @param output   Shiny server output object.
#' @param session  Shiny server session object.
#' @param sim      A reactive \code{simList} object (e.g., pass \code{sim = sim()}).
#' @param reps     A reactive indicating the number of replicates (e.g., pass \code{reps = reps()}).
#' @param seed     An integer to pass to \code{set.seed} for the simulation experiment.
#' @param objectsToHash  A list of objects to hash (with \code{Cache}).
#' @param cacheDebug  Cache debugging (default \code{"complete"}). See \code{\link{Cache}}.
#' @param ...      Additional arguments passed to \code{experiment}.
#'
#' @return A list of \code{simList} objects.
#' @seealso \code{\link[SpaDES.core]{experiment}}
#'
#' @author Alex Chubaty
#' @export
#' @importFrom raster endCluster
#' @importFrom reproducible Cache Copy
#' @importFrom SpaDES.core experiment
#'
spades_expt <- function(input, output, session, sim, reps, seed, objectsToHash,
                        cacheDebug = "complete") {

  runExperiment <- function(sim, nReps, seed, objectsToHash = "") {
    # # Do an initial run for each given study area so that all the data prep can be done once only
    #initialRun1 <- spades(Copy(sim), debug = TRUE)
    # 5 minutes for 6e3 km2
    # 30 minutes for 6e4 km2
    simCopy <- Copy(sim)
    end(simCopy) <- 0
    message("Running Initial spades call")
    initialRun <- Cache(spades, sim = simCopy, #notOlderThan = Sys.time(),
                        debug = "paste(Sys.time(), paste(unname(current(sim)), collapse = ' '))",
                        objects = "shpStudyRegion",
                        #cacheRepo = cachePath(sim),
                        .plotInitialTime = NA,
                        omitArgs = c("debug", ".plotInitialTime"))

    raster::endCluster()
    set.seed(seed)
    message("Current experiment seed is: ", seed)
    args <- list(experiment, sim, replicates = nReps,
                 objects = objectsToHash,
                 debug = "paste(unname(current(sim)), collapse = ' ')",
                 .plotInitialTime = NA,
                 clearSimEnv = TRUE,
                 omitArgs = c("debug", ".plotInitialTime"))
    args <- args[!unlist(lapply(args, is.null))]
    simOut <- do.call(Cache, args)
    message(attr(simOut, "tags"))
    simOut
  }

  reactive({
    message("  Starting Experiment...")

    set.seed(seed)
    message("    current seed is: ", seed)

    # THIS IS THE MAIN "SIMULATION FUNCTION"
    # THE FOLLOWING OBJECT IS A LIST OF 1 simList,
    # A simList is a rich data structure that comes with the SpaDES.core package
    mySimOut <- Cache(runExperiment, sim(), nReps = reps(), seed = seed,
                      debugCache = cacheDebug,
                      objectsToHash = objectsToHash(), objects = objectsToHash())#, sideEffect = TRUE)

    message("  Finished Experiment.")

    mySimOut
  })
}

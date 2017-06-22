#' Parsing of raw data
#'
#' Data related to the PNAS paper. Accessed on Nov 14, 2015.
#'
#' Parse and load CDC's ILI data, Google Flu Trend data,
#' Google Correlate data trained with ILI as of 2010, Google Correlate data trained with ILI as of 2009,
#' Google Trend data with search terms identified from Google Correlate (2010 version).
#'
#' Each week ends on the Saturday indicated in the xts object
#'
#' Google Correlate data is standardized by Google, and we rescale it to 0 -- 100 during parsing.
#' Google Trends data is in the scale of 0 -- 100.
#'
#' @importFrom zoo index
#'
#' @examples
#' system.file("extdata", "correlate-Influenza_like_Illness_h1n1_CDC_.csv", package = "argo")
#' system.file("extdata", "correlate-Influenza_like_Illness_CDC_.csv", package = "argo")
#' system.file("extdata", "GFT.csv", package = "argo")
#' system.file("extdata", "ILINet.csv", package = "argo")
#' load_data()
#'
#' @return A list of following named xts objects
#' \itemize{
#'  \item \code{GC10} Google Correlate trained with ILI available as of 2010.
#'    Available online at \url{https://www.google.com/trends/correlate/search?e=id:20xKcnNqHrk&t=weekly}
#'  \item \code{GC09} Google Correlate trained with ILI available as of 2009.
#'    Not directly available online, you have to manually input ILI time series
#'    at \url{https://www.google.com/trends/correlate}
#'  \item \code{GT} Google Trends data for search queries identified using Google Correlate.
#'    Not directly available online, you have to manually input query terms
#'    at \url{https://www.google.com/trends}
#'  \item \code{CDC} CDC's ILI dataset.
#'    Available online at \url{http://gis.cdc.gov/grasp/fluview/fluportaldashboard.html}
#'  \item \code{GFT} Google Flu Trend (historical predictions).
#'    Available online at \url{https://www.google.org/flutrends}
#' }
#'
#' @references
#' Yang, S., Santillana, M., & Kou, S. C. (2015). Accurate estimation of influenza epidemics using Google search data via ARGO. Proceedings of the National Academy of Sciences, \href{https://dx.doi.org/10.1073/pnas.1515373112}{doi: 10.1073/pnas.1515373112}.
#'
#' @export
load_data <- function() {
  resacle_gc <- function(GC_data) {
    GC_data$Date <- as.Date(as.character(GC_data$Date))
    GC_data$Date <- GC_data$Date + 6
    GFT_rescale <- xts::xts(GC_data[, -(1:2)], order.by = as.Date(GC_data$Date))
    for (i in 1:ncol(GFT_rescale)) {
      GFT_rescale[, i] <- GFT_rescale[, i] - min(GFT_rescale[, i])
      GFT_rescale[, i] <- GFT_rescale[, i] * 100/max(GFT_rescale[, i])
    }
    GFT_rescale
  }
  GC09_data <- read.csv(system.file("extdata", "correlate-Influenza_like_Illness_h1n1_CDC_.csv",
                                    package = "argo"), skip = 11)
  GC10_data <- read.csv(system.file("extdata", "correlate-Influenza_like_Illness_CDC_.csv",
                                    package = "argo"), skip = 11)

  GC09_data <- resacle_gc(GC09_data)
  GC10_data <- resacle_gc(GC10_data)

  GT_data <- read.csv(system.file("extdata", "GTdata.csv", package = "argo"))
  GT_data <- xts::xts(GT_data[, -1], order.by = as.Date(GT_data$Week))

  ili <- read.csv(system.file("extdata", "ILINet.csv", package = "argo"),
                  skip = 1)
  ili <- xts::as.xts(ili[, c("X..WEIGHTED.ILI", "X.UNWEIGHTED.ILI")],
                     order.by = as.Date("1997-10-04") + (1:nrow(ili) - 1) * 7)
  cdc <- ili[, "X..WEIGHTED.ILI"]
  cdc[cdc=="X"] <- NA
  cdc <- xts::xts(as.numeric(cdc), order.by = index(cdc))
  names(cdc) <- "CDC.data"

  updated_gft <- read.csv(system.file("extdata", "GFT.csv", package = "argo"),
                          skip = 11)
  updated_gft <- xts::xts(as.numeric(updated_gft$United.States)/1000,
                          order.by = as.Date(updated_gft$Date) + 6)
  names(updated_gft) <- "GFT"
  updated_gft <- na.omit(updated_gft)

  data_all <- list()
  data_all$GC09 <- GC09_data
  data_all$GC10 <- GC10_data
  data_all$GT <- GT_data
  data_all$CDC <- cdc
  data_all$GFT <- updated_gft
  data_all
}

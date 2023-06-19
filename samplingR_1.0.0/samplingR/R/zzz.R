#'@importFrom utils packageVersion

.onAttach <- function(lib, pkg){

  packageStartupMessage(c(paste0("                           _ _            ______
                          | (_)           | ___ \\
 ___  __ _ _ __ ___  _ __ | |_ _ __   __ _| |_/ /
/ __|/ _` | '_ ` _ \\| '_ \\| | | '_ \\ / _` |    /
\\__ \\ (_| | | | | | | |_) | | | | | | (_| | |\\ \\
|___/\\__,_|_| |_| |_| .__/|_|_|_| |_|\\__, \\_| \\_|
                    | |               __/ |
                    |_|              |___/         version ", packageVersion("samplingR")),"\n Type 'citation(\"samplingR\")' for citing this R package in publications.\n"))
  invisible()
}

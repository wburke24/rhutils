#' Parse RHESSys command output
#'
#' @param cmdout The command output string from RHESSysIOinR that would be input for running RHESSys.
#' @return A list containing the program and its arguments.
#' @export
parse_rhessys_cmdout <- function(cmdout) {
  if (length(cmdout) != 1 || !nzchar(cmdout)) {
    stop("cmdout must be a single non-empty string.")
  }

  clean_cmd <- gsub('\\\\"', '"', cmdout)
  clean_cmd <- trimws(clean_cmd)

  inner_cmd <- clean_cmd
  if (grepl("wsl\\s+bash\\s+-lc", clean_cmd)) {
    inner_cmd <- sub('^.*wsl\\s+bash\\s+-lc\\s+"', "", clean_cmd)
    inner_cmd <- sub('"\\s*$', "", inner_cmd)
  }

  tokens <- strsplit(trimws(inner_cmd), "\\s+")[[1]]
  tokens <- tokens[nzchar(tokens)]

  if (length(tokens) < 2) {
    stop("Unable to parse RHESSys command from cmdout.")
  }

  list(
    program = tokens[1],
    args = tokens[-1]
  )
}

#' Write RHESSys launch JSON from command output
#'
#' @param cmdout The command output string from RHESSys.
#' @param launch_path The path to the launch JSON file.
#' @param config_name The name of the debug configuration.
#' @param cwd The current working directory for the debugger.
#' @param miDebuggerPath The path to the MI debugger.
#' @param program_override Optional program path override.
#' @return A list containing the path to the launch JSON and the launch object.
#' @export
write_rhessys_launch_json_from_cmd <- function(
  cmdout,
  launch_path = ".vscode/launch.json",
  config_name = "Debug RHESSys",
  cwd = "${workspaceFolder}",
  miDebuggerPath = "/usr/bin/gdb",
  program_override = NULL
) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required.")
  }

  parsed <- parse_rhessys_cmdout(cmdout)

  normalize_program <- function(path) {
    p <- gsub("\\\\", "/", path)
    p <- sub("^\\./", "", p)
    while (grepl("^\\.\\./", p)) {
      p <- sub("^\\.\\./", "", p)
    }
    file.path("${workspaceFolder}", p, fsep = "/")
  }

  program_path <- if (is.null(program_override)) parsed$program else program_override
  program_path <- normalize_program(program_path)

  launch_obj <- list(
    version = "0.2.0",
    configurations = list(
      list(
        name = config_name,
        type = "cppdbg",
        request = "launch",
        program = program_path,
        args = unname(parsed$args),
        stopAtEntry = FALSE,
        cwd = cwd,
        environment = list(),
        externalConsole = FALSE,
        MIMode = "gdb",
        miDebuggerPath = miDebuggerPath,
        setupCommands = list(
          list(
            description = "Enable pretty-printing for gdb",
            text = "-enable-pretty-printing",
            ignoreFailures = TRUE
          ),
          list(
            description = "Stop on signals",
            text = "handle SIGSEGV stop print",
            ignoreFailures = TRUE
          ),
          list(
            description = "Stop on aborts",
            text = "handle SIGABRT stop print",
            ignoreFailures = TRUE
          )
        )
      )
    )
  )

  dir.create(dirname(launch_path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(launch_path)) {
    existing <- jsonlite::read_json(launch_path)
    existing_names <- vapply(existing$configurations, `[[`, character(1), "name")
    new_config <- launch_obj$configurations[[1]]
    match_idx <- which(existing_names == config_name)
    if (length(match_idx) > 0) {
      existing$configurations[[match_idx]] <- new_config
    } else {
      existing$configurations <- c(existing$configurations, list(new_config))
    }
    launch_obj <- existing
  }

  jsonlite::write_json(launch_obj, path = launch_path, pretty = TRUE, auto_unbox = TRUE)

  list(path = normalizePath(launch_path, winslash = "/", mustWork = FALSE), launch = launch_obj)
}
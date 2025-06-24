
library(htmltools)

# Comment box functions
info_box <- function(text, title = "Info") {
  if (is_html_output()) {
    div(class = "info-box",
        HTML(paste0('<i class="fas fa-info-circle box-icon"></i><strong>', 
                    title, ':</strong> ', text)))
  } else if (is_latex_output()) {
    cat("\\begin{infobox}\n")
    cat("\\faInfoCircle\\ \\textbf{", title, ":} ", text, "\n")
    cat("\\end{infobox}\n")
  }
}

warning_box <- function(text, title = "Warning") {
  if (is_html_output()) {
    div(class = "warning-box",
        HTML(paste0('<i class="fas fa-exclamation-triangle box-icon"></i><strong>', 
                    title, ':</strong> ', text)))
  } else if (is_latex_output()) {
    cat("\\begin{warningbox}\n")
    cat("\\faExclamationTriangle\\ \\textbf{", title, ":} ", text, "\n")
    cat("\\end{warningbox}\n")
  }
}

tip_box <- function(text, title = "Tip") {
  if (is_html_output()) {
    div(class = "tip-box",
        HTML(paste0('<i class="fas fa-lightbulb box-icon"></i><strong>', 
                    title, ':</strong> ', text)))
  } else if (is_latex_output()) {
    cat("\\begin{tipbox}\n")
    cat("\\faLightbulb\\ \\textbf{", title, ":} ", text, "\n")
    cat("\\end{tipbox}\n")
  }
}

error_box <- function(text, title = "Error") {
  if (is_html_output()) {
    div(class = "error-box",
        HTML(paste0('<i class="fas fa-times-circle box-icon"></i><strong>', 
                    title, ':</strong> ', text)))
  } else if (is_latex_output()) {
    cat("\\begin{errorbox}\n")
    cat("\\faTimesCircle\\ \\textbf{", title, ":} ", text, "\n")
    cat("\\end{errorbox}\n")
  }
}

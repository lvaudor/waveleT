library(rstudioapi)

join_lines <- function() {
  context <- getActiveDocumentContext()

  # Récupérer la sélection
  sel <- context$selection[[1]]$range
  text <- context$selection[[1]]$text

  # Si rien n'est sélectionné, prendre la ligne courante + suivante
  if (text == "") {
    line_num <- sel$start[[1]]
    lines <- context$contents
    if (line_num < length(lines)) {
      # Concaténer, supprimer espaces/indentations multiples
      new_line <- gsub("\\s+", " ", paste0(trimws(lines[line_num]), " ", trimws(lines[line_num + 1])))
      lines[line_num] <- new_line
      lines <- lines[-(line_num + 1)]
      modifyRange(c(c(1,1), c(length(lines), nchar(lines[length(lines)]))), lines, id = context$id)
    }
  } else {
    # Fusionner les lignes sélectionnées et supprimer tous les espaces multiples
    new_text <- gsub("\\s+", " ", paste(trimws(unlist(strsplit(text, "\n"))), collapse = " "))
    modifyRange(sel, new_text, id = context$id)
  }
}

#' Get info regarding x if x is time or date values
#'
#'' @param x Numeric vector of time or date values.
Tproperties <- function(x, origin = x[1]) {
  t_as_date <- as.POSIXct(x, origin = origin)
  xb <- difftime(t_as_date[2:length(t_as_date)],
                 t_as_date[1:(length(t_as_date) - 1)])
  list(mean = mean(xb), units = units(xb))
}

#' Formats time as a numeric vector (for use with wavelet transforms function) and step size
#'
#' @param x the time vector
#' @param time_format the format of the time vector (as in `as.POSIXct` function)
#' @return A list with original time vector(x) and derived numeric vector (xt), as well as original step size (step) and numerical step (stept.
#' @export
#' @examples
#' data(cat_moods)
#' format_time(cat_moods$x,"%Y-%m-%d %H:%M:%S")
format_time=function(x, time_format){
  x=as.POSIXct(x, format=time_format, tz="UTC")
  numstep=median(as.numeric(diff(x)),na.rm=TRUE)
  units=units(diff(x))
  step=paste0(numstep," ",units)
  xt=as.numeric(difftime(x, time2=min(x)), units=units)
  return(list(x=x,xt=xt, step=step, numstep=numstep))
}

#' Extract numeric step from step string
#' @param step A string representing the step size, e.g. "3 hours".
#' @return Numeric value of the step size.
#' @examples
#' waveleT:::numeric_step("10 mins")
numeric_step=function(step){
  step_only_digits=!grepl(pattern="[A-Za-z]",step)
  if(step_only_digits){return(as.numeric(step))}
  step_parts=unlist(strsplit(step, " "))
  numstep=as.numeric(step_parts[1])
  return(numstep)
}

#' Crée un downloadHandler générique
#'
#' Cette fonction fournit un wrapper générique autour de `downloadHandler`.
#' Elle permet de factoriser totalement la construction des handlers en séparant
#' la logique de génération du nom de fichier et la logique d'écriture du fichier.
#'
#' @param id Integer. Identifiant (souvent 1 ou 2 dans les modules).
#' @param fig_or_info "Fig" or "Info
#' @param input L'objet `input` de Shiny.
#' @param filename_fun Fonction prenant `(input, id, ...)` et retournant un nom de fichier.
#' @param content_fun Fonction prenant `(file, input, id, ...)` et écrivant le fichier.
#' @param ... Arguments supplémentaires transmis aux deux fonctions `filename_fun` et `content_fun`.
#'
#' @return Un objet de type `downloadHandler`.
#' @export
#'
makeDownloadHandler <- function(id,
                                fig_or_info,
                                options=app_options(),
                                input,
                                content_fun,
                                analysis,
                                variable = NA,
                                type = NA,
                                info = NA,
                                period = NA,
                                ...) {
  if(fig_or_info=="Fig"){contentType="image/png"}
  if(fig_or_info=="Info"){contentType="text/csv"}
  f=downloadHandler(
    filename = function(...){
      name <- paste0(fig_or_info, "_", analysis)
      if (!is.na(variable)) name <- paste0(name, "_", variable)
      if(analysis=="CWT"){
        type=input[[paste0("CWT_plot_type",id)]]
        type=substr(type, 1, 1)
        info=input$info
        period=input[[paste0("period",id)]]
        if (!is.na(type)){name=paste0(name, "_type", type)}
        if (!is.na(info) & type != 3){name=paste0(name, "_", info)}
        if (!is.na(period) & type==2){name=paste0(name, "_T", period)}
      }
      if (fig_or_info == "Fig"){name=paste0(name, ".", options$graph_format)}
      if (fig_or_info == "Info"){name=paste0(name, ".csv")}
      return(name)
    },
    content  = function(file){
        if(fig_or_info=="Fig"){
            fgraph=get(options$graph_format)
            fgraph(filename=file,width = options$width, height = options$height)
            content_fun()
            dev.off()
        } else {
            result=content_fun()
            write.table(result,file=file,sep=";",row.names=FALSE)
        }
    },
    contentType=contentType
  )
  return(f)
}

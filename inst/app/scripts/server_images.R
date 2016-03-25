output$mylogo <- renderImage({
  list(src=findmypath("app/images","logo_toolkat_small.png"),
                       width="252px",height="180px")
}, deleteFile = FALSE)


for(x in 1:10){
  local({  ## sans le "local" tous les outputs renvoient sur une meme image
    i=x
    output[[paste0("catpaw",i)]]=renderImage({
      list(src=findmypath("app/images","catpaw.png"),width="18px",height="28px")
    },deleteFile=FALSE)
  })
}


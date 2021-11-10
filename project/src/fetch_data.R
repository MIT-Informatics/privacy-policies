###
### Privaseer policies
###
### Main site: https://privaseer.ist.psu.edu/
### Data site: https://drive.google.com/drive/folders/1zJFy13tqeWscvad-xUfAYGTG1Lhv74Mb?usp%20%20%20%20=sharing
###
### Note: contains tarballs for data and metadata, sample data and metdata, and a readme.

library(googledrive)

fetch_privaseer<-function(demo_only=TRUE){
  googledrive::drive_deauth()
  targetFolderURI <-  'https://drive.google.com/drive/folders/1zJFy13tqeWscvad-xUfAYGTG1Lhv74Mb'
  listing.drb <- drive_ls(targetFolderURI %>% as_id())
  listing.drb %<>% drive_reveal(.,"size") %>% mutate(size=replace_na(0))
  if (demo_only) {
    listing.drb %<>% filter(size < 100000000)
  }
  map_dfr( listing.drb %>% pull("id"),
           drive_download )
}

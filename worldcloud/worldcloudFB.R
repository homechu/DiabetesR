install.packages("Rfacebook")
install.packages("devtools")
library(devtools)
install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
library(Rfacebook)

token <- "EAACEdEose0cBADhfRqtbzYru86AuflhlCIeZAjIkBjhZAuEKLb1KO9ZBJrMzmkzdwkcyDRU5txxzbnTIKJ0FwqhZAgNjgra7J95CUyCsoiwurq66q04c7MvLPJy9Lj4S9v8TFjK5oQZBj9lxdMM1WFZCXKx3aD4npid8mIGO9GzquklNWWMq5ZBQN2BfoaGZB9gcCjPIqjfv5BZCUAmdpZB6dbgwdG1QV4NhoZD"

me <- getUsers("me", token, private_info = TRUE)
me$name

require("Rfacebook")

fb.oauth <- fbOAuth(
  app_id="2064529323788229",               #請複製貼上應用程式編號(ID數字)
  app_secret="b31af07f47215ef0b3d2b60506856778", #請複製貼上應用程式密鑰(Secret數字)
  extended_permissions = TRUE)

me <- getUsers("me",token=fb.oauth)
me$name

save(fb.oauth, file="fb_oauth")

page.id <- "1738563996468413" 
page <- getPage(page.id, token, n = 10)
str(page)
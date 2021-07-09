library(rdrop2)

GRglobalSettings<-new.env()


#'@export 
GRversion<-function()
{
  return("2021.07.07")
}




#'@export 
GRstatus<-function()
{
  return(GRglobalSettings)
}






#'@export 
GRcheckStatus<-function()
{
  if(is.null(GRglobalSettings$status)) stop("Not connected!")
}



#'@export 
GRconnect<-function(projectName, yourName="")
{
  my_path <- find.package("GRcomp")
  
  if(file.exists(paste0(my_path,"/data/token.RDS")))
  {
    token <- rdrop2::drop_auth(rdstoken=paste0(my_path,"/data/token.RDS"))
    message("Token was read from the original file.")
  }
  else
  {
    if(file.exists("token.RDS"))
    {
      token <- rdrop2::drop_auth(rdstoken="token.RDS")
      message("Token was read from local file.")
    }
    else
    {
      token <- rdrop2::drop_auth()
      saveRDS(token,"token.RDS")
      message("Token was saved to local file.")
    }
  }
  
  GRglobalSettings$token <- token
  
  GRglobalSettings$status <- rdrop2::drop_acc()
  GRglobalSettings$projectName<-projectName
  GRglobalSettings$yourName<-yourName
  if(rdrop2::drop_exists(projectName)==FALSE)
  {
    GRcreateFolder(projectName)
    message("A new folder was created.")
  }
  else
  {
    message("Connected to an existing folder.")
  }
}





#'@export 
GRdisconnect<-function()
{
  rm(list=ls(envir=GRglobalSettings),envir = GRglobalSettings)
}




GRcreateName<-function()
{
  tmp <- paste0(as.numeric(Sys.time()),"_",ifelse(GRglobalSettings$yourName=="",GRrandomStr(),GRglobalSettings$yourName))
  return(gsub("\\.","_",tmp))
}





GRrandomStr<-function()
{
  return(paste(sample(c(LETTERS,letters),replace = T,size = 16),collapse=""))
}



GRcreateFolder<-function(folder_name)
{
  rdrop2::drop_create(folder_name)
}



#'@export 
GRpush<-function(Robject, overWrite=FALSE, fileName=NULL)
{
  GRcheckStatus()
  
  if(is.null(fileName))
  {
    fileName <-paste0(GRcreateName(),".RDS")
  }

  dir<-tempdir()
  local_file<-paste0(dir,"/",fileName)
  
  saveRDS(Robject,file = local_file)
  
  rdrop2::drop_upload(file=local_file, path=GRglobalSettings$projectName)
  
  if(overWrite)
  {
    if(!is.null(GRglobalSettings$lastFileName) && GRglobalSettings$lastFileName!="")
    {
      rdrop2::drop_delete(paste0(GRglobalSettings$projectName,"/",GRglobalSettings$lastFileName))
    }
  }
  
  GRglobalSettings$lastFileName<-fileName
  file.remove(local_file)
  return(0)
}




#'@export 
GRupload <- function(local_file, dest_file=NULL)
{
  GRcheckStatus()

  tmp <- strsplit(local_file,"/")[[1]]
  pure_file_name <- tmp[length(tmp)]
  
  tmp <- tempdir()
  
  if(!is.null(dest_file))
  {
    file.copy(from=local_file, to=paste0(tmp,"/",dest_file))
    local_file <- paste0(tmp,"/",dest_file)
  }
  
  try(rdrop2::drop_delete(path = paste0(GRglobalSettings$projectName,"/",pure_file_name)), silent = T)
  rdrop2::drop_upload(file=local_file, path=paste0(GRglobalSettings$projectName))
}




#'@export 
GRlist<-function()
{
  GRcheckStatus()
  
  files <- rdrop2::drop_dir(path = paste0(path=GRglobalSettings$projectName))
  
  n<-dim(files)[1]
  if(n==0) return(c())
  dates<-rep("",n)
  nms<-files$name
  return(nms)
}




#'@export 
GRpull<-function(fileName)
{
  GRcheckStatus()
  local_file <- tempfile()
  res <- rdrop2::drop_download(path=paste0(GRglobalSettings$projectName,"/",fileName),local_path=local_file,overwrite=T, dtoken = GRglobalSettings$token)
  return(readRDS(local_file))
}




#'@export 
GRclean<-function()
{
  if(is.null(GRglobalSettings$status)) stop("Not connected!")

  files<-GRlist()
  counter<-0
  for(f in files)
  {
    rdrop2::drop_delete(paste0(GRglobalSettings$projectName,"/",f))
    counter<-counter+1
  }
  message(paste(counter,"files were deleted."))
}




#'@export 
GRrun<-function(file_name="run.R")
{
  local_folder <- tempdir()
  GRcheckStatus()
  rdrop2::drop_download(paste0(GRglobalSettings$projectName,"/",file_name),local_path =paste0(local_folder,"/",file_name),overwrite = T)
  setwd(local_folder)
  source(paste0(local_folder,"/",file_name))
}






#'@export 
GRcollect<-function()
{
  out<-NULL
  files<-GRlist()
  for(i in 1:length(files))
  {
    file<-files[i]
    if(substring(file,1,1)!="/")
    {
      if(i==1) out<-GRpull(file)
      else
      {
        if(is.vector(out))
          out<-c(out,GRpull(file))
        else
          out<-rbind(out,GRpull(file))
      }
    }
  }
  return(out)
}












GRnotify<-function()
{
  msg <- paste("Salam")# Define who the sender is
  sender <- "resplabubc@gmail.com"# Define who should get your email
  recipients <- c("msafavi@mail.ubc.ca")# Send your email with the send.mail function
  
  send.mail(from = sender,
            to = recipients,
            subject = "Top 10 cars dashboard",
            body = msg,
            smtp = list(host.name = "smtp.gmail.com", port = 587,
                        user.name = "resplabubc@gmail.com",
                        passwd = "resplabubc123", ssl = TRUE),
                        authenticate = TRUE,
                        send = TRUE)
}
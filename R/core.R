GRglobalSettings<-new.env()


#'@export 
GRversion<-function()
{
  return("2022.04.19")
}




#'@export 
GRstatus<-function()
{
  return(as.list(GRglobalSettings))
}






#'@export 
GRcheckStatus<-function()
{
  if(is.null(GRglobalSettings$status)) stop("Not connected!")
}





#'@export 
GRconnect<-function(projectName, machineId="")
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
  GRglobalSettings$machineId<-machineId
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
  tmp <- paste0(ifelse(GRglobalSettings$machineId=="",GRrandomStr(),GRglobalSettings$machineId),"_",as.numeric(Sys.time()))
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
GRupload <- function(localFile, destFile=NULL)
{
  GRcheckStatus()

  tmp <- strsplit(localFile,"/")[[1]]
  pureFileName <- tmp[length(tmp)]
  
  tmp <- tempdir()
  
  if(!is.null(destFile))
  {
    file.copy(from=localFile, to=paste0(tmp,"/",destFile))
    localFile <- paste0(tmp,"/",destFile)
  }
  
  try(rdrop2::drop_delete(path = paste0(GRglobalSettings$projectName,"/",pureFileName)), silent = T)
  rdrop2::drop_upload(file=localFile, path=paste0(GRglobalSettings$projectName))
}




#'@export 
GRlist<-function()
{
  GRcheckStatus()
  
  files <- rdrop2::drop_dir(path = paste0(GRglobalSettings$projectName))
  
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
GRdownload<-function(fileName)
{
  GRcheckStatus()
  local_file <- tempfile()
  res <- rdrop2::drop_download(path=paste0(GRglobalSettings$projectName,"/",fileName),local_path=local_file,overwrite=T, dtoken = GRglobalSettings$token)
  return(local_file)
}








#'@export 
GRclean<-function(delete_code=F)
{
  if(is.null(GRglobalSettings$status)) stop("Not connected!")

  files<-GRlist()
  counter<-0
  for(f in files)
  {
    if(substring(f,nchar(f)-1)!=".R" | delete_code==T)
    {
      message("deleting ",f)
      rdrop2::drop_delete(paste0(GRglobalSettings$projectName,"/",f))
      counter<-counter+1
    }
  }
  message(paste(counter,"files were deleted."))
}




#'@export 
GRrun<-function(fileName="run.R", instanceId=NULL)
{
  if(!is.null(instanceId)) .GlobalEnv$instanceId <- instanceId
  localFolder <- tempdir()
  GRcheckStatus()
  rdrop2::drop_download(paste0(GRglobalSettings$projectName,"/",fileName),local_path =paste0(localFolder,"/",fileName),overwrite = T)
  setwd(localFolder)
  source(paste0(localFolder,"/",fileName))
}






#'@export 
GRcollect<-function()
{
  out<-NULL
  files<-GRlist()
  for(i in 1:length(files))
  {
    file<-files[i]
    if(substring(file,1,1)!="/" && toupper(substring(file,nchar(file)-1))!=".R")
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


















#'@export
GRserver <- function(fileName="server.R", sleep=10)
{
  GRcheckStatus()
  fullFileName <- paste0(GRglobalSettings$projectName,"/",fileName)
  while(TRUE)
  {
    if(rdrop2::drop_exists(fullFileName))
    {
      localFile <- GRdownload(fileName)
      print(paste("Sourcing ", fileName))
      try(source(localFile))
      rdrop2::drop_delete(fullFileName)
    }
    print(paste(fullFileName,"not found; waiting for ",sleep,"seconds"))
    Sys.sleep(sleep)
  }
}





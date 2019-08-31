library(googledrive)

GRglobalSettings<-new.env()


#'@export 
GRversion<-function()
{
  return("2019.08.30.02")
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
  if(GRglobalSettings$status$user$emailAddress!="resplabubc@gmail.com") stop("Not the correct user.")
}



#'@export 
GRconnect<-function(projectName, yourName="")
{
  #invoke a simply funciton and it forces authentication (if it has not happened yet)
  googledrive::drive_auth()
  GRglobalSettings$status<-googledrive::drive_about()
  GRglobalSettings$projectName<-projectName
  GRglobalSettings$yourName<-yourName
  if(tolower(GRglobalSettings$status$user$emailAddress)!="resplabubc@gmail.com") stop("Not the correct user.")
  if(dim(googledrive::drive_find(projectName))[1]==0)
  {
    GRcreateFolder(projectName)
    message("A new folder was created.")
  }
  else
  {
    message("Connected to an existing folder.")
  }
}




GRcreateName<-function()
{
  return(paste0(as.numeric(Sys.time()),".",ifelse(GRglobalSettings$yourName=="",GRrandomStr(),GRglobalSettings$yourName)))
}





GRrandomStr<-function()
{
  return(paste(sample(c(LETTERS,letters),replace = T,size = 16),collapse=""))
}



GRcreateFolder<-function(projectName)
{
  googledrive::drive_mkdir(projectName)
}



#'@export 
GRpush<-function(Robject, overWrite=FALSE, objectName=GRcreateName())
{
  GRcheckStatus()
  
  dir<-tempdir()
  f<-paste0(dir,"\\",GRrandomStr())
  saveRDS(Robject,file = f)
  googledrive::drive_upload(media=f,path = paste0(GRglobalSettings$projectName,"/",objectName))
  
  if(overWrite)
  {
    if(!is.null(GRglobalSettings$lastObjectName) && GRglobalSettings$lastObjectName!="")
    {
      drive_rm(GRglobalSettings$lastObjectName)
    }
  }
  
  GRglobalSettings$lastObjectName<-objectName
}



#'@export 
GRdeleteAll<-function()
{
  GRcheckStatus()
  
  files<-googledrive::drive_find()
  drive_rm(files)
}



#'@export 
GRlist<-function(folder="")
{
  GRcheckStatus()
  
  files<-googledrive::drive_ls(paste0(path=GRglobalSettings$projectName,"/",folder))
  
  n<-dim(files)[1]
  if(n==0) return(c())
  dates<-rep("",n)
  nms<-files$name
  for(i in 1:n)
  {
    dates[i]<-files[i,]$drive_resource[[1]]$createdTime
    if(files[i,]$drive_resource[[1]]$mimeType=="application/vnd.google-apps.folder") nms[i]<-paste0("/",nms[i])
  }
  o<-order(dates)
  
  return(nms[o])
}




#'@export 
GRpull<-function(fileName)
{
  GRcheckStatus()
  
  res<-googledrive::drive_download(fileName,path=tempfile())
  return(readRDS(res$local_path))
}




#'@export 
GRclean<-function()
{
  if(is.null(GRglobalSettings$status)) stop("Not connected!")
  if(GRglobalSettings$status$user$emailAddress!="resplabubc@gmail.com") stop("Not the correct user.")
  
  files<-GRlist()
  counter<-0
  for(f in files)
  {
    if(substring(f,first = nchar(f)-1)!=".R")
    {
      drive_rm(f)
      counter<-counter+1
    }
  }
  message(paste(counter,"files were deleted."))
}




#'@export 
GRrun<-function()
{
  GRcheckStatus()
  x<-GRlist()
  if(is.na(match("/RUN",x))) stop("No RUN folder was found.")
  path<-tempdir()
  message(paste("Local folder is",path))
  files<-GRlist("RUN")
  for(fl in files)
  {
    if(substring(fl,1,1)=="/")
    {
      message("Warning: subfolder detectd; content ignored.")
    }
    else
    {
      message(paste("Doanloading",fl))
      googledrive::drive_download(fl,paste0(path,"/",fl),overwrite = T)
    }
  }
  setwd(path)
  source(paste0(path,"/GRrun.R"))
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





#'@export
GRformatOutput<-function(x,rGroupVars,cGroupVars,func=GRformatOutput.defaultProcessor)
{
  x<-as.data.frame(x)
  
  allGroups<-paste0(paste0(rGroupVars,collapse=","),",",paste0(cGroupVars,collapse=","))
  ssql<-paste0("SELECT ", allGroups," FROM x GROUP BY ",allGroups)
  y<-sqldf(ssql)
  
  z<-sqldf(paste0("SELECT DISTINCT ", paste0(rGroupVars,collapse=",") ," FROM y"))
  n_row<-dim(z)[1]
  row_names<-rep("",n_row)
  for(i in 1:dim(z)[2])
  {
    if(i>1) row_names<-paste0(row_names,",")
    row_names<-paste0(row_names,paste0(names(z)[i],":",z[,i]))
  }
  z<-sqldf(paste0("SELECT DISTINCT ", paste0(cGroupVars,collapse=",") ," FROM y"))
  n_col<-dim(z)[1]
  col_names<-rep("",n_col)
  for(i in 1:dim(z)[2])
  {
    if(i>1) col_names<-paste0(col_names,",")
    col_names<-paste0(col_names,paste0(names(z)[i],":",z[,i]))
  }
  
  
  ssql<-"SELECT * FROM x WHERE 1"
  for(i in 1:dim(y)[2])
  {
    ssql<-paste0(ssql," AND ",colnames(y)[i],"=")
    if(is.numeric(y[,i])) 
    {
      ssql<-paste0(ssql,"%f")
    }
    else
    {
      ssql<-paste0(ssql,"'%s'")
    }
  }
  
  message(ssql)
  
  out<-structure(rep(" ",n_row*n_col),.Dim=c(n_row,n_col))
  rownames(out)<-row_names
  colnames(out)<-col_names
  i_row<-1
  i_col<-1
  for(i in 1:(n_row*n_col))
  {
    cat(".")
    ssql2<-do.call(sprintf,args=c(ssql,y[i,]))
    
    res<-func(sqldf(ssql2))
    out[i_row,i_col]<-res
    i_col<-i_col+1
    if(i_col>n_col)
    {
      i_row<-i_row+1
      i_col<-1
    }
  }
  return(out)
}






GRformatOutput.defaultProcessor<-function(data)
{
  out<-""
  n<-dim(data)[2]
  nms<-colnames(data)
  for(i in 1:n)
  {
    if(is.numeric(data[,i]))
    {
      out<-paste0(out,nms[i],":mean=",mean(data[,i]),"(SD=",sd(data[,i]),"); ")
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
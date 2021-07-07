library(rdrop2)

GRglobalSettings<-new.env()


#'@export 
GRversion<-function()
{
  return("2021.07.06")
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
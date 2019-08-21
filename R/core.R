library(googledrive)

GRglobalSettings<-new.env()



GRcheckStatus<-function()
{
  if(is.null(GRglobalSettings$status)) stop("Not connected!")
  if(GRglobalSettings$status$user$emailAddress!="resplabubc@gmail.com") stop("Not the correct user.")
}


GRconnect<-function(projectName,yourName="")
{
  #invoke a simply funciton and it forces authentication (if it has not happened yet)
  googledrive::drive_auth()
  GRglobalSettings$status<-googledrive::drive_about()
  GRglobalSettings$projectName<-projectName
  GRglobalSettings$yourName<-yourName
  if(GRglobalSettings$status$user$emailAddress!="resplabubc@gmail.com") stop("Not the correct user.")
  if(dim(drive_find(projectName))[1]==0)  GRcreateFolder(projectName)
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




GRpush<-function(Robject, overWrite=FALSE, objectName=GRcreateName())
{
  GRcheckStatus()
  
  dir<-tempdir()
  f<-paste0(dir,"\\",GRrandomStr())
  saveRDS(Robject,file = f)
  drive_upload(media=f,path = paste0(GRglobalSettings$projectName,"/",objectName))
  
  if(overWrite)
  {
    if(!is.null(GRglobalSettings$lastObjectName) && GRglobalSettings$lastObjectName!="")
    {
      drive_rm(GRglobalSettings$lastObjectName)
    }
  }
  
  GRglobalSettings$lastObjectName<<-objectName
}



GRdeleteAll<-function()
{
  GRcheckStatus()
  
  files<-drive_find()
  drive_rm(files)
}



GRlist<-function()
{
  GRcheckStatus()
  
  files<-googledrive::drive_ls(path=GRglobalSettings$projectName)
  
  n<-dim(files)[1]
  if(n==0) return(c())
  dates<-rep("",n)
  for(i in 1:n)
  {
    dates[i]<-files[i,]$drive_resource[[1]]$createdTime
  }
  o<-order(dates)
  return(files$name[o])
}



GRpull<-function(fileName)
{
  GRcheckStatus()
  
  res<-drive_download(fileName,path=tempfile())
  return(readRDS(res$local_path))
}




GRclean<-function()
{
  if(is.null(GRglobalSettings$status)) stop("Not connected!")
  if(GRglobalSettings$status$user$emailAddress!="resplabubc@gmail.com") stop("Not the correct user.")
  
  files<-GRlist()
  for(file in files)
  {
    drive_rm(file)
  }
}




GRcollect<-function()
{
  out<-NULL
  files<-GRlist()
  for(i in 1:length(files))
  {
    file<-files[i]
    if(i==1) out<-GRpull(file)
    else
    {
      if(is.vector(out))
        out<-c(out,GRpull(file))
      else
        out<-rbind(out,GRpull(file))
    }
      
  }
  return(out)
}






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
## ~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~     Initialisation     ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

rm(list=ls())

setwd("/Users/ful083/Work/Cumulative_Impacts_&_IEA_&_ERA/FRDC_cum_impacts_fisheries_project/Analysis_&_Workup/EwE_sims")

basefile <- read.table("NoF.csv", header=FALSE, skip = 3, sep=",")
fleetnames <- c("Trawl", "Non-trawl", "Line", "NSW trawl", "Vic trawl", "Scallop", "Squid", "Trap", "Danish Seine", "Tuna Longline", "Recreational")
fleetcodes <- c("Tw", "NTw", "L", "NSW", "V", "Sc", "Sq", "Tp", "DS", "TL", "Rec")

line1 <- "Name,Trawl,Non-trawl,Line,NSW trawl,Vic trawl,Scallop,Squid,Trap,Danish Seine,Tuna Longline,Recreational"
line2 <- "Pool code,1,2,3,4,5,6,7,8,9,10,11"
line3 <- "Type,3,3,3,3,3,3,3,3,3,3,3"

nfleet <- length(fleetnames)
numTimeSteps <- 600

# One way adjustments
for (i in 2:36) {
  # val to fill in
  if(i < 27) {
    val <- (i-1) * 0.2 
  } else {
    val <- (i-26) * 0.5 + 5.0
  }
  
  # base array to fill
  ncol <- nfleet + 1
  zeroed <- array(rep(0, ncol*numTimeSteps), dim=c(numTimeSteps,nfleet+1))
  
  for(f in 1:nfleet){
    filename <- paste(fleetcodes[f],"_",i,".csv",sep="")
    newdat <- zeroed
    for(nr in 1:numTimeSteps) {
      newdat[nr,1] <- nr
    }
    newdat[,f+1] <- val  # assign value to appropriate column
    # Write file
    cat(line1,file=filename,sep="\n")
    cat(line2,file=filename,sep="\n",append=TRUE)
    cat(line3,file=filename,sep="\n",append=TRUE)
    write.table(newdat, file = filename, row.names = FALSE,col.names = FALSE, sep = ",", append = TRUE)
  }
}

# Two way adjustments
for (i in 2:36) {
  # val to fill in
  if(i < 27) {
    val <- (i-1) * 0.2 
  } else {
    val <- (i-26) * 0.5 + 5.0
  }
  
  # base array to fill
  ncol <- nfleet + 1
  zeroed <- array(rep(0, ncol*numTimeSteps), dim=c(numTimeSteps,nfleet+1))
  
  fleetend <- nfleet-1
  for(f in 1:fleetend){
    newdat <- zeroed
    for(nr in 1:numTimeSteps) {
      newdat[nr,1] <- nr
    }
    newdat[,f+1] <- val  # assign value to appropriate column
    kstart <- f+1
    for(k in kstart:nfleet) {
      filename <- paste(fleetcodes[f],"_",fleetcodes[k],"_",i,".csv",sep="")
      newdat[,k+1] <- val  # assign value to appropriate column
      # Write file
      cat(line1,file=filename,sep="\n")
      cat(line2,file=filename,sep="\n",append=TRUE)
      cat(line3,file=filename,sep="\n",append=TRUE)
      write.table(newdat, file = filename, row.names = FALSE,col.names = FALSE, sep = ",", append = TRUE)
      
      # Rezero for next time
      newdat[,k+1] <- 0
    }
  }
}

# Three way adjustments
for (i in 2:36) {
  # val to fill in
  if(i < 27) {
    val <- (i-1) * 0.2 
  } else {
    val <- (i-26) * 0.5 + 5.0
  }
  
  # base array to fill
  ncol <- nfleet + 1
  zeroed <- array(rep(0, ncol*numTimeSteps), dim=c(numTimeSteps,nfleet+1))
  
  fleetend <- nfleet-2
  for(f in 1:fleetend){
    newdat <- zeroed
    for(nr in 1:numTimeSteps) {
      newdat[nr,1] <- nr
    }
    newdat[,f+1] <- val  # assign value to appropriate column
    kstart <- f+1
    kend <- nfleet-1
    for(k in kstart:kend) {
      jstart <- k+1
      newdat[,k+1] <- val  # assign value to appropriate column
      for(j in jstart:nfleet) {
        filename <- paste(fleetcodes[f],"_",fleetcodes[k],"_",fleetcodes[j],"_",i,".csv",sep="")
        newdat[,j+1] <- val  # assign value to appropriate column
        # Write file
        cat(line1,file=filename,sep="\n")
        cat(line2,file=filename,sep="\n",append=TRUE)
        cat(line3,file=filename,sep="\n",append=TRUE)
        write.table(newdat, file = filename, row.names = FALSE,col.names = FALSE, sep = ",", append = TRUE)
        
        # Rezero for next time
        newdat[,j+1] <- 0
      }
      newdat[,k+1] <- 0
    }
  }
}

# Four way adjustments
for (i in 2:36) {
  # val to fill in
  if(i < 27) {
    val <- (i-1) * 0.2 
  } else {
    val <- (i-26) * 0.5 + 5.0
  }
  
  # base array to fill
  ncol <- nfleet + 1
  zeroed <- array(rep(0, ncol*numTimeSteps), dim=c(numTimeSteps,nfleet+1))
  
  fleetend <- nfleet-3
  for(f in 1:fleetend){
    newdat <- zeroed
    for(nr in 1:numTimeSteps) {
      newdat[nr,1] <- nr
    }
    newdat[,f+1] <- val  # assign value to appropriate column
    kstart <- f+1
    kend <- nfleet-2
    for(k in kstart:kend) {
      jstart <- k+1
      newdat[,k+1] <- val  # assign value to appropriate column
      jend <- nfleet-1
      for(j in jstart:jend) {
        xstart <- j+1
        newdat[,j+1] <- val  # assign value to appropriate column
        for(x in xstart:nfleet) {
          filename <- paste(fleetcodes[f],"_",fleetcodes[k],"_",fleetcodes[j],"_",fleetcodes[x],"_",i,".csv",sep="")
          newdat[,x+1] <- val  # assign value to appropriate column
          # Write file
          cat(line1,file=filename,sep="\n")
          cat(line2,file=filename,sep="\n",append=TRUE)
          cat(line3,file=filename,sep="\n",append=TRUE)
          write.table(newdat, file = filename, row.names = FALSE,col.names = FALSE, sep = ",", append = TRUE)
          
          # Rezero for next time
          newdat[,x+1] <- 0
        }
        newdat[,j+1] <- 0
      }
      newdat[,k+1] <- 0
    }
  }
}

# Five way adjustments
for (i in 2:36) {
  # val to fill in
  if(i < 27) {
    val <- (i-1) * 0.2 
  } else {
    val <- (i-26) * 0.5 + 5.0
  }
  
  # base array to fill
  ncol <- nfleet + 1
  zeroed <- array(rep(0, ncol*numTimeSteps), dim=c(numTimeSteps,nfleet+1))
  
  fleetend <- nfleet-4
  for(f in 1:fleetend){
    newdat <- zeroed
    for(nr in 1:numTimeSteps) {
      newdat[nr,1] <- nr
    }
    newdat[,f+1] <- val  # assign value to appropriate column
    kstart <- f+1
    kend <- nfleet-3
    for(k in kstart:kend) {
      jstart <- k+1
      newdat[,k+1] <- val  # assign value to appropriate column
      jend <- nfleet-2
      for(j in jstart:jend) {
        xstart <- j+1
        newdat[,j+1] <- val  # assign value to appropriate column
        xend <- nfleet-1
        for(x in xstart:xend) {
          ystart <- x+1
          newdat[,x+1] <- val  # assign value to appropriate column
          for(y in ystart:nfleet) {
            filename <- paste(fleetcodes[f],"_",fleetcodes[k],"_",fleetcodes[j],"_",fleetcodes[x],"_",fleetcodes[y],"_",i,".csv",sep="")
            newdat[,y+1] <- val  # assign value to appropriate column
            # Write file
            cat(line1,file=filename,sep="\n")
            cat(line2,file=filename,sep="\n",append=TRUE)
            cat(line3,file=filename,sep="\n",append=TRUE)
            write.table(newdat, file = filename, row.names = FALSE,col.names = FALSE, sep = ",", append = TRUE)
            
            # Rezero for next time
            newdat[,y+1] <- 0
          }
          newdat[,x+1] <- 0
        }
        newdat[,j+1] <- 0
      }
      newdat[,k+1] <- 0
    }
  }
}
# Six way adjustments
for (i in 2:36) {
  # val to fill in
  if(i < 27) {
    val <- (i-1) * 0.2 
  } else {
    val <- (i-26) * 0.5 + 5.0
  }
  
  # base array to fill
  ncol <- nfleet + 1
  zeroed <- array(rep(0, ncol*numTimeSteps), dim=c(numTimeSteps,nfleet+1))
  
  fleetend <- nfleet-5
  for(f in 1:fleetend){
    newdat <- zeroed
    for(nr in 1:numTimeSteps) {
      newdat[nr,1] <- nr
    }
    newdat[,f+1] <- val  # assign value to appropriate column
    kstart <- f+1
    kend <- nfleet-4
    for(k in kstart:kend) {
      jstart <- k+1
      newdat[,k+1] <- val  # assign value to appropriate column
      jend <- nfleet-3
      for(j in jstart:jend) {
        xstart <- j+1
        newdat[,j+1] <- val  # assign value to appropriate column
        xend <- nfleet-2
        for(x in xstart:xend) {
          ystart <- x+1
          newdat[,x+1] <- val  # assign value to appropriate column
          yend <- nfleet-1
          for(y in ystart:yend) {
            zstart <- y+1
            newdat[,y+1] <- val  # assign value to appropriate column
            for(z in zstart:nfleet) {
              filename <- paste(fleetcodes[f],"_",fleetcodes[k],"_",fleetcodes[j],"_",fleetcodes[x],"_",fleetcodes[y],"_",fleetcodes[z],"_",i,".csv",sep="")
              newdat[,z+1] <- val  # assign value to appropriate column
              # Write file
              cat(line1,file=filename,sep="\n")
              cat(line2,file=filename,sep="\n",append=TRUE)
              cat(line3,file=filename,sep="\n",append=TRUE)
              write.table(newdat, file = filename, row.names = FALSE,col.names = FALSE, sep = ",", append = TRUE)
              
              # Rezero for next time
              newdat[,z+1] <- 0
            }
            newdat[,y+1] <- 0
          }
          newdat[,x+1] <- 0
        }
        newdat[,j+1] <- 0
      }
      newdat[,k+1] <- 0
    }
  }
}

# Seven way adjustments
for (i in 2:36) {
  # val to fill in
  if(i < 27) {
    val <- (i-1) * 0.2 
  } else {
    val <- (i-26) * 0.5 + 5.0
  }
  
  # base array to fill
  ncol <- nfleet + 1
  zeroed <- array(rep(0, ncol*numTimeSteps), dim=c(numTimeSteps,nfleet+1))
  
  fleetend <- nfleet-6
  for(f in 1:fleetend){
    newdat <- zeroed
    for(nr in 1:numTimeSteps) {
      newdat[nr,1] <- nr
    }
    newdat[,f+1] <- val  # assign value to appropriate column
    kstart <- f+1
    kend <- nfleet-5
    for(k in kstart:kend) {
      jstart <- k+1
      newdat[,k+1] <- val  # assign value to appropriate column
      jend <- nfleet-4
      for(j in jstart:jend) {
        xstart <- j+1
        newdat[,j+1] <- val  # assign value to appropriate column
        xend <- nfleet-3
        for(x in xstart:xend) {
          ystart <- x+1
          newdat[,x+1] <- val  # assign value to appropriate column
          yend <- nfleet-2
          for(y in ystart:yend) {
            zstart <- y+1
            newdat[,y+1] <- val  # assign value to appropriate column
            zend <- nfleet-1
            for(z in zstart:zend) {
              astart <- z+1
              newdat[,z+1] <- val  # assign value to appropriate column
              for(a in astart:nfleet) {
                filename <- paste(fleetcodes[f],"_",fleetcodes[k],"_",fleetcodes[j],"_",fleetcodes[x],"_",fleetcodes[y],"_",fleetcodes[z],"_",fleetcodes[a],"_",i,".csv",sep="")
                newdat[,a+1] <- val  # assign value to appropriate column
                # Write file
                cat(line1,file=filename,sep="\n")
                cat(line2,file=filename,sep="\n",append=TRUE)
                cat(line3,file=filename,sep="\n",append=TRUE)
                write.table(newdat, file = filename, row.names = FALSE,col.names = FALSE, sep = ",", append = TRUE)
                
                # Rezero for next time
                newdat[,a+1] <- 0
              }
              newdat[,z+1] <- 0
            }
            newdat[,y+1] <- 0
          }
          newdat[,x+1] <- 0
        }
        newdat[,j+1] <- 0
      }
      newdat[,k+1] <- 0
    }
  }
}

# Eight way adjustments
for (i in 2:36) {
  # val to fill in
  if(i < 27) {
    val <- (i-1) * 0.2 
  } else {
    val <- (i-26) * 0.5 + 5.0
  }
  
  # base array to fill
  ncol <- nfleet + 1
  zeroed <- array(rep(0, ncol*numTimeSteps), dim=c(numTimeSteps,nfleet+1))
  
  fleetend <- nfleet-7
  for(f in 1:fleetend){
    newdat <- zeroed
    for(nr in 1:numTimeSteps) {
      newdat[nr,1] <- nr
    }
    newdat[,f+1] <- val  # assign value to appropriate column
    kstart <- f+1
    kend <- nfleet-6
    for(k in kstart:kend) {
      jstart <- k+1
      newdat[,k+1] <- val  # assign value to appropriate column
      jend <- nfleet-5
      for(j in jstart:jend) {
        xstart <- j+1
        newdat[,j+1] <- val  # assign value to appropriate column
        xend <- nfleet-4
        for(x in xstart:xend) {
          ystart <- x+1
          newdat[,x+1] <- val  # assign value to appropriate column
          yend <- nfleet-3
          for(y in ystart:yend) {
            zstart <- y+1
            newdat[,y+1] <- val  # assign value to appropriate column
            zend <- nfleet-2
            for(z in zstart:zend) {
              astart <- z+1
              newdat[,z+1] <- val  # assign value to appropriate column
              aend <- nfleet-1
              for(a in astart:aend) {
                bstart <- a+1
                newdat[,a+1] <- val  # assign value to appropriate column
                for(b in bstart:nfleet) {
                  filename <- paste(fleetcodes[f],"_",fleetcodes[k],"_",fleetcodes[j],"_",fleetcodes[x],"_",fleetcodes[y],"_",fleetcodes[z],"_",fleetcodes[a],"_",fleetcodes[b],"_",i,".csv",sep="")
                  newdat[,b+1] <- val  # assign value to appropriate column
                  # Write file
                  cat(line1,file=filename,sep="\n")
                  cat(line2,file=filename,sep="\n",append=TRUE)
                  cat(line3,file=filename,sep="\n",append=TRUE)
                  write.table(newdat, file = filename, row.names = FALSE,col.names = FALSE, sep = ",", append = TRUE)
                  
                  # Rezero for next time
                  newdat[,b+1] <- 0
                }
                newdat[,a+1] <- 0
              }
              newdat[,z+1] <- 0
            }
            newdat[,y+1] <- 0
          }
          newdat[,x+1] <- 0
        }
        newdat[,j+1] <- 0
      }
      newdat[,k+1] <- 0
    }
  }
}

# Nine way adjustments
for (i in 2:36) {
  # val to fill in
  if(i < 27) {
    val <- (i-1) * 0.2 
  } else {
    val <- (i-26) * 0.5 + 5.0
  }
  
  # base array to fill
  ncol <- nfleet + 1
  zeroed <- array(rep(0, ncol*numTimeSteps), dim=c(numTimeSteps,nfleet+1))
  
  fleetend <- nfleet-8
  for(f in 1:fleetend){
    newdat <- zeroed
    for(nr in 1:numTimeSteps) {
      newdat[nr,1] <- nr
    }
    newdat[,f+1] <- val  # assign value to appropriate column
    kstart <- f+1
    kend <- nfleet-7
    for(k in kstart:kend) {
      jstart <- k+1
      newdat[,k+1] <- val  # assign value to appropriate column
      jend <- nfleet-6
      for(j in jstart:jend) {
        xstart <- j+1
        newdat[,j+1] <- val  # assign value to appropriate column
        xend <- nfleet-5
        for(x in xstart:xend) {
          ystart <- x+1
          newdat[,x+1] <- val  # assign value to appropriate column
          yend <- nfleet-4
          for(y in ystart:yend) {
            zstart <- y+1
            newdat[,y+1] <- val  # assign value to appropriate column
            zend <- nfleet-3
            for(z in zstart:zend) {
              astart <- z+1
              newdat[,z+1] <- val  # assign value to appropriate column
              aend <- nfleet-2
              for(a in astart:aend) {
                bstart <- a+1
                newdat[,a+1] <- val  # assign value to appropriate column
                bend <- nfleet-1
                for(b in bstart:bend) {
                  cstart <- b+1
                  newdat[,b+1] <- val  # assign value to appropriate column
                  for(c in cstart:nfleet) {
                    filename <- paste(fleetcodes[f],"_",fleetcodes[k],"_",fleetcodes[j],"_",fleetcodes[x],"_",fleetcodes[y],"_",fleetcodes[z],"_",fleetcodes[a],"_",fleetcodes[b],"_",fleetcodes[c],"_",i,".csv",sep="")
                    newdat[,c+1] <- val  # assign value to appropriate column
                    # Write file
                    cat(line1,file=filename,sep="\n")
                    cat(line2,file=filename,sep="\n",append=TRUE)
                    cat(line3,file=filename,sep="\n",append=TRUE)
                    write.table(newdat, file = filename, row.names = FALSE,col.names = FALSE, sep = ",", append = TRUE)
                    
                    # Rezero for next time
                    newdat[,c+1] <- 0
                  }
                  newdat[,b+1] <- 0
                }
                newdat[,a+1] <- 0
              }
              newdat[,z+1] <- 0
            }
            newdat[,y+1] <- 0
          }
          newdat[,x+1] <- 0
        }
        newdat[,j+1] <- 0
      }
      newdat[,k+1] <- 0
    }
  }
}

# Ten way adjustments
for (i in 2:36) {
  # val to fill in
  if(i < 27) {
    val <- (i-1) * 0.2 
  } else {
    val <- (i-26) * 0.5 + 5.0
  }
  
  # base array to fill
  ncol <- nfleet + 1
  zeroed <- array(rep(0, ncol*numTimeSteps), dim=c(numTimeSteps,nfleet+1))
  
  fleetend <- nfleet-9
  for(f in 1:fleetend){
    newdat <- zeroed
    for(nr in 1:numTimeSteps) {
      newdat[nr,1] <- nr
    }
    newdat[,f+1] <- val  # assign value to appropriate column
    kstart <- f+1
    kend <- nfleet-8
    for(k in kstart:kend) {
      jstart <- k+1
      newdat[,k+1] <- val  # assign value to appropriate column
      jend <- nfleet-7
      for(j in jstart:jend) {
        xstart <- j+1
        newdat[,j+1] <- val  # assign value to appropriate column
        xend <- nfleet-6
        for(x in xstart:xend) {
          ystart <- x+1
          newdat[,x+1] <- val  # assign value to appropriate column
          yend <- nfleet-5
          for(y in ystart:yend) {
            zstart <- y+1
            newdat[,y+1] <- val  # assign value to appropriate column
            zend <- nfleet-4
            for(z in zstart:zend) {
              astart <- z+1
              newdat[,z+1] <- val  # assign value to appropriate column
              aend <- nfleet-3
              for(a in astart:aend) {
                bstart <- a+1
                newdat[,a+1] <- val  # assign value to appropriate column
                bend <- nfleet-2
                for(b in bstart:bend) {
                  cstart <- b+1
                  newdat[,b+1] <- val  # assign value to appropriate column
                  cend <- nfleet-1
                  for(c in cstart:cend) {
                    dstart <- c+1
                    newdat[,c+1] <- val  # assign value to appropriate column
                    for(d in dstart:nfleet) {
                      filename <- paste(fleetcodes[f],"_",fleetcodes[k],"_",fleetcodes[j],"_",fleetcodes[x],"_",fleetcodes[y],"_",fleetcodes[z],"_",fleetcodes[a],"_",fleetcodes[b],"_",fleetcodes[c],"_",fleetcodes[d],"_",i,".csv",sep="")
                      newdat[,d+1] <- val  # assign value to appropriate column
                      # Write file
                      cat(line1,file=filename,sep="\n")
                      cat(line2,file=filename,sep="\n",append=TRUE)
                      cat(line3,file=filename,sep="\n",append=TRUE)
                      write.table(newdat, file = filename, row.names = FALSE,col.names = FALSE, sep = ",", append = TRUE)
                      
                      # Rezero for next time
                      newdat[,d+1] <- 0
                    }
                    newdat[,c+1] <- 0
                  }
                  newdat[,b+1] <- 0
                }
                newdat[,a+1] <- 0
              }
              newdat[,z+1] <- 0
            }
            newdat[,y+1] <- 0
          }
          newdat[,x+1] <- 0
        }
        newdat[,j+1] <- 0
      }
      newdat[,k+1] <- 0
    }
  }
}

# Eleven way adjustments
for (i in 2:36) {
  # val to fill in
  if(i < 27) {
    val <- (i-1) * 0.2 
  } else {
    val <- (i-26) * 0.5 + 5.0
  }
  
  # base array to fill
  ncol <- nfleet + 1
  zeroed <- array(rep(0, ncol*numTimeSteps), dim=c(numTimeSteps,nfleet+1))
  
  fleetend <- nfleet-10
  for(f in 1:fleetend){
    newdat <- zeroed
    for(nr in 1:numTimeSteps) {
      newdat[nr,1] <- nr
    }
    newdat[,f+1] <- val  # assign value to appropriate column
    kstart <- f+1
    kend <- nfleet-9
    for(k in kstart:kend) {
      jstart <- k+1
      newdat[,k+1] <- val  # assign value to appropriate column
      jend <- nfleet-8
      for(j in jstart:jend) {
        xstart <- j+1
        newdat[,j+1] <- val  # assign value to appropriate column
        xend <- nfleet-7
        for(x in xstart:xend) {
          ystart <- x+1
          newdat[,x+1] <- val  # assign value to appropriate column
          yend <- nfleet-6
          for(y in ystart:yend) {
            zstart <- y+1
            newdat[,y+1] <- val  # assign value to appropriate column
            zend <- nfleet-5
            for(z in zstart:zend) {
              astart <- z+1
              newdat[,z+1] <- val  # assign value to appropriate column
              aend <- nfleet-4
              for(a in astart:aend) {
                bstart <- a+1
                newdat[,a+1] <- val  # assign value to appropriate column
                bend <- nfleet-3
                for(b in bstart:bend) {
                  cstart <- b+1
                  newdat[,b+1] <- val  # assign value to appropriate column
                  cend <- nfleet-2
                  for(c in cstart:cend) {
                    dstart <- c+1
                    newdat[,c+1] <- val  # assign value to appropriate column
                    dend <- nfleet-1
                    for(d in dstart:dend) {
                      gstart <- d+1
                      newdat[,d+1] <- val  # assign value to appropriate column
                      for(g in gstart:nfleet) {
                        filename <- paste(fleetcodes[f],"_",fleetcodes[k],"_",fleetcodes[j],"_",fleetcodes[x],"_",fleetcodes[y],"_",fleetcodes[z],"_",fleetcodes[a],"_",fleetcodes[b],"_",fleetcodes[c],"_",fleetcodes[d],"_",fleetcodes[g],"_",i,".csv",sep="")
                        newdat[,g+1] <- val  # assign value to appropriate column
                        # Write file
                        cat(line1,file=filename,sep="\n")
                        cat(line2,file=filename,sep="\n",append=TRUE)
                        cat(line3,file=filename,sep="\n",append=TRUE)
                        write.table(newdat, file = filename, row.names = FALSE,col.names = FALSE, sep = ",", append = TRUE)
                      
                        # Rezero for next time
                        newdat[,g+1] <- 0
                      }
                      newdat[,d+1] <- 0
                    }
                    newdat[,c+1] <- 0
                  }
                  newdat[,b+1] <- 0
                }
                newdat[,a+1] <- 0
              }
              newdat[,z+1] <- 0
            }
            newdat[,y+1] <- 0
          }
          newdat[,x+1] <- 0
        }
        newdat[,j+1] <- 0
      }
      newdat[,k+1] <- 0
    }
  }
}

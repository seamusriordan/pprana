year = "2013"

filename = paste("years_", year, "_fantasy_fantasy.csv", sep="")

print(filename)

stats = read.csv(filename)

nstat = 30

legx = 400
legy = 0.020

txt_sym = 390
txt_num = 430

minval = 0
maxval = 500

basey = 0.015

stats$FPfull <- stats$Rec*1.0 + stats$PassYds*0.04 + stats$PassTD*4 + stats$Int*-2 + stats$RushYds*0.1 + stats$RushTD*6 + stats$RecYds*0.1 + stats$RecTD*6
stats$FPhalf <- stats$Rec*0.5 + stats$PassYds*0.04 + stats$PassTD*4 + stats$Int*-2 + stats$RushYds*0.1 + stats$RushTD*6 + stats$RecYds*0.1 + stats$RecTD*6
stats$FPqtr  <- stats$Rec*0.25 + stats$PassYds*0.04 + stats$PassTD*4 + stats$Int*-2 + stats$RushYds*0.1 + stats$RushTD*6 + stats$RecYds*0.1 + stats$RecTD*6
stats$FPnone <- stats$PassYds*0.04 + stats$PassTD*4 + stats$Int*-2 + stats$RushYds*0.1 + stats$RushTD*6 + stats$RecYds*0.1 + stats$RecTD*6


rbstats <- stats[stats$Pos=="RB",]
wrstats <- stats[stats$Pos=="WR",]

png(paste(year,"/fullppr.png", sep=""))
rbf <- hist(head(rbstats[order(rbstats$FPfull, decreasing=TRUE),]$FPfull,nstat), breaks=seq(minval,maxval,by=30), xlim=c(50,500), ylim=c(0,0.02), freq=FALSE,  col=rgb(0.1,0.1,1.0,0.5), main=paste("1 PPR - ", year), xlab="Points")
wrf <- hist(head(wrstats[order(wrstats$FPfull, decreasing=TRUE),]$FPfull,nstat), breaks=seq(minval,maxval,by=30), xlim=c(50,500), freq=FALSE,  col=rgb(1.0,0.1,0.1,0.5), add=TRUE)
temp <- legend(legx,legy,c("RB", "WR"), lty=c(1,1), lwd=c(10,10), col=c("blue","red") )

diff = sum(((rbf$density-wrf$density))**2)*1e6
rbmf = mean(head(rbstats[order(rbstats$FPfull, decreasing=TRUE),]$FPfull,nstat))
wrmf = mean(head(wrstats[order(wrstats$FPfull, decreasing=TRUE),]$FPfull,nstat))

text(txt_sym, basey, expression(paste(sigma^2, " = ")))
text(txt_num, basey - 0.0001, sprintf("%4.1f", diff) )
text(txt_sym, basey - 0.002, sprintf("RB mean = %5.1f", rbmf) )
text(txt_sym, basey - 0.004, sprintf("WR mean = %5.1f", wrmf) )

png(paste(year,"/halfppr.png", sep=""))
rbh <- hist(head(rbstats[order(rbstats$FPhalf, decreasing=TRUE),]$FPhalf,nstat), breaks=seq(minval,maxval,by=30), xlim=c(50,500), ylim=c(0,0.02), freq=FALSE,  col=rgb(0.1,0.1,1.0,0.5), main=paste("0.5 PPR - ", year), xlab="Points")
wrh <- hist(head(wrstats[order(wrstats$FPhalf, decreasing=TRUE),]$FPhalf,nstat), breaks=seq(minval,maxval,by=30), xlim=c(50,500), freq=FALSE,  col=rgb(1.0,0.1,0.1,0.5), add=TRUE)
temp <- legend(legx,legy,c("RB", "WR"), lty=c(1,1), lwd=c(10,10), col=c("blue","red") )

difh = sum(((rbh$density-wrh$density))**2)*1e6
rbmh = mean(head(rbstats[order(rbstats$FPhalf, decreasing=TRUE),]$FPhalf,nstat))
wrmh = mean(head(wrstats[order(wrstats$FPhalf, decreasing=TRUE),]$FPhalf,nstat))

text(txt_sym, basey, expression(paste(sigma^2, " = ")))
text(txt_num, basey - 0.0001, sprintf("%4.1f", difh) )
text(txt_sym, basey - 0.002, sprintf("RB mean = %5.1f", rbmh) )
text(txt_sym, basey - 0.004, sprintf("WR mean = %5.1f", wrmh) )

png(paste(year,"/qtrppr.png", sep=""))
rbq <- hist(head(rbstats[order(rbstats$FPqtr, decreasing=TRUE),]$FPqtr,nstat), breaks=seq(minval,maxval,by=30), xlim=c(50,500), ylim=c(0,0.02), freq=FALSE,  col=rgb(0.1,0.1,1.0,0.5), main=paste("0.25 PPR -", year), xlab="Points")
wrq <- hist(head(wrstats[order(wrstats$FPqtr, decreasing=TRUE),]$FPqtr,nstat), breaks=seq(minval,maxval,by=30), xlim=c(50,500), freq=FALSE,  col=rgb(1.0,0.1,0.1,0.5), add=TRUE)
temp <- legend(legx,legy,c("RB", "WR"), lty=c(1,1), lwd=c(10,10), col=c("blue","red") )

difq = sum(((rbq$density-wrq$density))**2)*1e6
rbmq = mean(head(rbstats[order(rbstats$FPqtr, decreasing=TRUE),]$FPqtr,nstat))
wrmq = mean(head(wrstats[order(wrstats$FPqtr, decreasing=TRUE),]$FPqtr,nstat))

text(txt_sym, basey, expression(paste(sigma^2, " = ")))
text(txt_num, basey - 0.0001, sprintf("%4.1f", difq) )
text(txt_sym, basey - 0.002, sprintf("RB mean = %5.1f", rbmq) )
text(txt_sym, basey - 0.004, sprintf("WR mean = %5.1f", wrmq) )

png(paste(year,"/noppr.png", sep=""))
rbn <- hist(head(rbstats[order(rbstats$FPnone, decreasing=TRUE),]$FPnone,nstat), breaks=seq(minval,maxval,by=30), xlim=c(50,500), ylim=c(0,0.02), freq=FALSE,  col=rgb(0.1,0.1,1.0,0.5), main=paste("No PPR - ", year), xlab="Points")
wrn <- hist(head(wrstats[order(wrstats$FPnone, decreasing=TRUE),]$FPnone,nstat), breaks=seq(minval,maxval,by=30), xlim=c(50,500), freq=FALSE,  col=rgb(1.0,0.1,0.1,0.5), add=TRUE)
temp <- legend(legx,legy,c("RB", "WR"), lty=c(1,1), lwd=c(10,10), col=c("blue","red") )

difn = sum(((rbn$density-wrn$density))**2)*1e6
rbmn = mean(head(rbstats[order(rbstats$FPnone, decreasing=TRUE),]$FPnone,nstat))
wrmn = mean(head(wrstats[order(wrstats$FPnone, decreasing=TRUE),]$FPnone,nstat))

text(txt_sym, basey, expression(paste(sigma^2, " = ")))
text(txt_num, basey - 0.0001, sprintf("%4.1f", difn) )
text(txt_sym, basey - 0.002, sprintf("RB mean = %5.1f", rbmn) )
text(txt_sym, basey - 0.004, sprintf("WR mean = %5.1f", wrmn) )

head(rbstats[order(rbstats$FPqtr, decreasing=TRUE),c("Name", "FPqtr")],12)
head(wrstats[order(wrstats$FPqtr, decreasing=TRUE),c("Name", "FPqtr")],12)



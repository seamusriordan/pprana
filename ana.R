year = c("2010", "2011", "2012", "2013", "2014")

pprval = c(1.0, 0.5, 0.25, 0.0)
pprsuf = c("full", "half", "qtr", "no")

i = 1

while( i <= length(year) ){
    j = 1

    filename = paste("years_", year[i], "_fantasy_fantasy.csv", sep="")

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

    while( j <= length(pprval) ){

        stats$FP <- stats$Rec*pprval[j] + stats$PassYds*0.04 + stats$PassTD*4 + stats$Int*-2 + stats$RushYds*0.1 + stats$RushTD*6 + stats$RecYds*0.1 + stats$RecTD*6

        rbstats <- stats[stats$Pos=="RB",]
        wrstats <- stats[stats$Pos=="WR",]

        png(paste(year[i],"/", pprsuf[j], "ppr.png", sep=""))
        rbf <- hist(head(rbstats[order(rbstats$FP, decreasing=TRUE),]$FP,nstat), breaks=seq(minval,maxval,by=30), xlim=c(50,500), ylim=c(0,0.02), freq=FALSE,  col=rgb(0.1,0.1,1.0,0.5), main=paste(pprval[j], " PPR - ", year[i]), xlab="Points")
        wrf <- hist(head(wrstats[order(wrstats$FP, decreasing=TRUE),]$FP,nstat), breaks=seq(minval,maxval,by=30), xlim=c(50,500), freq=FALSE,  col=rgb(1.0,0.1,0.1,0.5), add=TRUE)
        temp <- legend(legx,legy,c("RB", "WR"), lty=c(1,1), lwd=c(10,10), col=c("blue","red") )

        diff = sum(((rbf$density-wrf$density))**2)*1e6
        rbmf = mean(head(rbstats[order(rbstats$FP, decreasing=TRUE),]$FP,nstat))
        wrmf = mean(head(wrstats[order(wrstats$FP, decreasing=TRUE),]$FP,nstat))

        text(txt_sym, basey, expression(paste(sigma^2, " = ")))
        text(txt_num, basey - 0.0001, sprintf("%4.1f", diff) )
        text(txt_sym, basey - 0.002, sprintf("RB mean = %5.1f", rbmf) )
        text(txt_sym, basey - 0.004, sprintf("WR mean = %5.1f", wrmf) )


        j = j+1
    }

    i = i + 1
}

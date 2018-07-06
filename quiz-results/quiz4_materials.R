## latex format functions


beginDoc <- function(){
  return(c(
    '\\documentclass{article}',
    '\\usepackage{booktabs}',
    '\\usepackage{graphicx}',
    '\\usepackage[margin=1in]{geometry}',
    '\\begin{document}'))
}
endDoc <- function(){return('\\end{document}')}
beginTable <- function(aligns){return(paste0('\\begin{tabular}{',aligns,'}'))}
endTable <- function(){return('\\end{tabular}')}
toprule <- function(){return('\\toprule')}
bottomrule <- function(){return('\\bottomrule')}
midrule <- function(){return('\\midrule')}
partmidrule <- function(start,stop){return(sprintf('\\cline{%s-%s}',start,stop))}
titlerow <- function(panel, title, num){ return( paste0("\\textbf{",panel,'}',' & ',sprintf("\\multicolumn{%d}{c}{\\textbf{", num),title,'}} \\\\')) }
namerow <- function(name, num){paste(name, paste(paste(rep(' & ', num),collapse=''),'\\\\'),collapse='')}
formatNum <- function(x, dec=4, big.mark=",", stat=F, se=F, pvec=-1){
  if(is.numeric(x)){x=round(x,dec)}
  y = trimws(format(x,big.mark=',',nsmall=dec,digits=dec,scientific=F))
  if(sum(stat)!=0 & sum(stat)==1 & length(stat)==1){y = paste0(y,'\\%')}
  if(sum(se)!=0 & sum(se)==1 & length(se)==1){y = paste0('(',y,')')}
  if(sum(stat)!=0 & sum(stat)>=1 & length(stat)==length(x) & length(stat)>1){for(i in 1:length(x)){ if(stat[i]==1){y[i] = paste0(y[i],'\\%') }}}
  if(sum(se)!=0 & sum(se)>=1 & length(se)==length(x) & length(se)>1){for(i in 1:length(x)){ if(se[i]==1){y[i] = paste0('(',y[i],')') }}}
  if(length(pvec)==length(x) & max(pvec)>=0 & min(pvec)<=1){for(i in 1:length(x)){ if(!is.na(pvec[i])){ if(pvec[i]<=.01){y[i] = paste0(y[i],'*') }; if(pvec[i]<=.05){y[i] = paste0(y[i],'*') }; if(pvec[i]<=.1){y[i] = paste0(y[i],'*') }}  }}
  y[grep('NA',y)] = rep('',2)
  return(y)
}
numrow <- function(title, array, dec=2, stat=F, se=F, pvec=-1, spacer='\\\\'){
  x = formatNum(array, dec=dec, stat=stat, se=se, pvec=pvec)
  y = paste(paste0(' & ',x), collapse='')
  return(paste0(title,y,spacer))
}

## example of what the functions can be used to do

library(data.table)
library(stringr)

dd = data.table(group=c('Full Sample','Subsample'),coef=c(1.2,3.42),se=c(.6,.481),p=c(.051,.0),N=c(1234567,891011))

colspec='lcc'
numcol = nchar(colspec)

outtab = c(
  beginTable(colspec),
  toprule(),
  midrule(),
  titlerow(' ', 'Results at the Worker-level', numcol-1),
  numrow(' ',dd$group),
  partmidrule(2,numcol),
  numrow('Effect of X on Y',dd$coef,pvec=dd$p,dec=3),
  numrow(' ',dd$se,dec=3,se=T),
  midrule(),
  numrow('Sample Size (1,000)',dd$N/1000,dec=0),
  midrule(),
  bottomrule(),
  endTable()
)

## only do this if you want to be able to compile the table as a stand-alone document
outtab = c(beginDoc(),outtab,endDoc())

## export the table as .tex
openfile = file("quiz4_example.tex")
writeLines(outtab,openfile)
close(openfile)


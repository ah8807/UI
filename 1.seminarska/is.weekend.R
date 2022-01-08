is.weekend<-function(atribute){
  library(lubridate)
  tmp<-atribute
  tmp[wday(atribute)==7|wday(atribute)==6]<-TRUE
  tmp[wday(atribute)!=7&wday(atribute)!=6]<-FALSE
  return(tmp)
}

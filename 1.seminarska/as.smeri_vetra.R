as.smeri_vetra<-function(atribut){
  tmp<-atribut
  tmp[atribut==0]<-"brezveterje"
  tmp[atribut>0&atribut<=22.5]<-"sever"
  tmp[atribut>22.5&atribut<=62.5]<-"severo_vzhod"
  tmp[atribut>62.5&atribut<=112.5]<-"vzhod"
  tmp[atribut>112.5&atribut<=157.5]<-"jugo_vzhod"
  tmp[atribut>157.5&atribut<=202.5]<-"jug"
  tmp[atribut>202.5&atribut<=247.5]<-"jugo_zahod"
  tmp[atribut>247.5&atribut<=292.5]<-"zahod"
  tmp[atribut>292.5&atribut<=337.5]<-"severo_zahod"
  tmp[atribut>337.5&atribut<=360]<-"sever"
  atribut<-tmp
  atribut<-as.factor(atribut)
  return(as.factor(atribut))
}


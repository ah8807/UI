dosedanja.povprecna.poraba<-function(ucna){
  test<-ucna
  for(var in 1:nrow(ucna))
  {
    test$dosedanja_povprecna[var]<-mean(ucna$poraba[test$datum[var]>=ucna$datum & test$stavba[var]==ucna$stavba])
  }
  return(test$dosedanja_povprecna)
}
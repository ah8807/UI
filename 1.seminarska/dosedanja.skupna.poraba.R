dosedanja.skupna.poraba<-function(ucna){
  test<-ucna
  for(var in 1:nrow(ucna))
  {
    test$dosedanja_povprecna[var]<-sum(ucna$poraba[test$datum[var]>=ucna$datum & test$stavba[var]==ucna$stavba])
  }
  return(test$dosedanja_povprecna)
}

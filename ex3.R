(w_1,...,w_k)~Dir(2/k, ..., 2/k)
E(K*)=sum(i in 1:n)(alpha/(alpha+i-1))
E(K*)=~ alpha*log((alpha+n-1)/alpha)

Q1
n1=400
alpha=2
sum=0
for(i in 1:n1){
  sum= sum+ alpha/(alpha+i-1)
}
sum

Q2
n2=100
alpha=2
sum2=0
for(i in 1:n2){
  sum2= sum2+ alpha/(alpha+i-1)
}
sum2

Q3
n3=100
alpha=2
#E(K*)=~ alpha*log((alpha+n-1)/alpha)
E_K= alpha*log((alpha+n3-1)/alpha)
E_K

Q4
n4=200
E_k=2
E_K= alpha*log((alpha+n-1)/alpha)
2=alpha*log(alpha + 200-1)/alpha)

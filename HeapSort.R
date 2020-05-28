### 20/05/27 Keonwoo Park

## HeapSort

setwd("C:/users/거누/Desktop/Rstat/데이터 구조론/DataRst-03.알고리즘")



# Heap Sort

Heap_Sort <- function(d, decreasing=FALSE){
  if(decreasing==FALSE){
    Heap_Sort_I(d)
  }else{
      Heap_Sort_D(d)
  }
}



# 오름차순
Heap_Sort_I <- function(d){
  
  # 트리구조를 힙구조로 바꿉니다.
  n = length(d)
  for(i in 2:n){
    c = i
    # 힙 구조에서 부모의 위치
    index = c/2
    # 부모를 가장 큰 값으로 둔다.
    repeat{
      if(d[index]<d[c]){
        temp = d[index]
        d[index]=d[c]
        d[c]=temp
        
        c = index
        index = c/2
        if(index<1)break # 범위 밖의 인덱스를 얻는 경우 멈춘다.
      }else break
    }
  }
 #처음 값을 마지막 값과 바꾼 후 그 값을 빼고 다시 힙정렬.
 for(i in n:2){
   temp = d[1]
   d[1] = d[i]
   d[i] = temp
   # print(c(1, i,d))
   # 부모 위치 index
   index = 1
   c = 2
   repeat{
     c = index * 2
     #자식 두 개를 비교 한다.
     if(d[c]<d[c+1]& c+1< i){
       c = c+1
     }
     # 부모와 자식 값 비교한다.
     if(d[index]<d[c]& c<i){
       temp = d[index]
       d[index] = d[c]
       d[c] = temp
       index = c
       # print(c(1, i,d))
      }else break
    }
 }
  return(d)
}
# 내림차순
Heap_Sort_D <- function(d){
  
  n = length(d)
  for(i in 2:n){
    c = i
    # 힙 구조에서 부모의 위치
    index = c/2
    # 부모를 가장 큰 값으로 둔다.
    repeat{
      if(d[index]>d[c]){
        temp = d[index]
        d[index]=d[c]
        d[c]=temp
        
        c = index
        index = c/2
        if(index<1)break #부모값 보다 작아지는 경우 바로 끊는다.
      }else break
    }
  }
  #처음 값을 마지막 값과 바꾼 후 그 값을 빼고 다시 힙정렬.
  for(i in n:2){
    temp = d[1]
    d[1] = d[i]
    d[i] = temp
    # print(c(1, i,d))
    # 부모 위치 index
    index = 1
    c = 2
    repeat{
      c = index * 2
      #자식 두 개를 비교 한다.
      if(d[c]>d[c+1]& c+1< i){
        c = c+1
      }
      # 부모와 자식 값 비교한다.
      if(d[index]>d[c]& c<i){
        temp = d[index]
        d[index] = d[c]
        d[c] = temp
        index = c
        # print(c(1, i,d))
      }else break
    }
  }
  return(d)
}
a=sample(1000)
Heap_Sort(a)

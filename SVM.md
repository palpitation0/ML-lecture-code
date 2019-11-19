Suppor Vector Machine
================

``` r
library(e1071)
data(iris)
```

``` r
names(iris) = c("SL","SW","PL","PW","SP")
levels(iris$SP) = c("st","vc","vg")
```

``` r
iris[c(1,41,101),]
```

    ##      SL  SW  PL  PW SP
    ## 1   5.1 3.5 1.4 0.2 st
    ## 41  5.0 3.5 1.3 0.3 st
    ## 101 6.3 3.3 6.0 2.5 vg

``` r
tune(svm, SP~SW+SL, data=iris,
     ranges = list(gamma=2^(-1:1), cost=2^(2:4)),
     tunecontrol = tune.control(sampling="fix"))
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: fixed training/validation set 
    ## 
    ## - best parameters:
    ##  gamma cost
    ##    0.5    4
    ## 
    ## - best performance: 0.14

tune : 최적 파라미터를 구해줘

``` r
iris.svm <- svm(SP~SW+SL, data=iris, cost=4, gamma=0.5)
table(iris$SP, predict(iris.svm))
```

    ##     
    ##      st vc vg
    ##   st 50  0  0
    ##   vc  0 31 19
    ##   vg  0  9 41

``` r
tune(svm, SP~., data=iris,
     ranges = list(gamma=2^(-4:4), cost=2^(3:7)),
     tunecontrol = tune.control(sampling="fix"))
```

    ## 
    ## Parameter tuning of 'svm':
    ## 
    ## - sampling method: fixed training/validation set 
    ## 
    ## - best parameters:
    ##  gamma cost
    ##   0.25    8
    ## 
    ## - best performance: 0.04

``` r
iris.svmall <- svm(SP~., data=iris, cost=32, gamma=0.125)
table(iris$SP, predict(iris.svmall))
```

    ##     
    ##      st vc vg
    ##   st 50  0  0
    ##   vc  0 47  3
    ##   vg  0  0 50

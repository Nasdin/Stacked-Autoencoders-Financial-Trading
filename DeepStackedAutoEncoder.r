
#=======Function===============================================
	pr.OHLC<-function(o,  h,  l,  cl){
		# combine quotes matrix. Quotes pre-reversing
		price <- cbind(Open = rev(o), High = rev(h), Low = rev(l), Close = rev(cl));
		Med <-(price[ ,2] + price[ ,3])/2;
		CO <-price[ ,4] - price[ ,1];
		# add the matrix Med and CO
		price<- cbind(price, Med, CO);
	}
#--2----------------------
	In <- function(p = 16){
	  require(TTR)
	  require(dplyr)
	  require(magrittr)
	  adx <- ADX(price, n = p) %>% as.data.frame %>% 
		mutate(.,oscDX = DIp - DIn) %>% 
		transmute(.,DX, ADX, oscDX) %>% 
		as.matrix()
	  ar <- aroon(price[ ,c('High', 'Low')], n = p) %>% 
		extract(,3)
	  atr <- ATR(price, n = p, maType = "EMA") %>%
		extract(,1:2)
	  cci <- CCI(price[ ,2:4], n = p)
	  chv <- chaikinVolatility(price[ ,2:4], n = p)
	  cmo <- CMO(price[ ,'Med'], n = p)
	  macd <- MACD(price[ ,'Med'], 12, 26, 9) %>% 
		as.data.frame() %>% 
		mutate(., vsig = signal %>% 
				 diff %>% c(NA,.) %>% multiply_by(10)) %>% 
		transmute(., sign = signal, vsig) %>% 
		as.matrix()
	  rsi <- RSI(price[ ,'Med'], n = p)
	  stoh <- stoch(price[ ,2:4], nFastK = p, 
					nFastD =3, nSlowD = 3, 
					maType = "EMA") %>%
					as.data.frame() %>% 
					mutate(., oscK = fastK - fastD) %>%
					transmute(.,slowD, oscK) %>% 
					as.matrix()
	  smi <- SMI(price[ ,2:4],n = p, nFast = 2, 
				 nSlow = 25, nSig = 9)
	  kst <- KST(price[ ,4])%>% as.data.frame() %>% 
					mutate(., oscKST = kst - signal) %>%
					select(.,oscKST) %>% as.matrix()
	  In <- cbind(adx, ar, atr, cci, chv, cmo, macd, 
				  rsi, stoh, smi, kst)
	  return(In)
	}
	#----3-------------------------------------
	ZZ <- function(pr = price, ch = ch , mode="m") {
	  require(TTR)
	  require(magrittr)
	  if (ch > 1) ch <- ch/(10 ^ (Dig - 1))
	  if (mode == "m") {pr <- pr[ ,'Med']}
	  if (mode == "hl") {pr <- pr[ ,c("High", "Low")]}
	  if (mode == "cl") {pr <- pr[ ,c("Close")]}
	  zz <- ZigZag(pr, change = ch, percent = F, 
				   retrace = F, lastExtreme = T)
	  n <- 1:length(zz)
	  dz <- zz %>% diff %>% c(., NA)
	  sig <- sign(dz)
	  for (i in n) { if (is.na(zz[i])) zz[i] = zz[i - 1]}
	  return(cbind(zz, sig))
	}
	#-----4---------------------------------
	form.data <- function(n = 16, z = 37, len = 500){
	  require(magrittr)
	  x <- In(p = n)
	  out <- ZZ(ch = z, mode = "m")
	  data <- cbind(x, y = out[ ,2]) %>% 
		as.data.frame %>% head(., (nrow(x)-len))%>%
		na.omit
	  data$y <- as.factor(data$y)
	  return(data)
	}
	#---5---Cleaning------------------------------------------
	cleaning <- function(n = 16, z = 25, cut = 0.9){
	  data <- form.data(n, z)
	  descCor <- cor(data[ ,-ncol(data)])
	  #summary(descCor[upper.tri(descCor)])
	  highCor <- caret::findCorrelation(descCor, cutoff = cut)
	  data <- data[ ,-highCor]
	  #descCor <- cor(data.f[ ,-ncol(data.f)])
	  #summary(descCor[upper.tri(descCor)])
	  return(data)
	}
	#----6---Choosing the Best----------------------------
	prepareBest <- function(n, z, cut, norm, meth)
	{
	  require(randomUniformForest) 
	  data.f <<- cleaning(n = n, z = z, cut = cut)
	  idx <- rminer::holdout(y = data.f$y)
	  x.train <- data.f[idx$tr, -ncol(data.f)]
	  x.test <-  data.f[idx$ts, -ncol(data.f)]
	  y.train <- data.f[idx$tr, ncol(data.f)]
	  y.test <- data.f[idx$ts, ncol(data.f)]
	  if(norm){
		prep <- caret::preProcess(x = x.train, method = meth)
		#c("center", "scale","spatialSign"), "range"
		x.train <- predict(prep, x.train)
		x.test <- predict(prep, x.test)
	  }
	  #---------
	  ruf <- randomUniformForest( X = x.train, 
								  Y = y.train,
								  xtest = x.test, 
								  ytest = y.test,
								  mtry = 1, ntree = 300,
								  threads = 2, 
								  nodesize = 1
	  )
	  imp.ruf <- importance(ruf, Xtest = x.test)
	  best <- imp.ruf$localVariableImportance$classVariableImportance %>% 
		head(., 10) %>% rownames()
	  return(best)
	}
	#-------7---------------
	prepareTrain <- function(x , y, 
							 rati, mod = "stratified", 
							 balance = F, 
							 norm, meth)
	{
	  t <- rminer::holdout(y = y, ratio = rati,
						   mode = mod)
	  train <- cbind(x[t$tr, ], y = y[t$tr])
	  if(balance){
		train <- caret::upSample(x = train[ ,best], 
								 y = train$y, 
								 list = F)%>% tbl_df
		train <- cbind(train[ ,best], select(train, y = Class))
	  }
	  test <- cbind(x[t$ts, ], y = y[t$ts])
	  if (norm) {
		prepr <<- caret::preProcess(train[ ,best], method = meth)
		train = predict(prepr, train[ ,best])%>% cbind(., y = train$y)
		test =  predict(prepr, test[ ,best] %>% cbind(., y = test$y))
	  }
	  DT <- list(train = train,
				 test = test)
	  return(DT)
	}
	#---train the stacked auto encoder----------------------------------------------
	train_SAE <- function(dt,  
		h = c(50, 50, 50), LR = 0.7, Mom = 0.5,
		act = "tanh", out = "sigm", sae = "linear", 
		Ep = 100, Bs = 50, hd.out = 0.0, in.out = 0.0){
		require(deepnet)
		x <- dt$train[ ,best]%>% as.matrix
		if (soft) { y <- dt$train$y %>% classvec2classmat()}
		if (!soft){ y <- dt$train$y %>% as.integer() %>% subtract(1)}
		sae.dnn.train(x = x , y = y , hidden = h,
			activationfun = act, output = out, sae_output = sae,
			learningrate = LR, learningrate_scale = 1,
			momentum = Mom,
			numepochs = Ep, batchsize = Bs,
			hidden_dropout = hd.out, visible_dropout = in.out)
	}
	#----10-------------------------------------------
	prepareTest <- function(n, z, norm, len = 501)
	{
	  x <- In(p = n ) %>% na.omit %>% extract( ,best) %>% 
		tail(., len)
	  CO <- price[ ,"CO"] %>% tail(., len)
	  if (norm) {
		x <- predict(prepr,x)
	  }
	  dt <- cbind(x = x, CO = CO) %>% as.data.frame()
	  return(dt)
	}
	#---11-----
	testAcc <- function(obj){
	   require(deepnet)
	  x.ts <- DT$test[ ,best] %>% as.matrix()
	  y.ts <- DT$test$y %>% as.integer() %>% subtract(1)
	  out <- nn.predict(obj, x.ts) 
	  if (soft){out <- max.col(out)-1} else {out %<>% as.vector()}               
	  acc <- length(y.ts[y.ts == out])/length(y.ts) %>% 
		round(., digits = 4)
	  return(list(Acc = acc, y.ts = y.ts, y = out))
	}
	#---12-----
	testBal <- function(obj, typ = "bin") {
	  x <- DT.test[ ,best]
	  CO <- DT.test$CO
	  out <- nn.predict(obj, x) 
	  if(soft){out <- max.col(out)-1} else {out %<>% as.vector()} 
	  sig <- ifelse(out == 0, -1, 1)
	  sig1 <- Hmisc::Lag(sig) %>% na.omit
	  bal <- cumsum(sig1 * tail(CO, length(sig1)))
	  K <- tail(bal, 1)/length(bal) * 10 ^ Dig
	  Kmax <- max(bal)/which.max(bal) * 10 ^ Dig
	  dd <- fTrading::maxDrawDown(bal)
	  return(list(sig = sig, bal = bal, K = K, 
				  Kmax = Kmax, dd = dd))
	}
	#---13----------------------------------
	sig2stat <- function(x) {x %>% as.factor %>% as.numeric}
	stat2sig <- function(x) ifelse(x==1, -1, 1)
	#----14--correct-----------------------------------
	correct <- function(sig){
	  sig <- Hmisc::Lag(sig) %>% na.omit
	  bal <- cumsum(sig * (price[ ,6] %>% tail(.,length(sig))))
	  ma <- pracma::movavg(bal, 16, "t")
	  roc <- TTR::momentum(ma, 3)%>% na.omit
	  dbal <- (bal - ma) %>% tail(., length(roc))
	  dbr <- (roc + dbal) %>% as.matrix()
	  sig.cor <- sign(dbr)
	  S <- sig.cor * tail(sig, length(sig.cor))
	  bal <- cumsum(S * (price[ ,6]%>% tail(.,length(S))))
	  K <- tail(bal, 1)/length(bal) * 10 ^ Dig
	  Kmax <- max(bal)/which.max(bal) * 10 ^ Dig
	  dd <- fTrading::maxDrawDown(bal)
	  corr <<- list(sig.c = sig.cor, sig.res = S, bal = bal, Kmax = Kmax, K = K, dd = dd)
	  return(corr)
	}

	#---15---markovchain----------------------------------
	pred.sig <- function(sig, prev.bar = 10, nahead = 1){
	  require(markovchain)
        stat <- sig2stat(sig)

        #Basically I copied this part and idea from the internet somewhere
	if(!exists('MCbsp')){
	  MCbsp <<- markovchainFit(data = stat, 
							method = "bootstrap",
							nboot = 10L,
							name="Bootstrap MС")
	}
	  newData <- tail(stat, prev.bar)
	  pr <- predict(object = MCbsp$estimate, 
					newdata = newData,
					n.ahead = nahead)
	  sig.pr <- c(sig, stat2sig(pr))
	  return(sig.pr = sig.pr)
	}
	#---16---smooth------------------------------------
	smoooth <- function(sig){
	  require(mhsmm)
	if(!exists('obj.sm')){
	  obj.sm <<- sig2stat(sig)%>% smooth.discrete()
	 }
	
	  sig.s <- predict(obj.sm, x = sig2stat(sig))%>% 
				extract2(1)%>% stat2sig()
	  sig.s1 <- Hmisc::Lag(sig.s) %>% na.omit
	  bal <- cumsum(sig.s1 * (price[ ,6]%>% tail(.,length(sig.s1))))
	  K <- tail(bal, 1)/length(bal) * 10 ^ Dig
	  Kmax <- max(bal)/which.max(bal) * 10 ^ Dig
	  dd <- fTrading::maxDrawDown(bal)
	  return(list(sig = sig.s, bal = bal, Kmax = Kmax, K = K, dd = dd))
	}
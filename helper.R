reduction = function(myjpg,grid=6,interval=c(1,100)){
  try({nirvana = as.array(myjpg)})
  
  red = nirvana[,,1,1]
  blue = nirvana[,,1,2]
  green = nirvana[,,1,3]
  
  reds = svd(red)
  redd = diag(reds$d)
  redu = reds$u
  redv = reds$v
  #red = u d v' 로 singural value decomposition
  
  blues = svd(blue)
  blued = diag(blues$d)
  blueu = blues$u
  bluev = blues$v
  
  greens = svd(green)
  greend = diag(greens$d)
  greenu = greens$u
  greenv = greens$v
  
  
  reduction1 = function(i){
    somedata=rep(1,3*dim(red)[1]*dim(red)[2])
    result = array(somedata,dim=c(dim(red)[1],dim(red)[2],1,3))
    r = trunc(min(dim(red)[1],dim(red)[2])*i)
    result[,,1,1] = redu[,1:r] %*% redd[1:r,1:r] %*% t(redv[,1:r])
    result[,,1,2] = blueu[,1:r] %*% blued[1:r,1:r] %*% t(bluev[,1:r])
    result[,,1,3] = greenu[,1:r] %*% greend[1:r,1:r] %*% t(greenv[,1:r])
    result[result<0]=0
    result[result>1]=1
    return(result)
  }
  par(mfrow=c(grid/3,3),xaxt="n",yaxt="n")
  blurindex = c(1,
                seq(interval[2],interval[1],length.out=grid)[-1]/100)
  for (i in blurindex){
      plot(as.cimg(reduction1(i)),main=paste(i*100,"%   use"))
  }
}


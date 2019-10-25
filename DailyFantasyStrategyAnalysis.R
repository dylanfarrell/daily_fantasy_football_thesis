getwd()
setwd("/Users/dylanfarrell/Desktop/Thesis/")
library(logspline)
library(ggplot2)
library(lpSolveAPI)
## FUNCTION ARGUMENTS:
##
## train: 'players list' in .csv format with the following 8 columns (in this order): 
## Id (A unique numeric identifier for each player)
## Position (One of: WR, TE, RB, QB, K, D)
## First Name
## Last Name
## Expected Points
## Salary
## Team (player's team or defensive team)
## Opponent (opposing team)
## See here for the example formatting to use: https://github.com/geekman1/fantasylineup/fantasy_points.csv
## Can change 'Expected Points' to your expected points for that player and remove all players you don't want
## in the lineup (injuries, etc.)
##
## cap: Max salary cap (for FanDuel it is currently 60,000, for DraftKings it is 50,000)
##
## constraint: 'none' for no constraints on lineup configuration
##
## 'all_diff' ensures no two players are from the same team AND
## offensive players do not play against the chosen defense
##
## 'no_opp' ensures at most two players from the same team (include defense) 
## AND offensive players do not face the chosen defense AND offensive 
## players don't face eachother (for example, if NE is playing NYG, you can't 
## have Odell Beckham and Tom Brady)
##
## league: Which fantasy site you are using - FanDuel or DraftKings. Each site has a 
## slightly different lineup structure of 9 players. FanDuel currently has 
## the lineup structure (D, K, TE, RB, RB, QB, WR, WR, WR) and DraftKings follows 
## (D, QB, RB, RB, WR, WR, WR, TE, FLEX) where FLEX is a single additional TE/RB/WR.
##
## setplayers: Allows you to force lineups to be chosen with these players included. Simply 
## include a .csv in the same format as 'train', with your selected players chosen.
##
## removeteams: Allows you to remove certain teams from being considered. Slightly different 
## than simply removing players from the 'train' file in that this only removes specific 
## teams. It is used in the 'top_teams' function. Should be a matrix, with each column 
## a binary vector indicating the selected players for a single team; each column is a 
## different team; find_teams' then removes these teams.
##
## Function returns the top lineup under your constraints, with each team's
## expected total points and the total salary used (will be under the cap)


l = function(train,cap,constraint,league,setplayers=NULL,removeteams=NULL){
  defense <- ifelse(train$Pos == "D", 1, 0)
  qb <- ifelse(train$Pos == "QB", 1, 0)
  wr <- ifelse(train$Pos == "WR", 1, 0)
  te <- ifelse(train$Pos == "TE", 1, 0)
  rb <- ifelse(train$Pos == "RB", 1, 0)
  k <- ifelse(train$Pos == "K", 1, 0)
  
  ## number of decision variables is equal to the number of fantasy players/teams
  lpfantasy <- make.lp(0, nrow(train))
  
  ## Set objective function with the expected number of points
  set.objfn(lpfantasy, train$Value)
  
  ## Make sure the decision variables are binary
  set.type(lpfantasy, seq(1, nrow(train), by=1), type = c("binary"))
  
  
  add.constraint(lpfantasy, qb, "=", 1)
  add.constraint(lpfantasy, wr, "=", 3)
  add.constraint(lpfantasy, rb, "=", 2)
  
  
  ## DEPRECATED, but can uncomment if you want to impose the restrictions described on the next two lines
  ## Make sure not to select more than one WR, or more than one RB from a single team
  ## Constraint: make sure not to select a WR/TE, WR/RB, RB/TE combo from same team
  #for(i in 1:length(team_names)){
  #  position_check <- ifelse((train$Team == team_names[i] & 
  #                             (train$Position == "WR" | train$Position == "RB" | train$Position == "TE")), 1, 0)
  #  add.constraint(lpfantasy, position_check, "<=", 1)
  #}
  
  ## Add monetary constraint, max salary for the team
  add.constraint(lpfantasy, train$FD.salary, "<=", cap)
  
  ## Set objective direction
  lp.control(lpfantasy, sense='max')
  
  team_names <- levels(factor(train$Team))
  #constraint <- match.arg(constraint)
  if(constraint == "all_diff") {
    for(i in 1:length(team_names)) {
      ## label opponent of each player (what defense they are playing against)
      check <- ifelse(train$Oppt == team_names[i], 1, 0)
      ## label only that defense with a 1
      check <- ifelse(train$Pos == "D", 0, check) 
      check <- ifelse((train$Team == team_names[i] & train$Pos == "D"), 1, check)
      ## add the set of constraints
      add.constraint(lpfantasy, check, "<=", 1)
    }
  }
  
  if(constraint == "no_opp") {
    team_names <- levels(factor(train$Team))  
    for(i in 1:length(team_names)) {
      ## No more than two players from each team (including that team's defense)
      no_two <- ifelse(train$Team == team_names[i], 1, 0)
      add.constraint(lpfantasy, no_two, "<=", 2)
    }
    for(j in 1:nrow(train)) {
      no_opposing <- ifelse(train$Oppt == train$Team[j], 1, 0)
      no_opposing[j] <- 1
      ## To deal with defenses (since Team and Opponent are swtiched for defenses)
      no_opposing <- ifelse(train$Pos == "D", 0, no_opposing) 
      no_opposing <- ifelse((train$Team == train$Oppt[j] & train$Pos == "D"), 1, no_opposing)
      for(k in 1:nrow(train)) {
        out <- rep(0, nrow(train))
        out[j] <- 1
        out[k] <- no_opposing[k]
        add.constraint(lpfantasy, out, "<=", 1)
      }
    }
  }
  
  if(!is.null(setplayers)) {
    if(league == "MyLeague") {
      if((sum(setplayers$Position == "WR") > 3) || (sum(setplayers$Position == "RB") > 2) || (sum(setplayers$Position == "QB") > 1))
        stop("One of your positions has too many players")
    }
    ## Set constraints that each player here must be in lineup
    for(k in 1:nrow(setplayers)) {
      add.constraint(lpfantasy, ifelse(setplayers$GID[k] == train$GID, 1, 0), "=", 1)
    }
  }
  
  if(!is.null(removeteams)) {
    if(nrow(removeteams) != nrow(train))
      stop("Your team restrictions do not match the number of players included in the 'train' file")
    for(m in 1:ncol(removeteams)) {
      add.constraint(lpfantasy, removeteams[, m], "<=", 5)
    }
  }
  
  team <- data.frame(matrix(0, 1, ncol(train) + 3))
  colnames(team) <- c(colnames(train), "TeamSalary", "TotalPoints",'ActualPoints')
  
  ## Solve the model, if this returns 0 an optimal solution is found
  solve(lpfantasy)
  if(solve(lpfantasy) != 0)
    stop("Optimization failed at some step")
  
  ## Get the players on the team
  team_select <- subset(data.frame(train, get.variables(lpfantasy)), get.variables.lpfantasy. == 1)
  team_select$get.variables.lpfantasy. <- NULL
  team_select$TeamSalary <- sum(team_select$FD.salary)
  team_select$TotalPoints <- sum(team_select$Value)
  team_select$ActualPoints <- sum(team_select$Actual)
  team <- rbind(team, team_select)
  team <- team[-1,]
  rownames(team) <- NULL
  team
}


top_teams <- function(train, num_top, cap, constraint, league, setplayers=NULL) {
  result <- l(train, cap, constraint = constraint, league = league, setplayers = setplayers)
  pts = result$ActualPoints[1]
  restrict <- as.matrix(rep(0, nrow(train)))
  restrict[match(result$GID, train$GID), 1] <- 1
  j <- 1
  
  while(j < num_top) {
    resultnew <- l(train, cap, constraint = constraint, league = league, setplayers = setplayers, removeteams = restrict)
    restrict <- cbind(restrict, rep(0, nrow(restrict)))
    restrict[match(resultnew$GID, train$GID), j] <- 1
    result <- rbind(result, resultnew)
    pts = c(pts,resultnew$ActualPoints[1])
    j <- j + 1
  }
  
  TeamNumber <- rep(1:num_top, each = 6)
  result <- cbind.data.frame(result, TeamNumber)
  return(pts)
}

##def random picker:


rpick = function(fname){
  dat = read.csv(fname,stringsAsFactors = F)
  dat = dat[dat$New != 'msc.csv',]
  dat = dat[dat$FD.salary != 0,]
  qbs = dat[dat$Pos == 'QB',]
  rbs = dat[dat$Pos == 'RB',]
  wrs = dat[dat$Pos == 'WR',]
  qb = sample(1:length(qbs$Pos), 1, replace=FALSE)
  rb = sample(1:length(rbs$Pos), 2, replace=FALSE)
  wr = sample(1:length(wrs$Pos), 3, replace=FALSE)
  
  qb_sal = qbs$FD.salary[qb[1]]
  rb_sal = rbs$FD.salary[rb[1]]+rbs$FD.salary[rb[2]]
  wr_sal = wrs$FD.salary[wr[1]]+wrs$FD.salary[wr[2]]+wrs$FD.salary[wr[3]]
  tot_sal = qb_sal+rb_sal+wr_sal
  
  qb_pts = qbs$FD.points[qb[1]]
  rb_pts = rbs$FD.points[rb[1]]+rbs$FD.points[rb[2]]
  wr_pts = wrs$FD.points[wr[1]]+wrs$FD.points[wr[2]]+wrs$FD.points[wr[3]]
  tot_pts = qb_pts+rb_pts+wr_pts
  
  if (is.na(tot_sal)){
    return(rpick(fname))
  }
  if(is.nan(tot_sal)){
    return(rpick(fname))
  }
  if (tot_sal < 38000){
    return(rpick(fname))
  }
  if (tot_sal > 40000){
    return(rpick(fname))
  }
  return(c(tot_sal,tot_pts))
}

hist(sm(mydata$FPointsP),freq = FALSE,ylim =c(0,0.07),breaks=20, xlab = "Fantasy Points",col = 'dodgerblue',main="Drew Brees\n Estimated Conditional Distribution\n 2011 Week 3")

#padding data
sm = function(s){
  b = seq(0.0,20,1)
  d = rep(b,1)
  return(c(s,b))
}

## check accuracy of conditional distributions
## want to take every 


### Track how algorithms do against each other
ms = rep(0,119)
mds = rep(0,119)
fls = rep(0,119)
ces = rep(0,119)
stds = rep(0,119)
pks = rep(0,119)
mdfls = rep(0,119)
mdces = rep(0,119)
mdstds = rep(0,119)
mdbstds = rep(0,119)
flce = rep(0,119)
rnds = rep(0,119)

sim.ms = list()
sim.mds = list()
sim.fls = list()
sim.ces = list()
sim.stds = list()
sim.pks = list()
sim.mdfls = list()
sim.mdces = list()
sim.mdstds = list()
sim.mdbstds = list()
sim.flce = list()
sim.rnds = list()

ms2 = rep(0,119)
mds2 = rep(0,119)
fls2 = rep(0,119)
ces2 = rep(0,119)
stds2 = rep(0,119)
pks2 = rep(0,119)
mdfls2 = rep(0,119)
mdces2 = rep(0,119)
mdstds2 = rep(0,119)
mdbstds2 = rep(0,119)
flce2 = rep(0,119)
rnds2 = rep(0,119)


### read in all data

### want to generate list of names
gen_spline = function(name){
  mydata = read.csv(name)
  if (length(mydata$Actual) == 0){
    act = 10
  } else{
    act = mydata$Actual[1]
  }
  fps = rep(mydata$FPointsP,1)
  fit = logspline(sm(fps),lbound = -10,ubound = 60)
  z = function(x){dlogspline(x,fit)}
  m = mean(sm(fps))
  md = qlogspline(0.5,fit)
  floor = qlogspline(0.10,fit)
  ceiling = qlogspline(0.90,fit)
  std = sd(sm(fps))
  s = seq(-10,60,0.1)
  ind = which.max(lapply(s,z))
  peak = s[ind] 
  x = c(m,md,floor,ceiling,std,peak,md+floor,md+ceiling,md+std,md-std,floor+ceiling,act)
  return(x)
}

##generate file names
yrs = 2011:2017
wks = 1:17
names = c()
i = 1
for(yr in yrs){
  for(wk in wks){
    names[i] = paste0("rotoguru/all_games_",toString(yr),"_",toString(wk),".csv")
    i = i+1
  }
}

gen_sims = function(df){
  sims = list()
  for(i in 1:length(df$New)){
    mydata = read.csv(df$New[i])
    fit = logspline(sm(mydata$FPointsP),lbound = -10,ubound = 60)
    sims[[i]] = rlogspline(1000,fit)
  }
  tots = rep(0,1000)
  for(i in 1:1000){
    tots[i] = Reduce('+',(lapply(sims,function(x) x[i])))
  }
  return(tots)
}

sim.rpick = function(fname){
  c = grep(NA,1000)
  for(i in 1:1000){
    c[i] = rpick(fname)[2]
  }
}



gen_results = function(fname){
  dat = read.csv(fname,stringsAsFactors = F)
  dat = dat[dat$New != 'msc.csv',]
  dat = dat[dat$FD.salary != 0,]
  dat[,'Value'] = NA
  dat[,'Mean'] = NA
  dat[,'Median'] = NA
  dat[,'Floor'] = NA
  dat[,'Ceiling'] = NA
  dat[,'Std'] = NA
  dat[,'Peak'] = NA
  dat[,'MDFloor'] = NA
  dat[,'MDCeiling'] = NA
  dat[,'MDSTD'] = NA
  dat[,'MDBSTD'] = NA
  dat[,'FLCE'] = NA
  dat[,'Actual'] = NA
  
  for (i in 1:length(dat$New)){
    if (file.exists(dat$New[i])){
      tryCatch({
        r = gen_spline(dat$New[i])
        dat$Mean[i] = r[1]
        dat$Median[i] = r[2]
        dat$Floor[i] = r[3]
        dat$Ceiling[i] = r[4]
        dat$Std[i] = r[5]
        dat$Peak[i] = r[6]
        dat$MDFloor[i] = r[7]
        dat$MDCeiling[i] = r[8]
        dat$MDSTD[i] = r[9]
        dat$MDBSTD[i] = r[10]
        dat$FLCE[i] = r[11]
        dat$Actual[i] = r[12]

      }, warning = function(w){
        print(dat$New[i])
        print(w)
      }, error = function(e){
        print(e)
      })
    }
  }
  
  ##first model:
  m.dat = dat[!is.nan(dat$Mean),]
  m.dat = m.dat[!is.na(m.dat$Mean),]
  m.dat = m.dat[m.dat$Mean > 0.0,]
  m.dat$Value = m.dat$Mean
  m.test <- l(m.dat,cap=40000,constraint="no_opp",league="MyLeague",setplayers=NULL,removeteams=NULL)
  #m.top <- top_teams(m.dat,10,cap=40000,constraint = 'none',league ="MyLeague",setplayers=NULL)
  #m.mean = mean(m.top)
  m.points = gen_sims(m.test) #$ActualPoints[1]

  ##second model:
  md.dat = dat[!is.nan(dat$Median),]
  md.dat = md.dat[!is.na(md.dat$Median),]
  md.dat = md.dat[md.dat$Median > 0.0,]
  md.dat = md.dat[]
  md.dat$Value = md.dat$Mean
  md.test <- l(md.dat,cap=40000,constraint="no_opp",league="MyLeague",setplayers=NULL,removeteams=NULL)
  #md.top <- top_teams(md.dat,10,cap=40000,constraint = 'none',league ="MyLeague",setplayers=NULL)
  #md.mean = mean(md.top)
  md.points = gen_sims(md.test)#$ActualPoints[1]
  
  ##third model:
  floor.dat = dat[!is.nan(dat$Floor),]
  floor.dat = floor.dat[!is.na(floor.dat$Floor),]
  floor.dat = floor.dat[floor.dat$Floor > 0.0,]
  floor.dat$Value = floor.dat$Floor
  floor.test <- l(floor.dat,cap=40000,constraint="no_opp",league="MyLeague",setplayers=NULL,removeteams=NULL)
  floor.points = gen_sims(floor.test) #$ActualPoints[1]
  #floor.top <- top_teams(floor.dat,10,cap=40000,constraint = 'none',league ="MyLeague",setplayers=NULL)
  #floor.mean = mean(floor.top)
  
  ##fourth model:
  ceiling.dat = dat[!is.nan(dat$Ceiling),]
  ceiling.dat = ceiling.dat[!is.na(ceiling.dat$Ceiling),]
  ceiling.dat = ceiling.dat[ceiling.dat$Ceiling > 0.0,]
  ceiling.dat$Value = ceiling.dat$Ceiling
  ceiling.test <- l(ceiling.dat,cap=40000,constraint="no_opp",league="MyLeague",setplayers=NULL,removeteams=NULL)
  ceiling.points = gen_sims(ceiling.test) #$ActualPoints[1]
  #ceiling.top <- top_teams(ceiling.dat,10,cap=40000,constraint = 'none',league ="MyLeague",setplayers=NULL)
  #ceiling.mean = mean(ceiling.top)
  
  ##fourth model:
  std.dat = dat[!is.nan(dat$Std),]
  std.dat = std.dat[!is.na(std.dat$Std),]
  std.dat = std.dat[std.dat$Std > 0.0,]
  std.dat$Value = std.dat$Std
  std.test <- l(std.dat,cap=40000,constraint="no_opp",league="MyLeague",setplayers=NULL,removeteams=NULL)
  std.points = gen_sims(std.test) #$ActualPoints[1]
  
  ##peak
  
  peak.dat = dat[!is.nan(dat$Peak),]
  peak.dat = peak.dat[!is.na(peak.dat$Peak),]
  peak.dat = peak.dat[peak.dat$Peak > 0.0,]
  peak.dat$Value = peak.dat$Peak
  peak.test <- l(peak.dat,cap=40000,constraint="no_opp",league="MyLeague",setplayers=NULL,removeteams=NULL)
  peak.points = gen_sims(peak.test) #$ActualPoints[1]
  
  ## fifth model
  mceiling.dat = dat[!is.nan(dat$MDCeiling),]
  mceiling.dat = mceiling.dat[!is.na(mceiling.dat$MDCeiling),]
  mceiling.dat = mceiling.dat[mceiling.dat$MDCeiling > 0.0,]
  mceiling.dat$Value = mceiling.dat$MDCeiling
  mceiling.test <- l(mceiling.dat,cap=40000,constraint="none",league="MyLeague",setplayers=NULL,removeteams=NULL)
  mceiling.points = gen_sims(mceiling.test) #$ActualPoints[1]
  #mceiling.top <- top_teams(mceiling.dat,10,cap=40000,constraint = 'no_opp',league ="MyLeague",setplayers=NULL)
  #mceiling.mean = mean(mceiling.top)

  # sixth model
  mfl.dat = dat[!is.nan(dat$MDFloor),]
  mfl.dat = mfl.dat[!is.na(mfl.dat$MDFloor),]
  mfl.dat = mfl.dat[mfl.dat$MDFloor > 0.0,]
  mfl.dat$Value = mfl.dat$MDFloor
  mfl.test <- l(mfl.dat,cap=40000,constraint="none",league="MyLeague",setplayers=NULL,removeteams=NULL)
  #mfl.top <- top_teams(mstd.dat,10,cap=40000,constraint = 'no_opp',league ="MyLeague",setplayers=NULL)
  #mfl.mean = mean(mfl.top)
  #mfl.points = mfl.test$ActualPoints[1] 
  mfl.points = gen_sims(mfl.test)  
  
  
  # sixth model
  mstd.dat = dat[!is.nan(dat$MDSTD),]
  mstd.dat = mstd.dat[!is.na(mstd.dat$MDSTD),]
  mstd.dat = mstd.dat[mstd.dat$MDSTD > 0.0,]
  mstd.dat$Value = mstd.dat$MDSTD
  mstd.test <- l(mstd.dat,cap=40000,constraint="none",league="MyLeague",setplayers=NULL,removeteams=NULL)
  #mstd.top <- top_teams(mstd.dat,10,cap=40000,constraint = 'no_opp',league ="MyLeague",setplayers=NULL)
  #mstd.mean = mean(mstd.top)
  #mstd.points = mstd.test$ActualPoints[1]
  mstd.points = gen_sims(mstd.test)
  
  
  mbstd.dat = dat[!is.nan(dat$MDBSTD),]
  mbstd.dat = mbstd.dat[!is.na(mbstd.dat$MDBSTD),]
  mbstd.dat = mbstd.dat[mbstd.dat$MDBSTD > 0.0,]
  mbstd.dat$Value = mbstd.dat$MDBSTD
  mbstd.test <- l(mbstd.dat,cap=40000,constraint="none",league="MyLeague",setplayers=NULL,removeteams=NULL)
  #mbstd.top <- top_teams(mbstd.dat,10,cap=40000,constraint = 'no_opp',league ="MyLeague",setplayers=NULL)
  #mbstd.mean = mean(mbstd.top)
  #mbstd.points = mbstd.test$ActualPoints[1]
  mbstd.points = gen_sims(mbstd.test)
  
  
  flc.dat = dat[!is.nan(dat$FLCE),]
  flc.dat = flc.dat[!is.na(flc.dat$FLCE),]
  flc.dat = flc.dat[flc.dat$FLCE > 0.0,]
  flc.dat$Value = flc.dat$FLCE
  flc.test <- l(flc.dat,cap=40000,constraint="none",league="MyLeague",setplayers=NULL,removeteams=NULL)
  #mstd.top <- top_teams(mstd.dat,10,cap=40000,constraint = 'no_opp',league ="MyLeague",setplayers=NULL)
  #mstd.mean = mean(mstd.top)
  #flc.points = flc.test$ActualPoints[1]
  flc.points = gen_sims(flc.test)
  
  rd.pts = sim.rpick(fname)
  pts = list(m.points,md.points,floor.points,ceiling.points,std.points,peak.points,mfl.points,mceiling.points,mstd.points,mbstd.points,flc.points,rd.pts)
  return(pts)
}

for(i in 11:17){
  tryCatch({
    print(i)
    r = gen_results(names[i])
    sim.ms[i] = r[1]
    sim.mds[i] = r[2]
    sim.fls[i] = r[3]
    sim.ces[i] = r[4]
    sim.stds[i] = r[5]
    sim.pks[i] = r[6]
    sim.mdfls[i] =r[7]
    sim.mdces[i] = r[8]
    sim.mdstds[i] = r[9]
    sim.mdbstds[i] = r[10]
    sim.flce[i] = r[11]
    sim.rnds[i] = r[12]
  }, warning = function(w){
    print(w)
  }, error = function(e){
    print(e)
  }, finally = {
  })
}

sim.rnds = list()

for(i in 1:17){
  #mydat =read.csv(names[i],stringsAsFactors = FALSE)
  sim.rnds[i] = sim.rpick(names[i])
}

a = Reduce(c,sim.ms)
b = Reduce(c,sim.mds)
c = Reduce(c,sim.fls)
d = Reduce(c,sim.ces)
e = Reduce(c,sim.stds)
f = Reduce(c,sim.pks)
g = Reduce(c,sim.mdfls)
h = Reduce(c,sim.mdces)
i = Reduce(c,sim.mdstds)
j = Reduce(c,sim.mdbstds)
k = Reduce(c,sim.flce)
l = Reduce(c,sim.rnds)

p = rep('Mean',1700)
q = rep('Median',1700)
r = rep('Floor',1700)
s = rep('Ceiling',1700)
t = rep('Std',1700)
u = rep('Mode',1700)
v = rep('Random',1700)

r1 = rep('Median+Floor',1700)
r2 = rep('Median+Ceiling',1700)
r3 = rep('Median+Std',1700)
r4 = rep("Median-Std",1700)
r5 = rep('Floor+Ceiling',1700)
dat <- data.frame(xx = c(a,b,c,d,e,f,l),yy = c(p,q,r,s,t,u,v))

dat2 <- data.frame(xx = c(g,h,i,j,k,l),yy = c(r1,r2,r3,r4,r5,v))


d1 =ms2[1:110]
d2 =mds2[1:110]
d3 =fls2[1:110]
d4 =ces2[1:110]
d5 =stds2[1:110]
d6 =pks2[1:110]

e1 = rep('Mean',110)
e2 = rep('Median',110)
e3 = rep('Floor',110)
e4 = rep('Ceilinb',110)
e5 = rep('Std',110)
e6 = rep('Mode',110)

dat2 <- data.frame(xx = c(d1,d2,d3,d4,d5,d6),yy = c(e1,e2,e3,e4,e5,e6))

d7 =mfls2[1:110]
d8 = mces2[1:110]
d9 = 
d10
d11 = flc2[1:110]
d12



dat2 <- data.frame(xx = c(ms))

bp <- ggplot(data=dat, aes(x=yy, y=xx, fill=yy)) + geom_boxplot()+geom_jitter(position=position_jitter(0.2))
bp = bp+labs(x='Optimization Method',y='Fantasy Points',title='Actual Results')
bp = bp+guides(fill=guide_legend(title="Method"))
bp


anova(ms2[1:110],mds2[1:10])

ms2[1:110]
mds2[1:110]
fls2[1:110]
ces2[1:110]
stds2[1:110]
pks2[1:110]
mdfls2[1:110]
mdces2[1:110]
mdstds2[1:110]
mdbstds2[1:110]
flce2[1:110]
rnds2[1:110]

scores_df = data.frame(ms2[1:110],mds2[1:110],fls2[1:110],ces2[1:110],stds2[1:110],pks[1:110],mdfls2[1:110],mdces[1:110],mdstds2[1:110],mdbstds2[1:110],flce2[1:110],rnds2[1:110])

# Ranks the methods
rank_methods = function(i, m) {
  score = scores_df[i,][m]
  sorted = rev(sort(scores_df[i,]))
  if (score == sorted[1]) {
    rank_table[m,1] = rank_table[m,1] + 1
  } else if (score == sorted[2]) {
    rank_table[m,2] = rank_table[m,2] + 1
  } else if (score == sorted[3]) {
    rank_table[m,3] = rank_table[m,3] + 1
  } else if (score == sorted[4]) {
    rank_table[m,4] = rank_table[m,4] + 1
  } else if (score == sorted[5]) {
    rank_table[m,5] = rank_table[m,5] + 1
  } else if (score == sorted[6]) {
    rank_table[m,6] = rank_table[m,6] + 1
  } else if (score == sorted[7]) {
    rank_table[m,7] = rank_table[m,7] + 1
  } else if (score == sorted[8]) {
    rank_table[m,8] = rank_table[m,8] + 1
  } else if (score == sorted[9]) {
    rank_table[m,9] = rank_table[m,9] + 1
  } else if (score == sorted[10]) {
    rank_table[m,10] = rank_table[m,10] + 1
  } else if (score == sorted[11]) {
    rank_table[m,11] = rank_table[m,11] + 1
  } else {
    rank_table[m,12] = rank_table[m,12] + 1
  }
  return(rank_table)
}

rank_table = matrix(0, nrow=12, ncol=12)
for (i in 1:nrow(scores_df)) {
  rank_table = rank_methods(i, 1)
  rank_table = rank_methods(i, 2)
  rank_table = rank_methods(i, 3)
  rank_table = rank_methods(i, 4)
  rank_table = rank_methods(i, 5)
  rank_table = rank_methods(i, 6)
  rank_table = rank_methods(i, 7)
  rank_table = rank_methods(i, 8)
  rank_table = rank_methods(i, 9)
  rank_table = rank_methods(i, 10)
  rank_table = rank_methods(i, 11)
  rank_table = rank_methods(i, 12)
}

colnames(results) = c('Mean', 'Median','Floor','Ceiling','Std','Mode','Median+Floor','Median+Std','Median-Std','Floor+Ceiling','Random')
rownames(rownames) = c('Mean', 'Median', 'Floor', 'Ceiling','Std','Mode','Median+Floor','Median+Std','Median-Std','Floor+Ceiling','Random')

results = data.frame()
for(i in 1:110){
  x = c(ms2[i],mds2[i],fls2[i],ces2[i],stds2[i],pks[i],mdfls2[i],mdces[i],mdstds2[i],mdbstds2[i],flce2[i],rnds2[i])
  s = match(x,sort(x))
  
}

hist(ms2[1:110],col = 'blue')
hist(mds2[1:110],add=TRUE)
hist(fls2[1:110])

dat <- data.frame(xx = c(runif(100,20,50),runif(100,40,80),runif(100,0,30)),yy = rep(letters[1:3],each = 100))

ggplot(scores_df,aes(x=xx)) + 
  geom_histogram(data=subset(dat,yy == 'a'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(dat,yy == 'b'),fill = "blue", alpha = 0.2) +
  geom_histogram(data=subset(dat,yy == 'c'),fill = "green", alpha = 0.2)


ggplot() +
  geom_histogram(data=data.frame(ms2[1:110]),fill = "red", alpha = 0.2) +
  geom_histogram(data=data.frame(mds2[1:110]),fill = "blue", alpha = 0.2) #+
  #geom_histogram(data=fls2[1:110],fill = "green", alpha = 0.2) +
  #geom_histogram(data=ces2[1:110],fill = "yellow", alpha = 0.2)
  







# irf <- IRFrf(data = sim_dt, p = 2)
data = sim_dt
pmax = 5
s = 12
shockvar = 1
d = 1

# n <- nrow(data) - pmax -s
n <- 5
itevar <- rep(list(data), n)
nhist <- 1:n
s <- rep(s, n)
d <- rep(d,n)
pmax <- rep(pmax, n)
shockvar <- rep(shockvar,n)

st_tm <- Sys.time()
ans <- mapply(IRFrf, data = itevar, pmax = pmax, s = s,
              shockvar = shockvar, d = d, histime = nhist, SIMPLIFY = FALSE)
Sys.time() - st_tm

st_tm <- Sys.time()
IRFrf_gen(data = data)
Sys.time() - st_tm

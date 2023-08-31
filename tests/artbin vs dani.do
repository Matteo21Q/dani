artbin, pr(0.1 0.1) margin(0.05) alpha(0.05) power(0.9) wald
artbin, pr(0.1 0.1) margin(0.05) alpha(0.05) power(0.9) 
artbin, pr(0.1 0.1) margin(0.05) alpha(0.05) power(0.9) local
artbin, pr(0.1 0.12) margin(0.05) alpha(0.05) power(0.9) wald
artbin, pr(0.5 0.45) margin(0.4) alpha(0.05) power(0.9) wald
artbin, pr(0.1 0.1) margin(0.05) alpha(0.1) power(0.9) wald
artbin, pr(0.1 0.1) margin(0.05) alpha(0.05) power(0.54) wald
artbin, pr(0.1 0.1) margin(0.05) alpha(0.05) power(0.9) wald aratio(1 2)
artbin, pr(0.1 0.1) margin(0.05) alpha(0.05) power(0.9) wald aratio(2 1)
artbin, pr(0.1 0.1) margin(0.05) alpha(0.05) power(0.9) wald ccorr


artcat, pc(0.1) or(1) margin(1.5) alpha(0.05) power(0.9) ologit(aa)
artcat, pc(0.1) or(1) margin(1.5) alpha(0.05) power(0.9) ologit(na)
artcat, pc(0.1) or(1) margin(1.5) alpha(0.05) power(0.9) ologit(nn)
artcat, pc(0.1) rr(1.2) margin(1.5) alpha(0.05) power(0.9) ologit(aa)
artcat, pc(0.5) rr(0.9) margin(3) alpha(0.05) power(0.9) ologit(aa)
artcat, pc(0.1) or(1) margin(1.5) alpha(0.1) power(0.9) ologit(aa)
artcat, pc(0.1) or(1) margin(1.5) alpha(0.05) power(0.54) ologit(aa)
artcat, pc(0.1) or(1) margin(1.5) alpha(0.05) power(0.9) ologit(aa) aratio(1 2)
artcat, pc(0.1 0.1) margin(1.5) alpha(0.05) power(0.9) ologit(aa) aratio(2 1)
artcat, pc(0.1 0.1) margin(1.5) alpha(0.05) power(0.9) ologit(aa) ccorr

artbin, pr(0.1 0.1) margin(-0.05) alpha(0.05) power(0.9) wald favourable
artcat, pc(0.1) or(1) margin(0.75) alpha(0.05) power(0.9) ologit(aa) favourable

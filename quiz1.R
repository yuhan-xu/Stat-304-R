Celsius = function(Fahrenheit) { C = (5/9)*(Fahrenheit-32); return(C) }
stopifnot(isTRUE(all.equal(Celsius(32), 0)))

baby.dbinom = function(x, size, prob){ p = (factorial(size)/(factorial(x)*factorial(size-x)))*prob^(x)*(1-prob)^(size-x); return (p) }
stopifnot(isTRUE(all.equal(.25, baby.dbinom(0, 2, .5), dbinom(0, 2, .5))))
stopifnot(isTRUE(all.equal(.50, baby.dbinom(1, 2, .5), dbinom(1, 2, .5))))
stopifnot(isTRUE(all.equal(.25, baby.dbinom(2, 2, .5), dbinom(2, 2, .5))))

baby.Welch = function(x, y, mu=0, conf.level=.95) { stopifnot(length(x)>=2); stopifnot(length(y)>=2); stopifnot((0 < conf.level) & (conf.level < 1)); n_x = length(x); n_y = length(y); x.bar = mean(x); y.bar = mean(y); s.x = sd(x); s.y = sd(y); t = (x.bar - y.bar - mu) / sqrt(((s.x)^2)/n_x + ((s.y)^2)/n_y); r = list(); r$statistic = t; r$parameter = (((s.x)^2/n_x + (s.y)^2/n_y)^2)/( (((s.x)^2/n_x)^2)/(n_x-1) + (((s.y)^2/n_y)^2)/(n_y-1) ); r$p.value = 2*pt(q=-abs(t), df=(((s.x)^2/n_x + (s.y)^2/n_y)^2)/( (((s.x)^2/n_x)^2)/(n_x-1) + (((s.y)^2/n_y)^2)/(n_y-1) )); alpha = 1 - conf.level; t.for.conf.level = -qt(p=alpha/2, df=(((s.x)^2/n_x + (s.y)^2/n_y)^2)/( (((s.x)^2/n_x)^2)/(n_x-1) + (((s.y)^2/n_y)^2)/(n_y-1) )); error.margin = t.for.conf.level * sqrt((s.x)^2/n_x + (s.y)^2/n_y); r$conf.int = c(x.bar - y.bar - error.margin, x.bar - y.bar + error.margin); r$estimate = x.bar - y.bar; r$null.value = mu; return(r) }

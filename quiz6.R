v = readLines(con="yxu329Q6.txt")
grep(pattern = "A", x = v)
grep(pattern = "A", x = v, ignore.case = FALSE, value = TRUE)

grep(pattern = "(1|2|3)\\s(a|b|c)", x = v,value=TRUE)


grep(pattern = "^A", ignore.case = FALSE, x = v,value=TRUE)
grep(pattern = "^B", ignore.case = FALSE, x = v,value=TRUE)
grep(pattern = "^C", ignore.case = FALSE, x = v,value=TRUE)

grep(pattern = "[^aeiou]\\>", x = v)


grep(pattern = "(aa|ae|ai|ao|au|ea|ee|ei|eo|eu|ia|ie|ii|io|iu|oa|oe|oi|oo|ou|ua|ue|ui|uo|uu)", x = v,value = TRUE)

v1 = "1+2+3+4+5+6"
strsplit(x = v1, split = " +")


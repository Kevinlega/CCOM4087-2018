(*Kevin Legarreta*)
use "depython.sml";

(*Prints the code in high level*)
fun pretty (Module statement) = prettyStm statement

	(*prints the print statement and the expression*)
and prettyStm (PrintStm p) = (print "print "; prettyExp p; print "\n")
	(*print the expression*)
  | prettyStm (Expr expression) = (prettyExp expression; print "\n")
 	 (* print the compound statement*)
  | prettyStm (CompoundStm (s1,s2)) = (prettyStm s1; prettyStm s2; print "\n")
  	(*prints the assign statement*)
  | prettyStm (Assign (id,s)) = (print id; print " = "; prettyExp s; print "\n")

	(*print the Num value*)
and prettyExp (Num n) = print (Int.toString n)
	(*prints the binOp of add and mult*)
  | prettyExp (BinOp (exp1,operator,exp2)) = (print "("; prettyExp exp1; prettyBinOp operator; prettyExp exp2; print ")")
  	(*print the variable name*)
  | prettyExp (Name name) = print name 

(*prints the binop +*)
and prettyBinOp Add = print "+"
(*prints the binop -*)
  | prettyBinOp Mult = print "*"
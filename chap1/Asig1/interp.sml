(*Kevin Legarreta*)

use "depython.sml";

(*Create a table of id and int values*)
type table = (id * int) list

(*Initialize a table to give the interpeter*)
val t1 = [] : table;

(*Lookup a variable in the table, if is there sends the value else raises a flag.*)
fun lookup ([] : table, name : id) = raise (Fail "Unbound identifier") 
	| lookup ((id,value)::tail, name) = 
		if id = name then
		 value 
		else 
			lookup (tail,name)

(*Updates the table concatenating  new item.*)
fun update(t1 : table, id : id, value : int) = ((id,value)::t1) 

(*Starts the execution of the program.*)
fun interp (Module program : prog) = (interpStm (program,t1); ()) 

(*interpretates the statements*)
(*This one interp' the Expr and returns the table*)
and interpStm (Expr expression, t1) = #2 (interpExp (expression,t1))

	(*This interp' the compound stm by giving the table of the first statement to 
	the second table and returning the table.*)
  | interpStm (CompoundStm (s1,s2),t1) = (interpStm (s2,interpStm (s1,t1)))

  (*This interp' the assign stm by updating the table with the expression value
  and returns the table.*)
  | interpStm (Assign (id,s),t1) = update(t1,id,(#1(interpExp (s,t1)))) 

  (*This prints the result of an expression and return the table*)
  | interpStm (PrintStm stm, t1) = (print (Int.toString (#1(interpExp (stm,t1)))); print "\n"; t1) 

(*interp' the expressions*)
(*This interp' exp Num, returns the value and the same table*)
and interpExp (Num exp, t1) = (exp,t1)

	(*This interp' exp BinOp add, returns the addition of both expressions and the same table*)
  | interpExp (BinOp (exp1,Add,exp2), t1) = ((#1(interpExp (exp1,t1))) + (#1(interpExp (exp2,t1))),t1)

	(*This interp' exp BinOp mult, returns the multiplication of both expressions and the same table*)
  | interpExp (BinOp (exp1,Mult,exp2), t1) = ((#1(interpExp (exp1,t1))) * (#1(interpExp (exp2,t1))),t1)

  	(*This interp' exp Name, Looks for it in the table and returns the value of that name and the table*)
  | interpExp (Name name,t1) = (lookup(t1, name),t1)

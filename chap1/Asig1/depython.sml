type id = string

datatype binop = Add | Mult

datatype prog = Module of stm

and stm = Assign of id * exp
        | CompoundStm of stm * stm
        | PrintStm of exp
        | Expr of exp

and exp =  Num of int
         | BinOp of exp * binop * exp
         | Name of id

val p1 = Module (Expr (Num 123));

val p2 = Module (Expr (BinOp (Num 3, Add, Num 2))) : prog;

val p3 = Module (CompoundStm (Assign ("a", Num 3), PrintStm (BinOp (Name "a", Mult, Num 2)))); 

val p4 = Module (CompoundStm (Expr (BinOp (Num 3, Add, Num 2)), Expr (BinOp (Num 4, Add, Num 6))));

val p5 = Module(PrintStm (BinOp(BinOp(BinOp(Num 3, Add, Num 5), Mult, Num 2), Add, Num 3)));

(*like a = b = c = 1*)
val p6 = Module (CompoundStm(Assign ("c",Num 1), CompoundStm(Assign ("b",Name "c"), CompoundStm(Assign ("a",Name "b"),CompoundStm(PrintStm (Name "a"), CompoundStm(PrintStm (Name "b"), PrintStm (Name "c")))))));

val p7 = Module(Expr(BinOp(BinOp(BinOp(Num 5, Add, Num 7), Mult, BinOp(Num 5, Add, Num 7)), Mult, Num 5)));

val p8 = Module(PrintStm(BinOp(BinOp(BinOp(Num 5, Add, Num 7), Mult, BinOp(Num 5, Add, Num 7)), Mult, Num 5)));

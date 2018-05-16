Kevin Legarreta 801-14-3452*)

structure Semant :
     sig val transProg : Absyn.exp -> unit end =
struct
  structure A = Absyn
  structure S = Symbol

  structure Translate = struct type exp = unit end

  
  val globalstart = ref 0
(*  val parameteri = ref 0*)

  type expty = {exp: Translate.exp, ty: Types.ty}
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table

  fun checkInt ({ty=Types.INT, exp=_}, pos) = ()

  | checkInt ({ty=_,exp=_},pos) = ErrorMsg.error pos "integer required\n"

  fun checkComparison ({exp=_,ty=Types.INT},{exp=_,ty=Types.INT},pos) = ()


    | checkComparison ({exp=_,ty=_},{exp=_,ty=_},pos) = (ErrorMsg.error pos "Can't compare this\n")


  fun transProg (exp:A.exp) : unit =
    (print "\nmain:\n";
    let
      val {ty=_, exp=prog} = transExp (Env.base_venv, Env.base_tenv) exp
    in
      prog
    end)
  and transExp(venv:venv,tenv:tenv) : A.exp -> expty =
    let 
      val ifcount = ref 1
      val globalif = ref 1
     
      fun trexp (A.IntExp num) = {ty=Types.INT, exp=(
                    print ("    li $a0 " ^(Int.toString (num)) ^ "\n")
      )}
 
       | trexp (A.VarExp var) = (trvar(var))

       | trexp (A.LetExp{decs,body,pos}) = (
 
              globalstart := !globalstart + 1;
              print("    b start" ^(Int.toString (!globalstart))^"\n");        
              let val { venv=venv', tenv=tenv' } = 
                    transDecs(venv,tenv,decs)
              in       
                print("\nstart" ^(Int.toString (!globalstart))^":\n");
                transExp(venv',tenv') body
                
              end
              )
        | trexp (A.SeqExp exprs) = trexprs (exprs)


        | trexp (A.CallExp{func,args,pos}) =( 

                case S.look(venv, func) of
                      NONE => ({exp=ErrorMsg.error pos ("This " ^ S.name func ^ " is not a function"), ty=Types.UNIT})

                    | SOME(Env.FunEntry({formals, result})) => ({exp=(
                      print "    sw $fp 0($sp)\n";
                      print "    addiu $sp $sp -4\n";
                      do_formals(args);
                      print ("    jal " ^ S.name func ^ "\n")
                      )
                    , ty=result})

                    | SOME(_) => ({exp=ErrorMsg.error pos ("var " ^ S.name func ^ " not function " ), ty=Types.UNIT}))
                   

        | trexp (A.IfExp {test=A.OpExp {left, right, oper=A.EqOp, pos=postest}, then', else'=SOME(elseexp), pos=posif}) =
           (
            ifcount := !globalif;
            trexp left;
            print "    sw $a0, 0($sp)\n";
            print "    addiu $sp, $sp, -4\n";
            trexp right;
            print "    lw $t1 4($sp)\n";
            print "    addiu $sp, $sp, 4\n";
            print ("    beq $a0, $t1, true_branch" ^(Int.toString (!ifcount))^"\n");
            print ("\nfalse_branch" ^(Int.toString (!ifcount)) ^ ":\n");
            ifcount := !ifcount+1;
            globalif := !globalif+1;
            trexp elseexp;
            ifcount := !ifcount-1;
            print ("    b end_if" ^(Int.toString (!ifcount))^ "\n");
            print ("\ntrue_branch" ^(Int.toString (!ifcount)) ^ ":\n");
            trexp then';
            print ("\nend_if"^(Int.toString (!ifcount)) ^ ":\n");
            
           {ty=Types.INT, exp=()})

        | trexp (A.OpExp{left=left,oper=A.PlusOp,right=right,pos=pos}) =
            let
                val e1 = trexp left
            in
                checkInt (e1, pos);
                print "    sw $a0, 0($sp)\n";     (* save e1 *)
                print "    addiu $sp, $sp, -4\n";       (* push stack *)
                let
                    val e2 = trexp right
                in
                    checkInt (e2, pos)
                end;
                {ty=Types.INT,
                exp=(
                    print "    lw $t1, 4($sp)\n";    (* copy e1 to tmp *)
                    print "    add $a0, $t1, $a0\n"; (* add e1 and e2 *)
                    print "    addiu $sp, $sp, 4\n"        (* pop stack *)
                )}
            end

        | trexp (A.OpExp{left=left,oper=A.MinusOp,right=right,pos=pos}) =
            let
                val e1 = trexp left
            in
                checkInt (e1, pos);
                print "    sw $a0, 0($sp)\n";     (* save e1 *)
                print "    addiu $sp, $sp, -4\n";       (* push stack *)
                let
                    val e2 = trexp right
                in
                    checkInt (e2, pos)
                end;
                {ty=Types.INT,
                exp=(
                    print "    lw $t1, 4($sp)\n";    (* copy e1 to tmp *)
                    print "    sub $a0, $t1, $a0\n"; (* sub e1 and e2 *)
                    print "    addiu $sp, $sp, 4\n"        (* pop stack *)
                )}
            end

         | trexp (A.OpExp{left=left,oper=A.TimesOp,right=right,pos=pos}) =
            let
                val e1 = trexp left
            in
                checkInt (e1, pos);
                print "    sw $a0, 0($sp)\n";     (* save e1 *)
                print "    addiu $sp, $sp, -4\n";       (* push stack *)
                let
                    val e2 = trexp right
                in
                    checkInt (e2, pos)
                end;
                {ty=Types.INT,
                exp=(
                    print "    lw $t1, 4($sp)\n";    (* copy e1 to tmp *)
                    print "    mul $a0, $t1, $a0\n"; (* mul e1 and e2 *)
                    print "    addiu $sp, $sp, 4\n"  (* pop stack *)
                )}
            end

          | trexp (A.OpExp{left=left,oper=A.DivideOp,right=right,pos=pos}) =
            let
                val e1 = trexp left
            in
                checkInt (e1, pos);
                print "    sw $a0, 0($sp)\n";           (* save e1 *)
                print "    addiu $sp, $sp, -4\n";       (* push stack *)
                let
                    val e2 = trexp right
                in
                    checkInt (e2, pos)
                end;
                {ty=Types.INT,
                exp=(
                    print "    lw $t1, 4($sp)\n";    (* copy e1 to tmp *)
                    print "    div $a0, $t1, $a0\n";      (* div e1 and e2 *)
                    print "    addiu $sp, $sp, 4\n"        (* pop stack *)
                )}
            end

      | trexp _ = {ty=Types.UNIT, exp=ErrorMsg.error 0 "Can't typecheck this yet"}    


  and trexprs ((exp, pos)::exprs) = (trexp exp ; trexprs exprs)
    | trexprs nil = {ty=Types.NIL, exp=()}

  and do_formals(formal::formals) =
      (do_formals(formals);
      trexp formal; 
      print "    sw $a0 0($sp)\n";
      print "    addiu $sp $sp -4\n")
      
      | do_formals(nil) = ()

  and actual_ty ty = case ty of
                      Types.NAME(symbol, rty) => actual_ty (valOf (S.look (tenv, symbol)))  
                    | final_ty => final_ty

  and trvar (A.SimpleVar (id,pos)) = (case S.look(venv, id) of
          SOME (Env.VarEntry{ty}) => {exp=(
              print("    lw $a0, 4($fp)\n")

(*            parameteri := !parameteri +1;
*)
(*            print("    lw $a0, " ^ (Int.toString (!parameteri*4)) ^"($fp)\n")
*)
                    ), ty=actual_ty ty}
        | _ => ({exp=ErrorMsg.error pos ("undefined variable: " ^ S.name(id)), ty=Types.UNIT})                ) 
    in
      trexp
    end

and transTy (tenv,A.NameTy(sym,pos)) = (case S.look(tenv,sym) of SOME(t) => t)

and transDecs(venv,tenv,dec::decs) = let 
                                        val {venv=venv', tenv=tenv' } = transDec(venv,tenv,dec) 
                                     in

                                        transDecs(venv',tenv',decs)
                                     end

  | transDecs(venv,tenv,nil) = {tenv=tenv, venv=venv}


and transDec (venv, tenv, A.FunctionDec[{name,params,body,pos,result=NONE}]) = 
          (
            let 
              val x = ((length params)*4)+8
              val SOME(result_ty) = SOME(Types.INT)
              
              fun transparam{name,escape,typ,pos} = case S.look(tenv,typ) of SOME t => {name=name,ty=t}

              val params' = map transparam params
              val venv' = S.enter(venv,name, Env.FunEntry{formals= map #ty params', result=result_ty})

              fun enterparam ({name,ty},venv) = S.enter(venv,name,Env.VarEntry{ty=ty})
              
              val venv'' = foldr enterparam venv' params'

              in 
                ( print("\n"^S.name name ^ ":\n");
                  print"    move $fp $sp\n";
                  print"    sw $ra 0($sp)\n";
                  print"    addiu $sp $sp -4\n";
                  transExp(venv'',tenv) body;
                  print"    lw $ra 4($sp)\n";
                  print("    addiu $sp $sp "^(Int.toString (x))^"\n");
                  print"    lw $fp 0($sp)\n";
                  print"    jr $ra\n";
                  {venv=venv',tenv=tenv})
              end )

end








(*This here is for functions declared like so: fib(n:int):int not needed*)

(*and transDec (venv, tenv, A.FunctionDec[{name,params,body,pos,result=SOME(rt,pos2)}]) = *)           
          
           (*( *)  
            (*parameteri := 0;*)
            (*let
              val x = ((length params)*4)+8
              val SOME(result_ty) = S.look(tenv,rt)
              
              fun transparam{name,escape,typ,pos} = case S.look(tenv,typ) of SOME t => {name=name,ty=t}

              val params' = map transparam params
              val venv' = S.enter(venv,name, Env.FunEntry{formals= map #ty params', result=result_ty})

              fun enterparam ({name,ty},venv) = S.enter(venv,name,Env.VarEntry{ty=ty})
              
              val venv'' = foldr enterparam venv' params'

              in 
                ( print("\n"^S.name name ^ ":\n");
                  print"    move $fp $sp\n";
                  print"    sw $ra 0($sp)\n";
                  print"    addiu $sp $sp -4\n";
                  transExp(venv'',tenv) body;
                  print"    lw $ra 4($sp)\n";
                  print("    addiu $sp $sp "^(Int.toString (x))^"\n");
                  print"    lw $fp 0($sp)\n";
                  print"    jr $ra\n"; *)

                  (*parameteri := !parameteri - !parameteri;*) 
        (*          {venv=venv',tenv=tenv})
              end )*)
(*end
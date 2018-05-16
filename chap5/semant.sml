(*Kevin Legarreta 801-14-3452*)

structure Semant :
     sig val transProg : Absyn.exp -> unit end =
struct
  structure A = Absyn
  structure S = Symbol

  structure Translate = struct type exp = unit end

  type expty = {exp: Translate.exp, ty: Types.ty}
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table

  fun checkInt ({ty=Types.INT, exp=_}, pos) = ()

  | checkInt ({ty=_,exp=_},pos) = ErrorMsg.error pos "integer required\n"

(*  fun checkEquality ({exp=_,ty=Types.INT},{exp=_,ty=Types.INT},pos) = ()

    | checkEquality ({exp=_,ty=Types.STRING},{exp=_,ty=Types.STRING},pos) = ()

    | checkEquality ({exp=_,ty=_},{exp=_,ty=_},pos) = (ErrorMsg.error pos "Can't compare this\n")*)

  fun checkComparison ({exp=_,ty=Types.INT},{exp=_,ty=Types.INT},pos) = ()

    | checkComparison ({exp=_,ty=Types.STRING},{exp=_,ty=Types.STRING},pos) = ()

    | checkComparison ({exp=_,ty=_},{exp=_,ty=_},pos) = (ErrorMsg.error pos "Can't compare this\n")

  fun transProg (exp:A.exp) : unit =
    let
      val {ty=_, exp=prog} = transExp (Env.base_venv, Env.base_tenv) exp
    in
      prog
    end
  and transExp(venv:venv,tenv:tenv) : A.exp -> expty =
    let fun trexp (A.OpExp{left, oper, right, pos}) =
      let

          datatype Oper = MATH | COMP | EQ

          fun operselection (oper) : Oper =
              case oper of
                A.PlusOp => MATH
              | A.MinusOp => MATH
              | A.TimesOp => MATH
              | A.DivideOp => MATH
              | A.LtOp => COMP
              | A.GtOp => COMP
              | A.LeOp => COMP
              | A.GeOp => COMP
              | A.EqOp => EQ
              | A.NeqOp => EQ
      in
        case operselection(oper) of 
            MATH => (checkInt (trexp left, pos); checkInt (trexp right, pos);
                  {ty=Types.INT, exp=()})

          | COMP => (checkComparison(trexp left,trexp right,pos);{ty=Types.INT, exp=()})

          | EQ    => (checkComparison(trexp left,trexp right,pos);{ty=Types.INT, exp=()})
      end
            
       | trexp (A.IntExp _) = {ty=Types.INT, exp=()}

       | trexp (A.StringExp (_,_)) = {ty=Types.STRING, exp=()}

       | trexp (A.SeqExp exprs) = trexprs (exprs)

       | trexp (A.AssignExp{var,exp,pos}) =
            let
                val {exp=left,ty=varty} = trvar (var)
                val {exp=right,ty=expty} = trexp (exp)
            in
                if varty <> expty then
                  ({exp=ErrorMsg.error pos "mismatch assignment",ty=Types.UNIT})
                else
                  {exp=(),ty=Types.UNIT}
            end

       | trexp (A.NilExp) = {ty=Types.NIL, exp=()}
       | trexp (A.VarExp var) = (trvar(var))

       | trexp (A.LetExp{decs,body,pos}) = 
             
              let val { venv=venv', tenv=tenv' } = 
                    transDecs(venv,tenv,decs)
              in 
                transExp(venv',tenv') body
              end

        | trexp (A.CallExp{func,args,pos}) = 
              let
                  fun checkArgs (formalTy::formalList, argumentExp::argumentList, pos) = 
                          if (formalTy = (#ty (trexp argumentExp)))
                          then checkArgs(formalList, argumentList, pos)
                          else ErrorMsg.error pos "Formals and Arguments have different types"
                    
                    | checkArgs ([], [], pos) = ()
                    
                    | checkArgs ([], argumentExp::argumentList, pos) = 
                                ErrorMsg.error pos "Formals are less then Arguments"
                    
                    | checkArgs (forTy::formalList, [], pos) = 
                                ErrorMsg.error pos "Formals are more then Arguments"
        
              in
                  case S.look(venv, func) of

                      NONE => ({exp=ErrorMsg.error pos ("This " ^ S.name func ^ " is not a function"), ty=Types.UNIT})

                    | SOME(Env.FunEntry({formals, result})) => (checkArgs(formals, args, pos); {exp=(), ty=result})

                    | SOME(_) => ({exp=ErrorMsg.error pos ("var " ^ S.name func ^ " not function " ), ty=Types.UNIT})
                    
              end

        | trexp (A.IfExp {test, then'=thenExp, else'=elseExp, pos=pos}) =

         (case elseExp of

           NONE => (if ((#ty (trexp thenExp)) <> Types.UNIT)
                       then {exp=(), ty=(#ty (trexp thenExp))} 
                    else ({exp=ErrorMsg.error pos "Then is not unit", ty=(#ty (trexp thenExp))}))

         | SOME elseExp => (if (#ty (trexp thenExp)) = ((#ty (trexp elseExp))) 
                                then ({exp=(), ty=(#ty (trexp thenExp))}) 
                            else ({exp=ErrorMsg.error pos "Then and else not same type", ty=(#ty (trexp thenExp))})))

      | trexp _ = {ty=Types.UNIT, exp=ErrorMsg.error 0 "Can't typecheck this yet"}    


  and trexprs ((exp, pos)::exprs) = (trexp exp ; trexprs exprs)  
    | trexprs nil = {ty=Types.NIL, exp=()}

  and actual_ty ty = case ty of
                      Types.NAME(symbol, rty) => actual_ty (valOf (S.look (tenv, symbol)))  
                    | final_ty => final_ty

  and trvar (A.SimpleVar (id,pos)) = (case S.look(venv, id) of
          SOME (Env.VarEntry{ty}) => {exp=(), ty=actual_ty ty}
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


and transDec (venv,tenv, A.VarDec{name,typ=SOME ty,init,escape,pos}) = 
            let 
              val {exp,ty} = transExp(venv,tenv) init
            in
              {tenv=tenv,venv=S.enter(venv,name,Env.VarEntry{ty=ty})}
            end

    | transDec (venv,tenv, A.TypeDec[{name,ty,pos}] ) = {venv=venv,tenv=S.enter(tenv,name,transTy(tenv,ty))}
    

    | transDec (venv, tenv, A.FunctionDec[{name,params,body,pos,result=SOME(rt,pos2)}]) = 
            let 
              val SOME(result_ty) = S.look(tenv,rt)
              
              fun transparam{name,escape,typ,pos} = case S.look(tenv,typ) of SOME t => {name=name,ty=t}

              val params' = map transparam params
              val venv' = S.enter(venv,name, Env.FunEntry{formals= map #ty params', result=result_ty})

              fun enterparam ({name,ty},venv) = S.enter(venv,name,Env.VarEntry{ty=ty})
              
              val venv'' = foldr enterparam venv' params'

              in 
                transExp(venv'',tenv) body;{venv=venv',tenv=tenv}
              end 

end
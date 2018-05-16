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
  | checkInt ({ty=_,exp=_},pos) = ErrorMsg.error pos "integer required"

  fun transProg (exp:A.exp) : unit =
    let
      val {ty=_, exp=prog} = transExp (Env.base_venv, Env.base_tenv) exp
    in
      prog
    end
  and transExp(venv:venv,tenv:tenv) : A.exp -> expty =
    let fun trexp (A.OpExp{left, oper, right, pos}) =
      (checkInt (trexp left, pos);
       checkInt (trexp right, pos);
       {ty=Types.INT, exp=()})
       | trexp (A.IntExp _) = {ty=Types.INT, exp=()}
       | trexp (A.StringExp (_,_)) = {ty=Types.STRING, exp=()}
       | trexp (A.SeqExp exprs) = trexprs (exprs)
       | trexp (A.AssignExp{var,exp,pos}) =
            let
                val {exp=left,ty=vty} = trvar (var)
                val {exp=right,ty=ety} = trexp (exp)
            in
                if vty <> ety then
                  (ErrorMsg.error pos "mismatch assignment"; {exp=(),ty=Types.UNIT})
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

      | trexp _ = {ty=Types.UNIT, exp=ErrorMsg.error 0 "Can't typecheck this yet"}    

  and trexprs ((exp, pos)::exprs) = (trexp exp ; trexprs exprs)  
    | trexprs nil = {ty=Types.NIL, exp=()}

  and actual_ty ty = case ty of
                      Types.NAME(symbol, rty) => actual_ty (valOf (S.look (tenv, symbol)))  
                    | final_ty => final_ty

  and trvar (A.SimpleVar (id,pos)) = (case S.look(venv, id) of
          SOME (Env.VarEntry{ty}) => {exp=(), ty=actual_ty ty}
        | _ => (ErrorMsg.error pos ("undefined variable: " ^ S.name id); {exp=(), ty=Types.INT})                ) 
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

  
end

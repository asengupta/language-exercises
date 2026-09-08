let rec lookup key dict=
  match dict with
  | [] -> None
  | (k,v)::t -> if k=key then Some(v) else lookup key t

type symbolic_expr =
| Const of int
| Var of string
| Add of symbolic_expr * symbolic_expr
| Sub of symbolic_expr * symbolic_expr
| Mul of symbolic_expr * symbolic_expr;;

type simplify_result = {
  expr: symbolic_expr;
  changed: bool
};;

let simplify expr env=
  let rec simplify_ expr_env env=
    begin
      let {expr;changed}=expr_env in
      match expr with
      | Var(var) -> let env_val=lookup var env in
                              begin
                                match env_val with
                                  | None -> {expr=Var(var);changed=changed}
                                  | Some(varVal) -> {expr=Const(varVal);changed=true}
                              end
      | Add(lhs,rhs) -> let {expr=lhs';changed=lhs_changed}=simplify_ {expr=lhs;changed=false} env in
                        let {expr=rhs';changed=rhs_changed}=simplify_ {expr=rhs;changed=false} env in
                        let binary_expr=lol (fun x y -> Add(x,y)) env lhs' rhs' lhs rhs lhs_changed rhs_changed in
                        begin
                          match (lhs',rhs') with
                          | (Const(lhs_),Const(rhs_)) -> {expr=Const(lhs_+rhs_);changed=true}
                          | (Const(0),rhs) -> {expr=rhs;changed=true}
                          | (lhs,Const(0)) -> {expr=lhs;changed=true}
                          | (_,_) -> binary_expr
                        end
      | Sub(lhs,rhs) -> let {expr=lhs';changed=lhs_changed}=simplify_ {expr=lhs;changed=false} env in
                        let {expr=rhs';changed=rhs_changed}=simplify_ {expr=rhs;changed=false} env in
                        let binary_expr=lol (fun x y -> Sub(x,y)) env lhs' rhs' lhs rhs lhs_changed rhs_changed in
                        begin
                          match (lhs',rhs') with
                          | (Const(lhs_),Const(rhs_)) -> {expr=Const(lhs_-rhs_);changed=true}
                          | (lhs,Const(0)) -> {expr=lhs';changed=true}
                          | (_,_) -> binary_expr
                        end
      | Mul(lhs,rhs) -> let {expr=lhs';changed=lhs_changed}=simplify_ {expr=lhs;changed=false} env in
                        let {expr=rhs';changed=rhs_changed}=simplify_ {expr=rhs;changed=false} env in
                        let binary_expr=lol (fun x y -> Mul(x,y)) env lhs' rhs' lhs rhs lhs_changed rhs_changed in
                        begin
                          match (lhs',rhs') with
                          | (Const(lhs_),Const(rhs_)) -> {expr=Const(lhs_*rhs_);changed=true}
                          | (Const(0),rhs) -> {expr=Const(0);changed=true}
                          | (lhs,Const(0)) -> {expr=Const(0);changed=true}
                          | (Const(1),rhs_) -> {expr=rhs';changed=true}
                          | (lhs_,Const(1)) -> {expr=lhs';changed=true}
                          | (_,_) -> binary_expr
                        end
      | anything -> {expr=anything;changed=changed}
    end
    and lol ctor env lhs' rhs' lhs rhs lhs_changed rhs_changed=
      if (lhs'=lhs && rhs'=rhs)
      then {expr=ctor lhs' rhs';changed=lhs_changed || rhs_changed}
      else (simplify_ {expr=(ctor lhs' rhs');changed=true} env)
  in simplify_ {expr=expr;changed=false} env;;


let e=[("x",3);("y",7)];;
simplify (Add (Const 0, Add (Var "x", Const 0)) e);;
simplify (Add (Add (Const 0, Const 0), Var "y") e);;
simplify (Add(Const 2, Const 3) e);;
simplify (Add(Const 0, Add(Const 2, Sub(Const 5, Const 2))) e);;



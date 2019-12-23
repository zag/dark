open Prelude
open Tc

type t = Types.fluidExpr [@@deriving show]

let id (expr : t) : Types.id =
  match expr with
  | EOldExpr expr ->
      Blank.toID expr
  | EInteger (id, _)
  | EString (id, _)
  | EBool (id, _)
  | ENull id
  | EFloat (id, _, _)
  | EVariable (id, _)
  | EFieldAccess (id, _, _, _)
  | EFnCall (id, _, _, _)
  | ELambda (id, _, _)
  | EBlank id
  | ELet (id, _, _, _, _)
  | EIf (id, _, _, _)
  | EPartial (id, _, _)
  | ERightPartial (id, _, _)
  | EList (id, _)
  | ERecord (id, _)
  | EPipe (id, _)
  | EPipeTarget id
  | EBinOp (id, _, _, _, _)
  | EConstructor (id, _, _, _)
  | EFeatureFlag (id, _, _, _, _, _)
  | EMatch (id, _, _) ->
      id


let rec find (target : Types.id) (expr : t) : t option =
  let fe = find target in
  if id expr = target
  then Some expr
  else
    match expr with
    | EInteger _
    | EBlank _
    | EString _
    | EVariable _
    | EBool _
    | ENull _
    | EPipeTarget _
    | EFloat _ ->
        None
    | ELet (_, _, _, rhs, next) ->
        fe rhs |> Option.orElse (fe next)
    | EIf (_, cond, ifexpr, elseexpr) ->
        fe cond |> Option.orElse (fe ifexpr) |> Option.orElse (fe elseexpr)
    | EBinOp (_, _, lexpr, rexpr, _) ->
        fe lexpr |> Option.orElse (fe rexpr)
    | EFieldAccess (_, expr, _, _) | ELambda (_, _, expr) ->
        fe expr
    | ERecord (_, fields) ->
        fields |> List.map ~f:Tuple3.third |> List.filterMap ~f:fe |> List.head
    | EMatch (_, expr, pairs) ->
        fe expr
        |> Option.orElse
             ( pairs
             |> List.map ~f:Tuple2.second
             |> List.filterMap ~f:fe
             |> List.head )
    | EFnCall (_, _, exprs, _)
    | EList (_, exprs)
    | EConstructor (_, _, _, exprs)
    | EPipe (_, exprs) ->
        List.filterMap ~f:fe exprs |> List.head
    | EOldExpr _ ->
        None
    | EPartial (_, _, oldExpr) | ERightPartial (_, _, oldExpr) ->
        fe oldExpr
    | EFeatureFlag (_, _, _, cond, casea, caseb) ->
        fe cond |> Option.orElse (fe casea) |> Option.orElse (fe caseb)


let findParent (target : Types.id) (expr : t) : t option =
  let rec findParent' ~(parent : t option) (target : Types.id) (expr : t) :
      t option =
    let fp = findParent' ~parent:(Some expr) target in
    if id expr = target
    then parent
    else
      match expr with
      | EInteger _
      | EBlank _
      | EString _
      | EVariable _
      | EBool _
      | ENull _
      | EPipeTarget _
      | EFloat _ ->
          None
      | ELet (_, _, _, rhs, next) ->
          fp rhs |> Option.orElse (fp next)
      | EIf (_, cond, ifexpr, elseexpr) ->
          fp cond |> Option.orElse (fp ifexpr) |> Option.orElse (fp elseexpr)
      | EBinOp (_, _, lexpr, rexpr, _) ->
          fp lexpr |> Option.orElse (fp rexpr)
      | EFieldAccess (_, expr, _, _) | ELambda (_, _, expr) ->
          fp expr
      | EMatch (_, _, pairs) ->
          pairs
          |> List.map ~f:Tuple2.second
          |> List.filterMap ~f:fp
          |> List.head
      | ERecord (_, fields) ->
          fields
          |> List.map ~f:Tuple3.third
          |> List.filterMap ~f:fp
          |> List.head
      | EFnCall (_, _, exprs, _)
      | EList (_, exprs)
      | EConstructor (_, _, _, exprs)
      | EPipe (_, exprs) ->
          List.filterMap ~f:fp exprs |> List.head
      | EOldExpr _ ->
          None
      | EPartial (_, _, expr) ->
          fp expr
      | ERightPartial (_, _, expr) ->
          fp expr
      | EFeatureFlag (_, _, _, cond, casea, caseb) ->
          fp cond |> Option.orElse (fp casea) |> Option.orElse (fp caseb)
  in
  findParent' ~parent:None target expr


let toNexpr (expr : t) : Types.expr =
  let open Types in
  let rec toNexpr' ?(inPipe = false) expr =
    (* inPipe is whether it's the immediate child of a pipe. *)
    let r = toNexpr' ~inPipe:false in
    match expr with
    | EInteger (id, num) ->
        F (id, Value (FluidUtil.literalToString (`Int num)))
    | EString (id, str) ->
        F (id, Value (FluidUtil.literalToString (`String str)))
    | EFloat (id, whole, fraction) ->
        F (id, Value (FluidUtil.literalToString (`Float (whole, fraction))))
    | EBool (id, b) ->
        F (id, Value (FluidUtil.literalToString (`Bool b)))
    | ENull id ->
        F (id, Value (FluidUtil.literalToString `Null))
    | EVariable (id, var) ->
        F (id, Variable var)
    | EFieldAccess (id, obj, fieldID, "") ->
        F (id, FieldAccess (toNexpr' obj, Blank fieldID))
    | EFieldAccess (id, obj, fieldID, fieldname) ->
        F (id, FieldAccess (toNexpr' obj, F (fieldID, fieldname)))
    | EFnCall (id, name, args, ster) ->
      ( match args with
      | EPipeTarget _ :: _ when not inPipe ->
          recover
            "fn has a pipe target but no pipe"
            ~debug:expr
            (Blank.new_ ())
      | EPipeTarget _ :: args when inPipe ->
          F
            ( id
            , FnCall
                (F (ID (deID id ^ "_name"), name), List.map ~f:r args, ster) )
      | _nonPipeTarget :: _ when inPipe ->
          recover
            "fn has a pipe but no pipe target"
            ~debug:expr
            (Blank.new_ ())
      | args ->
          F
            ( id
            , FnCall
                (F (ID (deID id ^ "_name"), name), List.map ~f:r args, ster) )
      )
    | EBinOp (id, name, arg1, arg2, ster) ->
      ( match arg1 with
      | EPipeTarget _ when not inPipe ->
          recover
            "binop has a pipe target but no pipe"
            ~debug:expr
            (Blank.new_ ())
      | EPipeTarget _ when inPipe ->
          F
            ( id
            , FnCall (F (ID (deID id ^ "_name"), name), [toNexpr' arg2], ster)
            )
      | _nonPipeTarget when inPipe ->
          recover
            "binop has a pipe but no pipe target"
            ~debug:expr
            (Blank.new_ ())
      | _ ->
          F
            ( id
            , FnCall
                ( F (ID (deID id ^ "_name"), name)
                , [toNexpr' arg1; toNexpr' arg2]
                , ster ) ) )
    | ELambda (id, vars, body) ->
        F
          ( id
          , Lambda
              ( List.map vars ~f:(fun (vid, var) -> Types.F (vid, var))
              , toNexpr' body ) )
    | EBlank id ->
        Blank id
    | ELet (id, lhsID, lhs, rhs, body) ->
        F (id, Let (F (lhsID, lhs), toNexpr' rhs, toNexpr' body))
    | EIf (id, cond, thenExpr, elseExpr) ->
        F (id, If (toNexpr' cond, toNexpr' thenExpr, toNexpr' elseExpr))
    | EPartial (id, str, oldVal) ->
        F (id, FluidPartial (str, toNexpr' ~inPipe oldVal))
    | ERightPartial (id, str, oldVal) ->
        F (id, FluidRightPartial (str, toNexpr' ~inPipe oldVal))
    | EList (id, exprs) ->
        F (id, ListLiteral (List.map ~f:r exprs))
    | ERecord (id, pairs) ->
        F
          ( id
          , ObjectLiteral
              (List.map pairs ~f:(fun (id, k, v) ->
                   (Types.F (id, k), toNexpr' v) )) )
    | EPipe (id, exprs) ->
      ( match exprs with
      | head :: tail ->
          F (id, Thread (r head :: List.map ~f:(toNexpr' ~inPipe:true) tail))
      | [] ->
          Blank id )
    | EConstructor (id, nameID, name, exprs) ->
        F (id, Constructor (F (nameID, name), List.map ~f:r exprs))
    | EMatch (id, mexpr, pairs) ->
        let pairs =
          List.map pairs ~f:(fun (p, e) ->
              (FluidPattern.toPattern p, toNexpr' e) )
        in
        F (id, Match (toNexpr' mexpr, pairs))
    | EPipeTarget _ ->
        recover
          "Cant convert pipetargets back to exprs"
          ~debug:expr
          (Blank.new_ ())
    | EFeatureFlag (id, name, nameID, cond, caseA, caseB) ->
        F
          ( id
          , FeatureFlag
              ( F (nameID, name)
              , toNexpr' cond
              , toNexpr' ~inPipe caseA
              , toNexpr' ~inPipe caseB ) )
    | EOldExpr expr ->
        expr
  in
  toNexpr' expr


let isBlank (expr : t) = match expr with EBlank _ -> true | _ -> false

let isEmpty (expr : t) : bool =
  match expr with
  | EBlank _ ->
      true
  | ERecord (_, []) ->
      true
  | ERecord (_, l) ->
      l
      |> List.filter ~f:(fun (_, k, v) -> k = "" && not (isBlank v))
      |> List.isEmpty
  | EList (_, l) ->
      l |> List.filter ~f:(not << isBlank) |> List.isEmpty
  | _ ->
      false


let hasEmptyWithId (id : Types.id) (expr : t) : bool =
  match find id expr with Some e -> isEmpty e | _ -> false


let walk ~(f : t -> t) (expr : t) : t =
  match expr with
  | EInteger _
  | EBlank _
  | EString _
  | EVariable _
  | EBool _
  | ENull _
  | EPipeTarget _
  | EFloat _ ->
      expr
  | ELet (id, lhsID, name, rhs, next) ->
      ELet (id, lhsID, name, f rhs, f next)
  | EIf (id, cond, ifexpr, elseexpr) ->
      EIf (id, f cond, f ifexpr, f elseexpr)
  | EBinOp (id, op, lexpr, rexpr, ster) ->
      EBinOp (id, op, f lexpr, f rexpr, ster)
  | EFieldAccess (id, expr, fieldID, fieldname) ->
      EFieldAccess (id, f expr, fieldID, fieldname)
  | EFnCall (id, name, exprs, ster) ->
      EFnCall (id, name, List.map ~f exprs, ster)
  | ELambda (id, names, expr) ->
      ELambda (id, names, f expr)
  | EList (id, exprs) ->
      EList (id, List.map ~f exprs)
  | EMatch (id, mexpr, pairs) ->
      EMatch
        (id, f mexpr, List.map ~f:(fun (name, expr) -> (name, f expr)) pairs)
  | ERecord (id, fields) ->
      ERecord
        (id, List.map ~f:(fun (id, name, expr) -> (id, name, f expr)) fields)
  | EPipe (id, exprs) ->
      EPipe (id, List.map ~f exprs)
  | EConstructor (id, nameID, name, exprs) ->
      EConstructor (id, nameID, name, List.map ~f exprs)
  | EOldExpr _ ->
      expr
  | EPartial (id, str, oldExpr) ->
      EPartial (id, str, f oldExpr)
  | ERightPartial (id, str, oldExpr) ->
      ERightPartial (id, str, f oldExpr)
  | EFeatureFlag (id, msg, msgid, cond, casea, caseb) ->
      EFeatureFlag (id, msg, msgid, f cond, f casea, f caseb)


let update ?(failIfMissing = true) ~(f : t -> t) (target : Types.id) (ast : t)
    : t =
  let found = ref false in
  let rec run e =
    if target = id e
    then (
      found := true ;
      f e )
    else walk ~f:run e
  in
  let finished = run ast in
  if failIfMissing
  then
    asserT
      ~debug:(target, ast)
      "didn't find the id in the expression to update"
      !found ;
  finished


(* FIXME: [replace] is just [update] with a hack for EPipe.
 * It's very unclear which to use at what point and likely to cause bugs.
 * We should either hide [update] from the public interface of FluidExpression
 * or remove [replace] and put the special-case EPipe logic into the calling code. *)
let replace ~(replacement : t) (target : Types.id) (ast : t) : t =
  let open Types in
  (* If we're putting a pipe into another pipe, fix it up *)
  let target', newExpr' =
    match (findParent target ast, replacement) with
    | Some (EPipe (parentID, oldExprs)), EPipe (newID, newExprs) ->
        let before, elemAndAfter =
          List.splitWhen ~f:(fun nested -> id nested = target) oldExprs
        in
        let after = List.tail elemAndAfter |> Option.withDefault ~default:[] in
        (parentID, EPipe (newID, before @ newExprs @ after))
    | _ ->
        (target, replacement)
  in
  update target' ast ~f:(fun _ -> newExpr')
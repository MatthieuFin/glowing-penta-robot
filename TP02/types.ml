(*
    Auteurs:
        | Damien PICARD
        | Benjamin ZIGH
*)
type terme =
  | True
  | False
  | Zero
  | Cond of terme * terme * terme
  | Succ of terme 
  | Pred of terme
  | IsZero of terme
;; 

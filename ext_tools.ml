
let log s = print_string s ; print_newline () ; flush stdout

let srule_id_from_rule_id env rid = (Model.get_rule env rid).Primitives.syntactic_rule

let rule_ast_name env rule_id = 
  Format.asprintf "%a" 
    (Model.print_ast_rule ~env) 
    (srule_id_from_rule_id env rule_id)

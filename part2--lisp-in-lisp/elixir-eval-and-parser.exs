### a dynamically scoped mini-lisp variant
### -- my first elixir program that does something.

## todo: more elixirish error handling


defmodule Minilisp do

  defmodule Environment do
    ## nb: order of {sym,val} pairs matters!
    def lookup(sym,[{sym,val}|_]), do: val
    def lookup(sym,[{_,_}|env]), do: lookup(sym,env)
    def lookup(sym,[]), do: raise("unbound symbol " <> inspect(sym))
    
    def extend(syms,vals,env), do: Enum.zip(syms,vals) ++ env

    ## "reserved words"; but why are they here?
    def is_primop(sym), do: Enum.member?(["+","-","*","=","<","car","cdr","cons","num?","atom?","list"],sym)
  end

  defmodule Evaluator do

    def eval_expr(expr,topenv) do
      try do
	eval(expr,topenv)
      rescue
	err in RuntimeError -> err
      end      
    end
    
    ## evaluation of atoms:
    def eval([],_), do: []
    def eval("T",_), do: "T" 
    def eval(n,_) when is_number(n), do: n  
    def eval(s,env) when is_binary(s), do: if Environment.is_primop(s), do: s, else: Environment.lookup(s,env)    
    def eval({"proc",args,body},_), do: {"proc",args,body} # [for lexical lisp there will be env enclosed too!]
    ## the three special forms:
    def eval(["quote",e],_), do: e
    def eval(["if",prem,concl,alt],env), do: if eval(prem,env)=="T", do: eval(concl,env), else: eval(alt,env)
    def eval(["^",args,body],env), do: {"proc",args,body} # [nb: we'll enclose env here in lexical case!]
    ## applications:
    def eval([rator|rands],env), do: apply_fun(eval(rator,env),Enum.map(rands,fn(e) -> eval(e,env) end),env)
    ## dunno...
    def eval(e,_), do: raise("error evaluating " <> inspect(e))    
    ## applying primitives:
    def apply_fun("+",[n1,n2],_), do: n1+n2
    def apply_fun("-",[n1,n2],_), do: n1-n2  
    def apply_fun("*",[n1,n2],_), do: n1*n2
    def apply_fun("<",[n1,n2],_), do: if n1<n2, do: "T", else: []
    def apply_fun("=",[e1,e1],_), do: "T"
    def apply_fun("=",[_,_],_), do: []
    def apply_fun("car",[[h|_]],_), do: h
    def apply_fun("cdr",[[_|tl]],_), do: tl
    def apply_fun("cons",[h,tl],_), do: [h|tl]
    def apply_fun("num?",[e],_), do: if is_number(e), do: "T", else: []
    def apply_fun("atom?",[[_|_]],_), do: []
    def apply_fun("atom?",[_],_), do: "T"
    def apply_fun("list",es,_), do: es
    ## applying user-defined procedures:
    # [in lexical case we'll extend with first env enclosed in proc, and then one from third argument.]
    def apply_fun({"proc",args,body},vals,env), do: eval(body,Environment.extend(args,vals,env))    
    ## sometimes things go wrong...
    def apply_fun(f,_,_), do: raise("error applying " <> inspect(f))
  end
  
  ### since [["^",["x"],["*","x","x"]] , ["+",2,3]] does not read well,
  ### here comes a crude SExp parser (does not recognize dotted pairs):
  defmodule Parser do    

    def parse_expr(str) do
      try do
	{expr,rest} = parse(tokenize(str))
	if rest==[], do: expr, else: raise("SYNTAX ERROR: superfluous part: "<>Enum.join(rest," "))
      rescue
	err in RuntimeError -> err
      end
    end
    
    def parse(["("|rest]), do: parse_list(rest)
    def parse([")"|[]]), do: raise("SYNTAX ERROR: superfluous closing paren")
    def parse([")"|rest]), do: raise("SYNTAX ERROR: superfluous closing paren before" <> Enum.join(rest," "))        
    def parse(["'"|rest]) do
      {expr, rest} = parse(rest)
      {["quote",expr], rest}
    end
    def parse([token|rest]), do: {mk_atom(token,Float.parse(token)), rest}    
    def parse(_), do: raise("SYNTAX ERROR: missing closing paren")
    
    def mk_atom(_,{num,""}), do: num
    def mk_atom(token,_), do: token
    
    def parse_list([")"|rest]), do: {[], rest}
    def parse_list(tokens) do
      {head, rest} = parse(tokens)
      {tail, rest} = parse_list(rest)
      {[head|tail], rest}
    end
    
    ### ''everybody stand back, I know regular expressions.''
    def tokenize(str), do: Regex.scan(~r/[\s]*([()']|[^\s()']+)/, str, capture: :all_but_first) |> List.flatten        

    ### reversing the parse proces:
    def unparse([h|t]) when is_number(t) or is_binary(t), do: "(" <> unparse(h) <> " . " <> unparse(t) <> ")"
    def unparse([h|t]), do: "(" <> unparse(h) <> unparse_list(t)
    def unparse({"proc",_,_}), do: "&procedure"
    def unparse(sym) when is_binary(sym), do: sym
    def unparse(e), do: inspect(e) # numbers and maybe sth...
    def unparse_list([]), do: ")"
    def unparse_list([h|t]), do: " " <> unparse(h) <> unparse_list(t)  
  end  
end

## eg:
Minilisp.Parser.parse_expr("((^ (x) (* x x)) (+ 2 3))") |> Minilisp.Evaluator.eval_expr([]) |> Minilisp.Parser.unparse()

# [the following one will not work in lexical lisp without Y:]
Minilisp.Parser.parse_expr(
  "( (^ (fac map) (map fac '(1 2 3 4 5)))"
  <> "(^ (n) (if (= n 0) 1 (* n (fac (- n 1)))))"
  <> "(^ (f xs) (if (= xs ()) () (cons (f (car xs)) (map f (cdr xs)))))"
  <>")")
  |> Minilisp.Evaluator.eval_expr([])
|> Minilisp.Parser.unparse()
### -- how cool is that?

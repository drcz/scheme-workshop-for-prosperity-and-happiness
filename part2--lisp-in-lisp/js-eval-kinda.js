/* small almost-working dynamic lisp in js */

var bool_to_Tnil = function(b) {
    if(b) return "T"; else return [];	
};

var sexp_eq = function(e1,e2) {
    //return(e1==e2);
    if(Array.isArray(e1)) {
	if(Array.isArray(e2) && e1.length==e2.length) {	    
	    for(var i=0;i<e1.length;i++) {
		if(!sexp_eq(e1[i],e2[i])) return false;
	    }
	    return true;
	}
	return false;
    } else {
	if(!Array.isArray(e2) && e1==e2) return true;
	else return false;
    }				
};

var kar = function(expr) {
    return expr[0];
};

var kdr = function(expr) {
    var tl=[];
    for(var i=1;i<expr.length;i++) tl[i-1]=expr[i];
    return tl;
};

var kons = function(e1,e2) {
    var e12 = [];
    e12[0]=e1;
    for(var i=0;i<e2.length;i++) e12[i+1]=e2[i];
    return e12;
};

var is_nil = function(expr) {	
    return Array.isArray(expr) && expr.length==0;
};

var is_numeric = function(expr) {
    return typeof expr == "number";
};

var is_proc = function(expr) {
    return (Array.isArray(expr) && expr[0]=="&proc");
};

var is_primop = function(expr) {
    switch(expr) {
    case "car": case "cdr": case "cons":
    case "atom?": case "num?": case "=":
    case "<": case "+": case "*": case "-": case "/":
    case "input": case "print":
	return true;
    default:
	return false;
    }
};

var is_symbol = function(expr) { /// todo wzmocnic
    return(typeof expr == "string");
};


var is_self_evaluating = function(expr) {
    return (is_numeric(expr)
	    || is_nil(expr)
	    || (is_symbol(expr) && expr=="T")
	    || is_primop(expr)
	    || is_proc(expr));
};

var is_quote_form = function(expr) {
    return (Array.isArray(expr) && kar(expr)=="quote");
};

var is_lambda_form = function(expr) {
    return (Array.isArray(expr) && kar(expr)=="^");
};

var is_if_form = function(expr) {
    return (Array.isArray(expr) && kar(expr)=="if");
};

var make_binding = function(args,vals,env) {
    var binding=[];    
    var prev_keys = Object.keys(env);
    for(var i=0;i<prev_keys.length;i++) {
	binding[prev_keys[i]] = env[prev_keys[i]];
    }
    /// this goes later to overwrite 
    for(var i=0;i<args.length;i++) {
	binding[args[i]] = vals[i];
    }    
    return binding;
};

var Eval = function(expr,env) {		
    if(is_self_evaluating(expr)) return expr;
    if(is_symbol(expr)) return env[expr];
    if(is_quote_form(expr)) return expr[1];
    if(is_if_form(expr)) {
	if(Eval(expr[1],env)=="T")
	    return Eval(expr[2],env);
	else return Eval(expr[3],env);
    }
    if(is_lambda_form(expr)) {
	return ["&proc",expr[1],expr[2]];
    }
    /// now the applications...
    evexpr = expr.map(function(e) {return Eval(e,env);});
    if(is_proc(kar(evexpr))) {
	var body = evexpr[0][2];
	var args = evexpr[0][1];		
	var vals = kdr(evexpr);
	return Eval(body,make_binding(args,vals,env));
    } else {
	var rator = kar(evexpr);
	var rands = kdr(evexpr);
	switch(rator) {
	case "car": return kar(rands[0]);
	case "cdr": return kdr(rands[0]);
	case "cons": return kons(rands[0],rands[1]);
	case "+": return rands[0]+rands[1];
	case "-": return rands[0]-rands[1];	
	case "*": return rands[0]*rands[1];
	case "=":  return bool_to_Tnil(sexp_eq(rands[0],rands[1]));
	    /// add some more here
	}
    }
};

/// todo: a repl?
/*
  var prog = [
  ["def","foldr",["^",["op","e","xs"],
  ["if",["=","xs",[]],
  "e",
  ["op",["car","xs"],["foldr","op","e",["cdr","xs"]]]]]],
  ["def","sum",["^",["xs"],["foldr","+",0,"xs"]]],
  ["sum",["quote",[1,2,3,4]]]
  ];
*/

console.log( Eval([["^",["f"],["f",5]],
	           ["^",["n"],
	            ["if",["=","n",0],1,["*","n",["f",["-","n",1]]]]]],
	          {}) );


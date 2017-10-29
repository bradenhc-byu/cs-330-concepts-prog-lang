module TransInt

push!(LOAD_PATH,".")

using Lexer
using Error
export parse, calc, analyze, interp, interp_pp, NumVal, ClosureVal

# =====================================================
# definitions
#

# dictionary of symbol lookups
dict = Dict(
  :+ => +,
  :- => -,
  :* => *,
  :/ => /,
  :mod => mod)

# array of symbols that cannot be used as function or variable names
invalid_ids = Symbol[:+,:-,:*,:/,:mod,:collatz,:if0,:with,:lambda]

# Abstract syntax

abstract OWL

type NumNode <: OWL
    n::Real
end

type BinopNode <: OWL
    op::Function
    lhs::OWL
    rhs::OWL
end

type MultiPlusNode <: OWL
  params::Array{OWL}
end

type Unop <: OWL
  op::Function
  arg::OWL
end

type CollatzNode <: OWL
  arg::OWL
end

type If0Node <: OWL
  condition::OWL
  zero_branch::OWL
  nonzero_branch::OWL
end

type AndNode <: OWL
  args::Array{OWL}
end

type WithNode <: OWL
  variables::Array{Symbol}
  binding_exprs::Array{OWL}
  body::OWL
end

type IdNode <: OWL
  name::Symbol
end

type FunDefNode <: OWL
    formal_parameters::Array{Symbol}
    fun_body::OWL
end

type FunAppNode <: OWL
    fun_expr::OWL
    arg_exprs::Array{OWL}
end

# Rejigger our type hierarchy to better support return values

abstract RetVal
abstract Environment

type NumVal <: RetVal
  n::Real
end

type ClosureVal <: RetVal
    params::Array{Symbol}
    body::OWL
    env::Environment  # this is the environment at definition time!
end

# Definitions for our environment data structures



type mtEnv <: Environment
end

type CEnvironment <: Environment
  name::Symbol
  value::RetVal
  parent::Environment
end


# ======================================================
# parse functions - create the abstract parse tree
#

# catch any invalid expressions
function parse(expr::Any)
  throw(LispError("PARSE ERROR: Invalid expression type not covered by parse()"))
end

# parse a number
function parse(expr::Real)
  return NumNode(expr)
end

# parse a symbol
function parse(expr::Symbol)
  if in(expr,invalid_ids)
    throw(LispError("PARSE ERROR: use of predefined id"))
  else
    return IdNode(expr)
  end
end

#parse an OWL tree item
function parse(expr::Array{Any})
  binops = [:+,:-,:*,:/,:mod]

  if length(expr) == 1
    if isa(expr[1],Array{Any})
      return parse(expr[1])
    # we are looking at simply an id or a number
    elseif isa(expr[1],Symbol) && !in(expr[1],invalid_ids)
      return IdNode(expr[1])
    elseif is(expr[1],Real)
      return NumNode(expr[1])
    else
      throw(LispError("PARSE ERROR: Invalid terminal type of " * string(expr[1]) * " -- must be of type Real or Symbol"))
    end

  elseif length(expr) == 2 && expr[1] == :-
    return Unop(-,parse(expr[2]))

  elseif in(expr[1],binops)
    if length(expr) > 3
      if expr[1] == :+
        args = OWL[]
        for i = 2:length(expr)
          single_arg = parse(expr[i])
          args = vcat(args,OWL[single_arg])
        end
        return MultiPlusNode(args)
      else
        throw(LispError("PARSE ERROR: too many arguments for binop function"))
      end
    elseif length(expr) < 3
      throw(LispError("PARSE ERROR: too few arguments for binop function"))
    else
      return BinopNode(dict[expr[1]],parse(expr[2]),parse(expr[3]))
    end

  elseif expr[1] == :collatz
    if length(expr) != 2
      throw(LispError("PARSE ERROR: invalid arity for (collatz)"))
    else
      return CollatzNode(parse(expr[2]))
    end

  elseif expr[1] == :if0
    if length(expr) != 4
      throw(LispError("PARSE ERROR: invalid arity for (if0)"))
    else
      return If0Node(parse(expr[2]), parse(expr[3]), parse(expr[4]))
    end

  elseif expr[1] == :and
    if length(expr) < 3
      throw(LispError("PARSE ERROR: Not enough arguments in (and) function"))
    end
    args = OWL[]
    for i = 2:length(expr)
      nextArg = parse(expr[i])
      args = vcat(args,OWL[nextArg])
    end
    return AndNode(args)

  elseif expr[1] == :with
    if length(expr) != 3
      throw(LispError("PARSE ERROR: invalid arity for (with)"))
    else
      symbols = Symbol[]
      b_exprs = OWL[]
      for i = 1:length(expr[2])
        if !isa(expr[2][i],Array{Any})
          throw(LispError("PARSE ERROR: second argument of (with) not an array"))
        elseif length(expr[2][i]) != 2
          throw(LispError("PARSE ERROR: Invalid binding in (with) - too many arguments"))
        else
          sym = expr[2][i][1]
          if(!isa(sym,Symbol))
            throw(LispError("PARSE ERROR: Invalid binding in (with) expression - must use a symbol"))
          elseif in(sym,symbols)
              throw(LispError("PARSE ERROR: attempt to use variable twice in (with)"))
          end
          bind = parse(expr[2][i][2])
          symbols = vcat(symbols,Symbol[sym])
          b_exprs = vcat(b_exprs,OWL[bind])
        end
      end
      return WithNode(symbols, b_exprs, parse(expr[3]))
    end

  elseif expr[1] == :lambda
    if length(expr) != 3
      throw(LispError("PARSE ERROR: invalid arity for (lambda)"))
    elseif !isa(expr[2],Array{Any})
      throw(LispError("PARSE ERROR: parameters for (lambda) must be given in a list"))
    end
    if length(expr[2]) == 0
      return parse(expr[3])
    else
      noDup = Symbol[]
      for i = 1:length(expr[2])
        if in(expr[2][i],noDup)
          throw(LispError("PARSE ERROR: attempt to use duplicate variables in function definition"))
        else
          noDup = vcat(noDup,Symbol[expr[2][i]])
        end
      end
      return FunDefNode(expr[2],parse(expr[3]))
    end

  else
    args = OWL[]
    for i = 2:(length(expr))
      parg = parse(expr[i])
      args = vcat(OWL[parg],args)
    end
    return FunAppNode(parse(expr[1]),args)

  end

  throw(LispError("PARSE ERROR: Unknown operator in expression"))
end

# =========================================================
# analyze functions

function analyze(ast::NumNode)
  return ast
end

function analyze(ast::IdNode)
  return ast
end

function analyze(ast::BinopNode)
  alhs = analyze(ast.lhs)
  arhs = analyze(ast.rhs)
  # No simplification is performed for this lab
  #=
  if isa(alhs, NumNode) && isa(arhs, NumNode)
    if(ast.op == Function(/) && rhs.n == 0)
      throw(LispError("CALC ERROR: Division by Zero"))
    end
    return NumNode( ast.op(alhs.n, arhs.n))
  end
  =#
  return BinopNode(ast.op,alhs,arhs)
end

function analyze(ast::MultiPlusNode)
  if length(ast.params) == 1
    return analyze(ast.params[1])
  else
    next = MultiPlusNode(getindex(ast.params,2:length(ast.params)))
    return BinopNode(+, analyze(ast.params[1]), analyze(next))
  end
end

function analyze(ast::If0Node)
  acond = analyze(ast.condition)
  # we are not supposed to use dead code removal for this lab
  #=
  if isa(acond, NumNode)
    if acond.n == 0
      return analyze(ast.zero_branch)
    else
      return analyze(ast.nonzero_branch)
    end
  end
  =#
  return If0Node(acond, analyze(ast.zero_branch), analyze(ast.nonzero_branch))
end

function analyze(ast::AndNode)
  if length(ast.args) == 1
    return If0Node(ast.args[1], NumNode(0), NumNode(1))
  else
    next = AndNode(getindex(ast.args,2:length(ast.args)))
    return If0Node(ast.args[1], NumNode(0), analyze(next))
  end

end

function analyze(ast::WithNode)
  closure_body = analyze(ast.body)
  fdn = FunDefNode(ast.variables, closure_body)
  return FunAppNode(fdn, ast.binding_exprs)
end

function analyze(ast::FunDefNode)
  return FunDefNode(ast.formal_parameters, analyze(ast.fun_body))
end

function analyze(ast::FunAppNode)
  for i = 1:length(ast.arg_exprs)
    ast.arg_exprs = analyze(ast.arg_exprs)
  end
  return FunAppNode(analyze(ast.fun_expr), ast.arg_exprs)
end

# ========================================================
# calc functions - takes an OWL and returns the result
#

# initializing calc function to create an empty envioronment

function calc(owl::OWL)
  return calc(owl,mtEnv())
end

# calculate a binary operation
function calc(owl::BinopNode, env::Environment)
  lhs = calc(owl.lhs, env)
  rhs = calc(owl.rhs, env)
  if(owl.op == Function(/) && rhs.n == 0)
    throw(LispError("CALC ERROR: Division by Zero"))
  elseif !isa(lhs,NumVal) || !isa(rhs,NumVal)
    throw(LispError("TYPE ERROR: cannot perform binop operation on element not of type NumNode"))
  else
    return NumVal( owl.op( lhs.n, rhs.n) )
  end
end

# calculate the value of a number
function calc(owl::NumNode, env::Environment)
  return NumVal(owl.n)
end

function calc(owl::Unop, env::Environment)
  result = calc(owl.arg, env)
  if !isa(result,NumVal)
    throw(LispError("CALC ERROR: atempt to apply unary '-' to something other than a number"))
  end
  return NumVal(-1 * result.n)
end

function calc(owl::CollatzNode, env::Environment)
  return NumVal( collatz(calc(owl.arg, env).n) )
end

function calc( owl::If0Node, env::Environment )
  if calc(owl.condition, env).n == 0
    return NumVal( calc( owl.zero_branch, env ).n )
  else
    return NumVal( calc( owl.nonzero_branch, env ).n )
  end
end

# recursively determine the value of a variable
function calc( owl::IdNode, env::Environment)
  if isa(env, mtEnv)
    throw(LispError("Attempt to reference undefined variable: " * string(owl.name)))
  elseif owl.name == env.name
    return env.value
  else
    return calc(owl, env.parent)
  end
end

# we need to build the environment
function calc( owl::WithNode, env::Environment)
  extended_env = env
  for i = 1:length(owl.variables)
    val = calc(owl.binding_exprs[i], env)
    extended_env = CEnvironment(owl.variables[i],val,extended_env)
  end
  return calc(owl.body, extended_env)
end

# now time to deal with calculating functions
# defining a function
function calc(owl::FunDefNode, env::Environment)
  return ClosureVal(owl.formal_parameters, owl.fun_body, env)
end

# calling a function
function calc(owl::FunAppNode, env::Environment)
  closure_val = calc(owl.fun_expr,env)
  if length(closure_val.params) != length(owl.arg_exprs)
    throw(LispError("ARITY ERROR: length of paramter list for function does not match number of given arguments"))
  end
  ext_env = env
  for i = 1:length(closure_val.params)
    actual_param = calc(owl.arg_exprs[i], env)
    ext_env = CEnvironment(closure_val.params[i], actual_param, ext_env)
  end
  return calc(closure_val.body, ext_env)
end

# ==========================================================
# collatz function - returns the number of recursions
#
function collatz( n::Real )
  if(n < 0)
    throw(LispError("COLLATZ ERROR: argument must be positive"))
  else
    return collatz_helper( n, 0 )
  end
end

function collatz_helper( n::Real, num_iters::Int )
  if n == 1
    return num_iters
  end
  if mod(n,2)==0
    return collatz_helper( n/2, num_iters+1 )
  else
    return collatz_helper( 3*n+1, num_iters+1 )
  end
end

# ==========================================================
# interp function for self testing
#
# interprets and evaluates an expression
function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    ast = analyze(ast)
    env = mtEnv()
    return calc( ast, env )
end

# pretty print interp function
function interp_pp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    ast = analyze(ast)
    pp(ast,0)
    env = mtEnv()
    return calc( ast, env )
end

#
# ===================================================
# ===================================================
# ===================================================
#

# A simple pretty printer to help us inspect the AST

function pp( ast::OWL, depth::Int )
    throw( LispError( "PP - Unknown node!" ) )
end


function pp( ast::FunDefNode, depth::Int )
    expr_str = "(lambda ("
    for i = 1:length(ast.formal_parameters)
      expr_str = expr_str * string(ast.formal_parameters[i])
      if i != length(ast.formal_parameters)
        expr_str = expr_str * " "
      else
        expr_str = expr_str * ") "
      end
    end
    print(expr_str)
    pp( ast.fun_body, depth+1 )
    print( ")" )
end

function pp( ast::FunAppNode, depth::Int )
    print( "(" )
    pp( ast.fun_expr, depth+1 )
    print( " " )
    for i = 1:length(ast.arg_exprs)
      pp( ast.arg_exprs[i], depth+1 )
      if i != length(ast.arg_exprs)
        print(" ")
      end
    end
    print( ")" )
end

function pp( ast::NumNode, depth::Int )
    print( ast.n )
end

function pp( ast::BinopNode, depth::Int )
    print( "(", string(ast.op), " " )
    pp( ast.lhs, depth+1 )
    print( " " )
    pp( ast.rhs, depth+1 )
    print( ")" )
end

function pp( ast::If0Node, depth::Int )
    print( "(if0 " )
    pp( ast.condition, depth+1 )
    print( " " )
    pp( ast.zero_branch, depth+1 )
    print( " " )
    pp( ast.nonzero_branch, depth+1 )
    print( ")" )
end

function pp( ast::WithNode, depth::Int )
    print( "(with ", ast.name, " " )
    pp( ast.binding_expr, depth+1 )
    print( " " )
    pp( ast.body, depth+1 )
    print( ")" )
end

function pp( ast::IdNode, depth::Int )
    print( ast.name )
end

end # module

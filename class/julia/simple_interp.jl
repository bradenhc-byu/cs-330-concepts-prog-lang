module CI1

push!(LOAD_PATH,".")

using Error
using Lexer
export parse, calc, interp, NumVal, ClosureVal



abstract OWL

type NumNode <: OWL
    n::Real
end

type PlusNode <: OWL
    lhs::OWL
    rhs::OWL
end

type MinusNode <: OWL
    lhs::OWL
    rhs::OWL
end

type If0Node <: OWL
  cond::OWL
  zerobranch::OWL
  nonzerobranch::OWL
end

type WithNode <: OWL
  variable::Symbol
  binding_expr::OWL
  body::OWL
end

# in the abstract syntax tree there are no assignments of variables
# it is only parsing, so we don't need to store the value of the Symbol
# here
type SymbolNode <: OWL
  variable::Symbol
end

type FuncDefNode <: OWL
  formal_param::Symbol
  body::OWL
end

type FuncAppNode <: OWL
  closure::OWL
  arg::OWL
end

abstract Environment

abstract RetVal

type ClosureVal <: RetVal
  cur_env::Environment
  formal_param::Symbol
  body::OWL
end

type NumVal <: RetVal
  n::Number
end

type EmptyEnv <: Environment
end

type ConcreteEnv <: Environment
  variable::Symbol
  value::RetVal
  parent::Environment
end



# ***********************************************************
# ***********************************************************
# begin interp function

# interprets and evaluates an expression
function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    ast = parse( lxd )
    return calc( ast, EmptyEnv() )
end


# end interp function
# ***********************************************************
# ***********************************************************
# begin parse functions


function parse(expr::Any)
  error("Bad programmer! Bad!")
end

function parse(expr::Real)
    return NumNode(expr)
end

function parse(expr::Symbol)
  return SymbolNode(expr)
end


function parse( expr::Array{Any} )
    # should hand us an array, should be length three

    if expr[1] == :+
        return PlusNode(parse(expr[2]), parse(expr[3]))

    elseif expr[1] == :-
        return MinusNode(parse(expr[2]), parse(expr[3]))

    elseif expr[1] == :if0
      return IF0Node(parse(expr[2]), parse(expr[3]), parse(expr[4]))

    elseif expr[1] == :with
      return WithNode(expr[2],parse(expr[3]),parse(expr[4]))

    elseif expr[1] == :lambda
      return FuncDefNode(expr[2],parse(expr[3]))

    else
      return FuncAppNode(parse(expr[1]),parse(expr[2]))

    end
end

# end parse functions
# ********************************************************
# begin analyze functions
# ********************************************************

function analyze(ast::NumNode)
  return ast
end

function analyze(ast::SymbolNode)
  return ast
end

function analyze(ast::PlusNode)
  alhs = analyze(ast.lhs)
  arhs = analyze(ast.rhs)
  if isa(alhs, NumNode) && isa(arhs, NumNode)
    return NumNode( alhs.n + arhs.n)
  end
  return PlusNode(alhs,arhs)
end

function analyze(ast::MinusNode)
  alhs = analyze(ast.lhs)
  arhs = analyze(ast.rhs)
  if isa(alhs, NumNode) && isa(arhs, NumNode)
    return NumNode( alhs.n - arhs.n)
  end
  return MinusNode(alhs,arhs)
end

function analyze(ast::If0Node)
  acond = analyze(ast.cond)
  if isa(acond, NumNode)
    if acond.n == 0
      return analyze(ast.zerobranch)
    else
      return analyze(ast.nonzerobranch)
    end
  end
  return If0Node(acond, analyze(ast.zerobranch), analyze(ast.nonzerobranch))
end

function analyze(ast::WithNode)
  closure_body = analyze(ast.body)
  fdn = FuncDefNode(ast.variable, closure_body)
  return FuncAppNode(fdn, actual_param_expression)
end

function analyze(ast::FuncDefNode)
  return FuncDefNode(ast.formal_param, analyze(ast.body))
end

function analyze(ast::FuncAppNode)
  return FuncAppNode(analyze(ast.closure), analyze(ast.arg))
end


# end analyze functions
# ********************************************************
# ********************************************************
# begin calc functions


function calc( owl::NumNode, env::Environment )
    return NumVal(owl.n)
end

function calc( owl::PlusNode, env::Environment )
    return NumVal( calc(owl.lhs, env).n + calc(owl.rhs, env).n )
end

function calc( owl::MinusNode, env::Environment )
    return NumVal( calc(owl.lhs, env).n - calc(owl.rhs, env).n )
end

function calc( owl::If0Node, env::Environment )
  if calc(owl.cond) == 0
    return calc( owl.zerobranch, env )
  else
    return calc( owl.nonzerobranch, env )
  end
end

# we need to build the environment
function calc( owl::WithNode, env::Environment)
  val = calc(owl.binding_expr, env)
  ext_env = ConcreteEnv(owl.variable, val, env)
  # supporting recursion
  if isa(val,ClosureVal)
    val.cur_env = ext_env
  end
  return calc(owl.body, ext_env)
end

# recursively determine the value of a variable
function calc( owl::SymbolNode, env::Environment)
  if isa(env, EmptyEnv)
    error("Attempt to reference undefined variable: " * string(owl.variable))
  elseif owl.variable == env.variable
    return env.value
  else
    return calc(owl, env.parent)
  end
end

# defining a function
function calc(owl::FuncDefNode, env::Environment)
  return ClosureVal(env,owl.formal_param, owl.body)
end

# calling a function
function calc(owl::FuncAppNode, env::Environment)
  closure_val = calc(owl.closure,env)
  actual_param = calc(owl.arg, env)
  ext_env = ConcreteEnv(closure_val.formal_param, actual_param, closure_val.cur_env)
  return calc(closure_val.body, ext_env)
end


end # module

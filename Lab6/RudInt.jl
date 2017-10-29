module RudInt

push!(LOAD_PATH,".")

using Lexer
using Error

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

abstract OWL

type Num <: OWL
	n::Real
end

type Binop <: OWL
	op::Function
	lhs::OWL
	rhs::OWL
end

type Unop <: OWL
  op::Function
  arg::OWL
end

type CollatzNode <: OWL
  arg::OWL
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
  return Num(expr)
end

#parse an OWL tree item
function parse(expr::Array{Any})
  binops = [:+,:-,:*,:/,:mod]
  if length(expr) == 2 && expr[1] == :-
    return Unop(-,parse(expr[2]))
  elseif in(expr[1],binops)
    if length(expr) > 3
      throw(LispError("PARSE ERROR: too many arguments for binary operator"))
    else
      return Binop(dict[expr[1]],parse(expr[2]),parse(expr[3]))
    end
  elseif expr[1] == :collatz
      if length(expr) > 2
        throw(LispError("PARSE ERROR: too many arguments for collatz"))
      else
        return CollatzNode(parse(expr[2]))
      end
  end
  throw(LispError("PARSE ERROR: Unknown operator in expression"))
end


# ========================================================
# calc functions - takes an OWL and returns the result
#

# calculate a binary operation
function calc(owl::Binop)
  lhs = calc(owl.lhs)
  rhs = calc(owl.rhs)
  if(owl.op == Function(/) && rhs == 0)
    throw(LispError("CALC ERROR: Division by Zero"))
  else
    return owl.op(calc(owl.lhs),calc(owl.rhs))
  end
end

# calculate the value of a number
function calc(owl::Num)
  return owl.n
end

function calc(owl::Unop)
  return -1 * calc(owl.arg)
end

function calc(owl::CollatzNode)
  return collatz(calc(owl.arg))
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
    return calc( ast )
end

end # module

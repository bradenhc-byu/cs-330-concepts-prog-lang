module HPInt

push!(LOAD_PATH,".")

using Lexer
using Error
using Images
using Cairo

export parse, calc, analyze, interp, interp_pp, NumVal, ClosureVal, MatrixVal

#
# =========================================================
# =========================================================
#
#  The high performance primitives you must bind in.
#
# =========================================================
# =========================================================
#

function simple_load( img_path::AbstractString )
  im = Images.load( img_path );
  tmp = Images.separate(im);
  d = Images.data( tmp );
  d = d[:,:,1];  # just the r channel
  d = convert( Array{Float32,2}, d );
  return d
end

function simple_save( output::Array, img_path::AbstractString )
    output[ output .> 1.0 ] = 1.0
    output[ output .< 0.0 ] = 0.0
    tmpc = convert( Array{UInt32,2}, floor(output*255.0) )
    tmp_output =  tmpc + tmpc*256 + tmpc*65536 + 0xff000000
    c2 = CairoImageSurface( transpose(tmp_output), Cairo.FORMAT_ARGB32 )
    write_to_png( c2, img_path )
    return 42
end

#-------------------------------------------------------------

function render_text( text_str::AbstractString, xpos, ypos )

  data = Matrix{UInt32}( 256, 256 );
  c = CairoImageSurface( data, Cairo.FORMAT_ARGB32 );
  cr = CairoContext( c );

  set_source_rgb( cr, 1., 1., 1. );
  rectangle( cr, 0., 0., 256., 256. );
  fill( cr );

  set_source_rgb( cr, 0, 0, 0 );
  select_font_face( cr, "Sans", Cairo.FONT_SLANT_NORMAL,
                    Cairo.FONT_WEIGHT_BOLD );
  set_font_size( cr, 70.0 );

  move_to( cr, xpos, ypos );
  show_text( cr, text_str );

  # tmp is an Array{UInt32,2}
  tmp = cr.surface.data;

  # grab just the blue channel, and convert the array to an array of floats
  tmp2 = convert( Array{Float32,2}, tmp & 0x000000ff ) / 255.0;
  tmp2 = convert( Array{Float32,2}, tmp2 );

  return tmp2
end

#-------------------------------------------------------------

function emboss( img::Array )
  f = [ -2. -1. 0.
        -1.  1. 1.
         0.  1. 1. ];
  f = convert( Array{Float32,2}, f );

  es = conv2( f, img );
  es = es[1:256,1:256];
  return es
end

#-------------------------------------------------------------

function drop_shadow( img::Array )
  foo = convert( Array{Float32,2}, gaussian2d(5.0,[25,25]) );
  foo = foo / maximum(foo);
  ds = conv2( foo, img );
  ds = ds[13:256+12,13:256+12];
  ds = ds / sum(foo);
  return ds
end

#-------------------------------------------------------------

# assumes img is black-on-white
function inner_shadow( img::Array )
  foo = convert( Array{Float32,2}, gaussian2d(5.0,[25,25]) );
  foo = foo / maximum(foo);
  is = conv2( foo, 1.0-img );
  is = is[8:251+12,8:251+12];
  is = is / sum(foo);
  is = max( is, img );
  return is
end

#
# ===================================================
#

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
invalid_ids = Symbol[:+,:-,:*,:/,:mod,:collatz,:if0,:with,:lambda,:min,:max]

# Abstract syntax

abstract OWL
abstract RetVal
abstract Environment

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

# Definitions for our high performance function node types

type MatrixVal <: RetVal
    n::Array{Float32,2}
end

type SimpleLoadNode <: OWL
  filename::AbstractString
end

type SimpleSaveNode <: OWL
  output::OWL
  filename::AbstractString
end

type RenderTextNode <: OWL
  text::AbstractString
  xpos::OWL
  ypos::OWL
end

type EmbossNode <: OWL
  img::OWL
end

type DropShadowNode <: OWL
  img::OWL
end

type InnerShadowNode <: OWL
  img::OWL
end

type MinNode <: OWL
  x::OWL
  y::OWL
end

type MaxNode <: OWL
  x::OWL
  y::OWL
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
    # we are looking at two possibilities: number or symbol
    # this if just handles nested parenthesis
    if isa(expr[1],Array{Any})
      return parse(expr[1])
    elseif isa(expr[1],Symbol) && !in(expr[1],invalid_ids)
      return IdNode(expr[1])
    elseif is(expr[1],Real)
      return NumNode(expr[1])
    else
      throw(LispError("PARSE ERROR: Invalid terminal type of " * string(expr[1]) * " -- must be of type Real or Symbol"))
    end

  elseif length(expr) == 2 && expr[1] == :-
    # we are looking at a unop function
    return Unop(-,parse(expr[2]))

  elseif in(expr[1],binops)
    # we are looking at a binop function
    if length(expr) > 3
      if expr[1] == :+
        # multi plus, parse all of the arguments
        args = OWL[]
        for i = 2:length(expr)
          single_arg = parse(expr[i])
          args = vcat(args,OWL[single_arg])
        end
        return MultiPlusNode(args)
      else
        # wasn't a plus operation, throw an error
        throw(LispError("PARSE ERROR: too many arguments for binop function"))
      end
    # it isn't a multip plus, continue with normal assumptions
    elseif length(expr) < 3
      throw(LispError("PARSE ERROR: too few arguments for binop function"))
    else
      return BinopNode(dict[expr[1]],parse(expr[2]),parse(expr[3]))
    end

  elseif expr[1] == :collatz
    # we are looking at a collatz function
    if length(expr) != 2
      throw(LispError("PARSE ERROR: invalid arity for (collatz)"))
    else
      return CollatzNode(parse(expr[2]))
    end

  elseif expr[1] == :if0
    # we are looking at an if0 function
    if length(expr) != 4
      throw(LispError("PARSE ERROR: invalid arity for (if0)"))
    else
      return If0Node(parse(expr[2]), parse(expr[3]), parse(expr[4]))
    end

  elseif expr[1] == :and
    # we are looking at an and function - this will
    # be converted to several if0 functions at analyze time
    if length(expr) < 3
      throw(LispError("PARSE ERROR: Not enough arguments in (and) function"))
    end
    # parse all of the arguments
    args = OWL[]
    for i = 2:length(expr)
      nextArg = parse(expr[i])
      args = vcat(args,OWL[nextArg])
    end
    return AndNode(args)

  elseif expr[1] == :with
    # we are looking at a with function - we'll need to parse
    # all of the bindings individually
    if length(expr) != 3
      throw(LispError("PARSE ERROR: invalid arity for (with)"))
    else
      symbols = Symbol[]
      b_exprs = OWL[]
      for i = 1:length(expr[2])
        if !isa(expr[2][i],Array{Any})
          throw(LispError("PARSE ERROR: second argument of (with) not an array"))
        elseif length(expr[2][i]) != 2
          throw(LispError("PARSE ERROR: Invalid binding in (with) - incorrect arity"))
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
    # the user is defining a function - set up the FunDefNode
    if length(expr) != 3
      throw(LispError("PARSE ERROR: invalid arity for (lambda)"))
    elseif !isa(expr[2],Array{Any})
      throw(LispError("PARSE ERROR: parameters for (lambda) must be given in a list"))
    end
    # the following if handles the case where the function
    # has no arguments
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

  # =================================================
  # HIGH PERFORMANCE FUNCTIONALITY PARSING

  elseif expr[1] == :simple_load
    if length(expr) != 2
      throw(LispError("PARSE ERROR: invalid arity for (simple_load) - Usage: (simple_load <string>)"))
    elseif !isa(expr[2],AbstractString)
      throw(LispError("PARSE ERROR: argument of (simple_load) must be a string"))
    end
    return SimpleLoadNode(expr[2])

  elseif expr[1] == :simple_save
    if length(expr) != 3
      throw(LispError("PARSE ERROR: invalid arity for (simple_save) - Usage: (simple_save <OWL> <string>)"))
    elseif !isa(expr[3],AbstractString)
      throw(LispError("PARSE ERROR: second argument of (simple_save) must be a string"))
    end
    return SimpleSaveNode(parse(expr[2]),expr[3])

  elseif expr[1] == :render_text
    if length(expr) != 4
      throw(LispError("PARSE ERROR: invalid arity for (render_text) = Usage: (render_text <string> <OWL> <OWL>))"))
    elseif !isa(expr[2],AbstractString)
      throw(LispError("PARSE ERROR: first argument of (render_text_ must be a string)"))
    end
    return RenderTextNode(expr[2],parse(expr[3]),parse(expr[4]))

  elseif expr[1] == :emboss
    if length(expr) != 2
      throw(LispError("PARSE ERROR: invalid arity for (emboss) - Usage: (emboss <OWL>)"))
    end
    return EmbossNode(parse(expr[2]))

  elseif expr[1] == :drop_shadow
    if length(expr) != 2
      throw(LispError("PARSE ERROR: invalid arity for (drop_shadow) - Usage: (drop_shadow <OWL>)"))
    end
    return DropShadowNode(parse(expr[2]))

  elseif expr[1] == :inner_shadow
    if length(expr) != 2
      throw(LispError("PARSE ERROR: invalid arity for (inner_shadow) - Usage: (inner_shadow <OWL>)"))
    end
    return InnerShadowNode(parse(expr[2]))

  elseif expr[1] == :min
    if length(expr) != 3
      throw(LispError("PARSE ERROR: invalid arity for (min) - Usage: (min <OWL> <OWL>)"))
    end
    return MinNode(parse(expr[2]),parse(expr[3]))

  elseif expr[1] == :max
    if length(expr) != 3
      throw(LispError("PARSE ERROR: invalid arity for (max) - Usage: (max <OWL> <OWL>)"))
    end
    return MaxNode(parse(expr[2]),parse(expr[3]))

  # END OF HIGH PERFORMANCE INTERP FUNCTION PARSING
  # =======================================================

  else
    # the user is calling a function that they defined
    # we need to parse all of the arguments they pass to it
    args = OWL[]
    for i = 2:(length(expr))
      parg = parse(expr[i])
      args = vcat(OWL[parg],args)
    end
    return FunAppNode(parse(expr[1]),args)

  end
  # THE CODE SHOULD NEVER REACH THIS....
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

function analyze(ast::Unop)
  return Unop(ast.op,analyze(ast.arg))
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
  for i = 1:length(ast.binding_exprs)
    ast.binding_exprs[i] = analyze(ast.binding_exprs[i])
  end
  fdn = FunDefNode(ast.variables, closure_body)
  return FunAppNode(fdn, ast.binding_exprs)
end

function analyze(ast::FunDefNode)
  return FunDefNode(ast.formal_parameters, analyze(ast.fun_body))
end

function analyze(ast::FunAppNode)
  for i = 1:length(ast.arg_exprs)
    ast.arg_exprs[i] = analyze(ast.arg_exprs[i])
  end
  return FunAppNode(analyze(ast.fun_expr), ast.arg_exprs)
end

# HIGH PERFORMANCE ANALYZE FUNCTIONS
function analyze(ast::SimpleLoadNode)
  return ast
end

function analyze(ast::SimpleSaveNode)
  return SimpleSaveNode(analyze(ast.output),ast.filename)
end

function analyze(ast::RenderTextNode)
  return RenderTextNode(ast.text, analyze(ast.xpos), analyze(ast.ypos))
end

function analyze(ast::EmbossNode)
  return EmbossNode(analyze(ast.img))
end

function analyze(ast::DropShadowNode)
  return DropShadowNode(analyze(ast.img))
end

function analyze(ast::InnerShadowNode)
  return InnerShadowNode(analyze(ast.img))
end

function analyze(ast::MinNode)
  return MinNode(analyze(ast.x),analyze(ast.y))
end

function analyze(ast::MaxNode)
  return MaxNode(analyze(ast.x),analyze(ast.y))
end

# ========================================================
# calc functions - takes an OWL and returns the result
#

# initializing calc function to create an empty envioronment
# used by the TAs in autograding
function calc(owl::OWL)
  return calc(owl,mtEnv())
end

# calculate a binary operation
function calc(owl::BinopNode, env::Environment)
  slhs = @spawn calc(owl.lhs, env)
  srhs = @spawn calc(owl.rhs, env)
  lhs = fetch(slhs)
  rhs = fetch(srhs)
  if owl.op == Function(/) && isa(rhs,NumNode) && rhs.n == 0
    throw(LispError("CALC ERROR: Division by Zero"))
  end
  if isa(lhs,NumVal) && isa(rhs,NumVal)
    return NumVal( owl.op( lhs.n, rhs.n) )
  elseif (isa(lhs,MatrixVal) && isa(rhs,NumVal)) || (isa(rhs,MatrixVal) && isa(lhs,NumVal)) || (isa(rhs,MatrixVal) && isa(lhs,MatrixVal))
    if owl.op == Function(*)
      return MatrixVal(lhs.n .* rhs.n)
    elseif owl.op == Function(/)
      return MatrixVal(lhs.n ./ rhs.n)
    elseif owl.op == Function(+)
      return MatrixVal(lhs.n + rhs.n)
    elseif owl.op == Function(-)
      return MatrixVal(lhs.n - rhs.n)
    else
      throw(LispError("CALC ERROR: invalid matrix operation"))
    end
  else
    throw(LispError("TYPE ERROR: cannot perform binop operation on element not of type NumVal or MatrixVal"))
  end
end

# calculate the value of a number
function calc(owl::NumNode, env::Environment)
  return NumVal(owl.n)
end

function calc(owl::Unop, env::Environment)
  result = calc(owl.arg, env)
  if isa(result,NumVal)
    return NumVal(-(result.n))
  elseif isa(result,MatrixVal)
    return MatrixVal(-1 .* result.n)
  else
    throw(LispError("CALC ERROR: atempt to apply unary '-' to something other than a number or matrix"))
  end
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
  # first let's spawn off all the bindings
  spawns = RemoteRef{Channel{Any}}[]
  for i = 1:length(owl.variables)
    spawn = @spawn calc(owl.binding_exprs[i],env)
    spawns = vcat(spawns,RemoteRef{Channel{Any}}[spawn])
  end
  # now let's fetch all of the spawned processes
  for i = 1:length(owl.variables)
    val = fetch(spawns[i])
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
  # first let's spawn off all the arguments for calc
  spawns = RemoteRef{Channel{Any}}[]
  for i = 1:length(closure_val.params)
    spawn = @spawn calc(owl.arg_exprs[i],env)
    spawns = vcat(spawns,RemoteRef{Channel{Any}}[spawn])
  end
  # now we need to fetch the spawned processes
  ext_env = env
  for i = 1:length(closure_val.params)
    actual_param = fetch(spawns[i])
    ext_env = CEnvironment(closure_val.params[i], actual_param, ext_env)
  end
  return calc(closure_val.body, ext_env)
end

#
# HIGH PERFORMANCE INTERPRETER CALC FUNCTIONS
#
function calc(owl::SimpleLoadNode, env::Environment)
  img = simple_load(owl.filename)
  return MatrixVal(img)
end

function calc(owl::SimpleSaveNode, env::Environment)
  img = calc(owl.output,env)
  if !isa(img,MatrixVal)
    throw(LispError("CALC ERROR: first argument for (simple_save) must evaluate to a MatrixVal"))
  end
  ret = simple_save(img.n,owl.filename)
  return NumVal(ret)
end

function calc(owl::RenderTextNode, env::Environment)
  sxpos = @spawn calc(owl.xpos, env)
  sypos = @spawn calc(owl.ypos, env)
  xpos = fetch(sxpos)
  ypos = fetch(sypos)
  if !isa(xpos,NumVal)
    throw(LispError("CALC ERROR: x position argument for (render_text) must evaluate to a NumVal"))
  elseif !isa(ypos,NumVal)
    throw(LispError("CALC ERROR: y position argument for (render_text) must evaluate to a NumVal"))
  end
  ret = render_text(owl.text,xpos.n,ypos.n)
  return MatrixVal(ret)
end

function calc(owl::EmbossNode, env::Environment)
  img = calc(owl.img, env)
  if !isa(img,MatrixVal)
    throw(LispError("CALC ERROR: first argument of (emboss) must evaluate to a MatrixVal"))
  end
  ret = emboss(img.n)
  return MatrixVal(ret)
end

function calc(owl::DropShadowNode, env::Environment)
  img = calc(owl.img, env)
  if !isa(img, MatrixVal)
    throw(LispError("CALC ERROR: first argument of (drop_shadow) must evaluate to a MatrixVal"))
  end
  ret = drop_shadow(img.n)
  return MatrixVal(ret)
end

function calc(owl::InnerShadowNode, env::Environment)
  img = calc(owl.img, env)
  if !isa(img, MatrixVal)
    throw(LispError("CALC ERROR: first argument of (inner_shadow) must evaluate to a MatrixVal"))
  end
  ret = inner_shadow(img.n)
  return MatrixVal(ret)
end

function calc(owl::MinNode, env::Environment)
  slhs = @spawn calc(owl.x, env)
  srhs = @spawn calc(owl.y, env)
  lhs = fetch(slhs)
  rhs = fetch(srhs)
  if !isa(lhs, NumVal) && !isa(lhs, MatrixVal)
    throw(LispError("CALC ERROR: first argument in (min) function must evaluate to either a NumVal or a MatrixVal"))
  elseif !isa(rhs, NumVal) && !isa(rhs, MatrixVal)
    throw(LispError("CALC ERROR: second argument in (min) function must evaluate to either a NumVal or a MatrixVal"))
  end
  ret = min(lhs.n,rhs.n)
  if isa(ret,Real)
    return NumVal(ret)
  else
    return MatrixVal(ret)
  end
end

function calc(owl::MaxNode, env::Environment)
  slhs = @spawn calc(owl.x, env)
  srhs = @spawn calc(owl.y, env)
  lhs = fetch(slhs)
  rhs = fetch(srhs)
  if !isa(lhs, NumVal) && !isa(lhs, MatrixVal)
    throw(LispError("CALC ERROR: first argument in (max) function must evaluate to either a NumVal or a MatrixVal"))
  elseif !isa(rhs, NumVal) && !isa(rhs, MatrixVal)
    throw(LispError("CALC ERROR: second argument in (max) function must evaluate to either a NumVal or a MatrixVal"))
  end
  ret = max(lhs.n,rhs.n)
  if isa(ret,Real)
    return NumVal(ret)
  else
    return MatrixVal(ret)
  end
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

function pp( ast::SimpleLoadNode, depth::Int)
  print("(simple_load \"", ast.filename, "\")")
end

function pp( ast::SimpleSaveNode, depth::Int )
  print("(simple_save ")
  pp( ast.output, depth+1 )
  print(" \"", ast.filename, "\")")
end

function pp( ast::RenderTextNode, depth::Int )
  print("(render_text \"", ast.text, "\" ")
  pp( ast.xpos, depth+1 )
  print(" ")
  pp( ast.ypos, depth+1 )
  print(")")
end

function pp( ast::EmbossNode, depth::Int )
  print("(emboss ")
  pp( ast.img, depth+1 )
  print(")")
end

function pp( ast::DropShadowNode, depth::Int )
  print("(drop_shadow ")
  pp( ast.img, depth+1 )
  print(")")
end

function pp( ast::InnerShadowNode, depth::Int )
  print("(inner_shadow ")
  pp( ast.img, depth+1 )
  print(")")
end

function pp( ast::MinNode, depth::Int )
  print("(min ")
  pp( ast.x, depth+1 )
  pp( ast.y, depth+1 )
  print(")")
end

function pp( ast::MaxNode, depth::Int )
  print("(max ")
  pp( ast.x, depth+1 )
  pp( ast.y, depth+1 )
  print(")")
end





end # module

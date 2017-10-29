abstract Shape

# type definitions for various shapes
type Position
  x::Real
  y::Real
end

type Circ <: Shape
  center::Position
  radius::Real
end

type Square <: Shape
  upper_left::Position
  length::Real
end

type Rect <: Shape
  upper_left::Position
  width::Real
  height::Real
end

# type definition of a color pixel
type Pixel
  r::Real
  g::Real
  b::Real
end

abstract TreeItem

type Person <: TreeItem
  name::AbstractString
  birthyear::Integer
  eyecolor::Symbol
  father::TreeItem
  mother::TreeItem
end

type Unknown <: TreeItem
end

# determines the area of a circle
function area(shape::Circ)
  return pi * (shape.radius)^2
end

# determines the area of a square
function area(shape::Square)
  return shape.length^2
end

# determines the area of a rectangle
function area(shape::Rect)
  return shape.width * shape.height
end

# determines if the given coordinates are inside the circle
function in_shape(shape::Circ, position::Position)
  inX = (shape.center.x + shape.radius) >= position.x && (shape.center.x - shape.radius) <= position.x
  inY = (shape.center.y + shape.radius) >= position.y && (shape.center.y - shape.radius) <= position.y
  return inX && inY
end

# determines if the given coordinates are inside the square
function in_shape(shape::Square, position::Position)
  inX = (shape.upper_left.x + shape.length) >= position.x
  inY = (shape.upper_left.y - shape.length) <= position.y
  return inX && inY
end

# determines if the given coordinates are inside the rectangle
function in_shape(shape::Rect, position::Position)
  inX = (shape.upper_left.x + shape.width) >= position.x
  inY = (shape.upper_left.y - shape.height) <= position.y
  return inX && inY
end

# greyscales all pixels in a two-dimensional array of Pixels
function greyscale(picture::Array{Pixel,2})
  return map(average_pixel,picture)
end

function average_pixel(pixel::Pixel)
  avg = (pixel.r + pixel.g + pixel.b) / 3
  return Pixel(avg,avg,avg)
end

# inverts the pixels in a two-dimension array of Pixels
function invert(picture::Array{Pixel,2})
  return map(invert_pixel,picture)
end

function invert_pixel(pixel::Pixel)
  return Pixel(255 - pixel.r, 255 - pixel.g, 255 - pixel.b)
end

# counts the number of people in the tree
function count_persons(tree::TreeItem)
  if(isa(tree,Unknown))
    return 0
  else
    return 1 + count_persons(tree.mother) + count_persons(tree.father)
  end
end

# returns the average age of the people in the tree
function average_age(tree::TreeItem)
  return total_age(tree) / count_persons(tree)
end

function total_age(tree::TreeItem)
  if(isa(tree,Unknown))
    return 0
  else
    age = total_age(tree.mother) + total_age(tree.father)
    return (2016 - tree.birthyear) + age
  end
end

# maps a function f to each known member of a tree. returns
# a new tree so that the original is unchanged, unless f applies
# a mutation
function tree_map(f::Function, tree::TreeItem)
  if(isa(tree,Unknown))
    return Unknown()
  end
  newTree = f(tree)
  newTree.father = tree_map(f, newTree.father)
  newTree.mother = tree_map(f, newTree.mother)
  return newTree
end


# adds the parameter "name" to the end of the name without
# adding a space
function add_last_name(name::AbstractString, tree::Person)
  return tree_map((x -> Person(x.name * name,x.birthyear,x.eyecolor,x.father,x.mother)),tree)
end


# returns a list of all eye colors in the tree. A color can appear
# more than once
function eye_colors(tree::TreeItem)
  if(isa(tree,Unknown))
    return Any[]
  else
    thisColor = Any[tree.eyecolor]
    eyecolors = vcat(eye_colors(tree.father), eye_colors(tree.mother))
    if(!in(tree.eyecolor,eyecolors))
      return vcat(thisColor, eyecolors)
    else
      return eyecolors
    end
  end
end

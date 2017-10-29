defmodule Qsort do
  def qsort([]) do [] end
  def qsort([pivot|rest]) do
    smaller = for n <- rest, n < pivot do n end
    larger = for n <- rest, n >= pivot do n end
    qsort(smaller) ++ [pivot] ++ qsort(larger)
  end
end

# qucksort with Enum
defmodule SimpleQsort do
  def qsort([]) do [] end
  def qsort([pivot|rest]) do
    {smaller, larger} = Enum.split_with(rest,fn(x) -> x < pivot)
    qsort(smaller) ++ [pivot] ++ qsort(larger)
  end
end

# construct anonymous functions using fn(x) -> ...

defmodule Elixir_Intro do

  # Fibonacci number function
  def fib(1) do 1 end
  def fib(2) do 1 end
  def fib(n) do
    fib(n - 1) + fib(n - 2)
  end

  # Function to calculate the area of rectangles, squares,
  # circles, and triangles
  def area(shape, shape_info) do
    case shape do
      :rectangle ->
        {length, height} = shape_info
        length * height
      :square ->
        {side_length} = shape_info
        :math.pow(side_length,2)
      :circle ->
        {radians} = shape_info
        :math.pi * (:math.pow(radians,2))
      :triangle ->
        {base, height} = shape_info
        0.5 * base * height
    end
  end

  #Function to return a new list where each item of the list
  # (which is a number) is squared
  def sqrList(nums) do
    for n <- nums, do: :math.pow(n,2)
  end

  # Given an inventory of the form {item, quantity, price},
  # returns a list of the form {item, total_price}
  def calcTotals(inventory) do
    for {item, quantity, price} <- inventory, do: {item, (quantity * price)}
  end

  # Maps a function to every item in the list
  def map(function, vals) do
    for n <- vals, do: function. (n)
  end

  # starts a simple server that will receive a list from
  # a client and send a quicksorted list back to the client
  def quickSortServer() do
    receive do
      {list, pid} ->
        sorted = qsort(list)
        send(pid,{sorted, self()})
    end
  end

  def qsort([]) do [] end
  def qsort(list) do
    length = :lists.flatlength(list)
    pos = :rand.uniform(length)
    pivot = :lists.nth(pos,list)
    smaller = for n <- list, n < pivot do n end
    larger = for n <- list, n > pivot do n end
    qsort(smaller) ++ [pivot] ++ qsort(larger)
  end
end

defmodule Client do
  def callServer(pid,nums) do
    send(pid, {nums, self()})
    listen()
  end

  def listen() do
    receive do
      {sorted, pid} -> sorted
    end
  end
end

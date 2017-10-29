defmodule NameServer do
  use GenServer

  # Start Helper Functions (Don't Modify)
  def start_link() do
    GenServer.start_link(__MODULE__, [], [])
  end

  def start() do
    GenServer.start(__MODULE__, [],  [])
  end

  def register(name_server, name) do
    GenServer.call(name_server, {:register, name})
  end

  def register(name_server, name, pid) do
    GenServer.cast(name_server, {:register, name, pid})
  end

  def resolve(name_server, name) do
    GenServer.call(name_server, {:resolve, name})
  end
  #End Helper Functions

  def init(_) do
    IO.puts("Initializing name server...")
    # initialize data structure
    registry = %{name_server: self()}
    {:ok,registry}
  end

  def handle_call({:register,name},{pid,_from},registry) do
    registry = Map.put(registry,name,pid)
    {:reply,:ok,registry}
  end

  def handle_call({:resolve,name},{pid,_from},registry) do
    case Map.get(registry,name) do
      nil ->
        {:reply,:error,registry}
      name_pid ->
        {:reply,name_pid,registry}
    end
  end

  def handle_cast({:register,name,pid},registry) do
    registry = Map.put(registry,name,pid)
    {:noreply,registry}
  end

  # handle any other calls and casts
  def handle_call(request, from, state) do
    super(request, from, state)
  end

  def handle_cast(request, state) do
    super(request, state)
  end
end

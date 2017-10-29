defmodule TopSupervisor do
  use Supervisor

  def start_link(ns) do
    Supervisor.start_link(__MODULE__, ns )
  end

  def init(ns) do
    children = [
      supervisor(CustomerServiceSupervisor, [ns]),
      supervisor(DatabaseSupervisor,[ns])
    ]

    supervise(children, strategy: :rest_for_one)
  end
end

defmodule CustomerServiceSupervisor do
  use Supervisor

  @name CustomerServiceSupervisor

  def start_link(ns) do
    Supervisor.start_link(__MODULE__,ns, name: @name)
  end

  def init(ns) do
    children = [
      worker(CustomerService,[ns])
    ]

    supervise(children, strategy: :one_for_one)
  end
end

defmodule DatabaseSupervisor do
  use Supervisor

  @name DatabaseSupervisor

  def start_link(ns) do
    Supervisor.start_link(__MODULE__,ns, name: @name)
  end

  def init(ns) do
    children = [
      worker(Database,[ns]),
      supervisor(DatabaseChildSupervisor,[ns])
    ]

    supervise(children, strategy: :rest_for_one)
  end
end

defmodule DatabaseChildSupervisor do
  use Supervisor

  @name DatabaseChildSupervisor

  def start_link(ns) do
    Supervisor.start_link(__MODULE__,ns, name: @name)
  end

  def init(ns) do
    children = [
      worker(Info,[ns]),
      worker(Shipper,[ns]),
      supervisor(UserOrderSupervisor,[ns])
    ]

    supervise(children, strategy: :rest_for_one)
  end
end



defmodule UserOrderSupervisor do
  use Supervisor

  @name UserOrderSupervisor

  def start_link(ns) do
    Supervisor.start_link(__MODULE__,ns, name: @name)
  end

  def init(ns) do
    children = [
      worker(User,[ns]),
      worker(Order,[ns])
    ]

    supervise(children, strategy: :one_for_all)
  end
end

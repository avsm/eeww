include Stdune
module Console = Dune_console
module Dune_rpc = Dune_rpc_private

include struct
  open Dune_engine
  module Build_system = Build_system
  module Scheduler = Scheduler
end

include struct
  open Dune_rpc_client
  module Where = Where
  module Client = Dune_rpc_client.Client
end

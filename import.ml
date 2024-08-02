include struct
  open Incr_dom
  module Component = Component
  module Incr = Incr
  module Start_app = Start_app
end

include struct
  open Virtual_dom
  module Vdom = Vdom
end

include struct
  include Ocaml_shadow
end

include struct
  open Ppxlib_ast
  module Ast = Ast
  module Parse = Parse
  module Pprintast = Pprintast
end

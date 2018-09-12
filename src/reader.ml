open Stream
open Unitlex
open Types

module type READER = sig
  val analex : char stream -> unitlex stream
  val parse : unitlex stream ->
    sort list * sort list * sort list * constructor_signature list *
    filter_signature list * string list * rule list
end
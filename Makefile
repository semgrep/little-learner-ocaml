all:
	dune build

utop:
	dune utop

# To load lib/ code from ocaml toplevel, in Emacs run M-x run-ocaml and
# enter 'ocaml', but first put in your ~/.ocamlinit this:
#   #use_output "dune ocaml top";;
#   open Tensor;;
#   open Learning;;
top:
	dune ocaml top

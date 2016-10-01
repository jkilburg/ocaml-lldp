OCAMLC = ocamlc
CC = ocamlc
OCAMLMKLIB = ocamlmklib
GCC = gcc
OCAML = ocaml

all: lldpd lldp_test

clean:
	rm -f *.o *.cmo *.cma *.a *.so a.out *.cmx *.cmi *.cmxa lldptool lldpd *~

lldptool: lldp.ml lldptool.ml Makefile
	ocamlfind ocamlopt -thread -g -o $@ -linkpkg -package core -package async_kernel -package async lldptool.ml lldp.ml

packet.cmx: packet.ml Makefile
	ocamlfind ocamlopt -thread -g -c -package core $(<)i $<

netdevice.cmx: netdevice.ml Makefile
	ocamlfind ocamlopt -thread -g -c -package core $(<)i $<

socket.cmx: socket.ml Makefile
	ocamlfind ocamlopt -thread -g -c -package core $(<)i $<

lldp.cmx: lldp.ml lldp.mli Makefile
	ocamlfind ocamlopt -thread -g -c -package ppx_jane -package core $(<)i $<

liblldp.a: packet_stub.o netdevice_stub.o socket_stub.o netdevice.cmx packet.cmx socket.cmx Makefile
	$(OCAMLMKLIB) -custom -oc lldp packet_stub.o netdevice_stub.o socket_stub.o

CMX_FILES=packet.cmx netdevice.cmx socket.cmx lldp.cmx

lldpd: $(CMX_FILES) lldpd.ml lldpd.mli liblldp.a Makefile
	ocamlfind ocamlopt -thread -g -o $@ -linkpkg -package ppx_jane -package core -package async liblldp.a $(CMX_FILES) lldpd.mli lldpd.ml

lldp_test: $(CMX_FILES) lldp_test.ml lldp_test.mli liblldp.a Makefile
	ocamlfind ocamlopt -thread -g -o $@ -linkpkg -package core -package async_kernel -package async liblldp.a $(CMX_FILES) lldp_test.mli lldp_test.ml

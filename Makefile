OCAMLC = ocamlc
CC = ocamlc
OCAMLMKLIB = ocamlmklib
GCC = gcc
OCAML = ocaml

all: lldpd lldp_test

clean:
	rm -f *.o *.cmo *.cma *.a *.so a.out *.cmx *.cmi *.cmxa lldptool lldpd *~

packet.cmx: packet.ml packet.mli Makefile
	ocamlfind ocamlopt -thread -g -c -package core $(<)i $<

netdevice.cmx: netdevice.ml netdevice.mli Makefile
	ocamlfind ocamlopt -thread -g -c -package core $(<)i $<

socket.cmx: socket.ml socket.mli Makefile
	ocamlfind ocamlopt -thread -g -c -package core $(<)i $<

lldp.cmx: lldp.ml lldp.mli Makefile
	ocamlfind ocamlopt -thread -g -c -package ppx_jane -package core $(<)i $<

site_server.cmx: site_server.ml site_server.mli Makefile
	ocamlfind ocamlopt -thread -g -c -package ppx_jane -package core -package async $(<)i $<

database.cmx: database.ml database.mli Makefile
	ocamlfind ocamlopt -thread -g -c -package ppx_jane -package core -package async $(<)i $<

interface.cmx: interface.ml interface.mli Makefile
	ocamlfind ocamlopt -thread -g -c -package ppx_jane -package core -package async $(<)i $<

libnetstubs.a: packet_stub.o netdevice_stub.o socket_stub.o Makefile
	$(OCAMLMKLIB) -custom -oc netstubs packet_stub.o netdevice_stub.o socket_stub.o

CMX_FILES=packet.cmx netdevice.cmx socket.cmx lldp.cmx site_server.cmx database.cmx interface.cmx

lldpd: $(CMX_FILES) lldpd.ml lldpd.mli libnetstubs.a
	ocamlfind ocamlopt -thread -g -o $@ -linkpkg -package ppx_jane -package core -package async libnetstubs.a $(CMX_FILES) lldpd.mli lldpd.ml

lldp_test: $(CMX_FILES) lldp_test.ml lldp_test.mli libnetstubs.a Makefile
	ocamlfind ocamlopt -thread -g -o $@ -linkpkg -package core -package async_kernel -package async libnetstubs.a $(CMX_FILES) lldp_test.mli lldp_test.ml

OCAMLC = ocamlc
CC = ocamlc
OCAMLMKLIB = ocamlmklib
GCC = gcc
OCAML = ocaml

all: lldpd

clean:
	rm -f *.o *.cmo *.cma *.a *.so a.out *.cmx *.cmi *.cmxa lldptool lldpd *~

lldptool: lldp.ml lldptool.ml Makefile
	ocamlfind ocamlopt -thread -g -o $@ -linkpkg -package core -package async_kernel -package async lldptool.ml lldp.ml

packet.cmx: packet.ml Makefile
	ocamlfind ocamlopt -thread -g -c -package core $<

netdevice.cmx: netdevice.ml Makefile
	ocamlfind ocamlopt -thread -g -c -package core $<

socket.cmx: socket.ml Makefile
	ocamlfind ocamlopt -thread -g -c -package core $<

liblldp.a: packet_intf.o netdevice_intf.o socket_intf.o netdevice.cmx packet.cmx socket.cmx Makefile
	$(OCAMLMKLIB) -custom -oc lldp packet_intf.o netdevice_intf.o netdevice.cmx packet.cmx socket.cmx

lldpd: lldp.ml lldpd.ml liblldp.a Makefile
	ocamlfind ocamlopt -thread -g -o $@ -linkpkg -syntax camlp4o -package sexplib.syntax -package comparelib.syntax -package core -package async_kernel -package async lldp.ml lldpd.ml liblldp.a

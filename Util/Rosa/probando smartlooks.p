def new shared var input-var-1 as char.
def new shared var input-var-2 as char.
def new shared var input-var-3 as char.
def new shared var output-var-1 as rowid.
def new shared var output-var-2 as char.
def new shared var output-var-3 as char.
def new shared var s-codcia AS INT.

assign
s-codcia = 001
input-var-1 = "000"
input-var-2 = ""
input-var-3 = "".

/*run util/rosa/c-catsvc ("Servicios").*/
/*run util/rosa/c-Marcas ("Marcas").*/
/*run util/rosa/c-Familia ("Marcas").*/
run util/rosa/c-SubFam ("SubFamilia").
/*run util/rosa/c-Proveedores ("Proveedores").*/

if output-var-1 = ? then do:
   message "Cancelo operacion" view-as alert-box.
end.
else
message string (output-var-1) skip
        output-var-2 skip
        output-var-3 
        view-as alert-box.

/* solo para I-03 */
disable triggers for load of almcmov.
disable triggers for load of almdmov.

def var s-codcia as int init 001.
def var x-linea as char format 'x(100)'.
def var x-llave as char.
def var x-fchdoc as date.
def temp-table b-cmov like almcmov.
def temp-table b-dmov like almdmov.
def temp-table t-cmov like almcmov.
def temp-table t-dmov like almdmov.
def var s-user-id as char init 'ENE092006'.
def temp-table t-matg like almmmatg.

x-fchdoc = date(01,31,2006).    /* fecha de corte */
input from c:\tmp\ienero2006.prn.
repeat:
    import unformatted x-linea.
    find almdmov where almdmov.codcia = s-codcia
        and almdmov.codalm = substring(x-linea,25,3)
        and almdmov.tipmov = substring(x-linea,39,1)
        and almdmov.codmov = integer(substring(x-linea,42,2))
        and almdmov.nroser = integer(substring(x-linea,46,3))
        and almdmov.nrodoc = integer(substring(x-linea,52,6))
        and almdmov.codmat = substring(x-linea,1,6)
        no-lock no-error.
    if not available almdmov then next.
    create b-dmov.
    buffer-copy almdmov to b-dmov
        assign b-dmov.fchdoc = x-fchdoc.
    find first almcmov of almdmov no-lock no-error.
    if available almcmov then do:
        find b-cmov of almcmov no-lock no-error.
        if not available b-cmov then do:
            create b-cmov.
            buffer-copy almcmov to b-cmov
                assign b-cmov.fchdoc = x-fchdoc
                       b-cmov.nroref = substring(x-linea,81,15).
        end.
    end.        
end.
input close.

for each b-cmov, 
        each b-dmov of b-cmov by b-dmov.codmat by b-dmov.fchdoc:
    display b-dmov.codmat b-cmov.fchdoc b-cmov.codalm b-cmov.almdes
        b-cmov.tipmov b-cmov.codmov b-cmov.nroser b-cmov.nrodoc
        b-dmov.candes with stream-io no-box no-labels.
end.
message 'continuamos?' view-as alert-box question
    buttons yes-no update rpta as log.
if rpta = no then return.
    
for each b-cmov:
    /* salida 67 */
    create t-cmov.
    buffer-copy b-cmov to t-cmov
        assign
            t-cmov.codalm = b-cmov.almdes
            t-cmov.tipmov = 'S'
            t-cmov.codmov = 67
            t-cmov.usuario = s-user-id.
    for each b-dmov of b-cmov no-lock:
        create t-dmov.
        buffer-copy b-dmov to t-dmov
            assign
                t-dmov.codalm = t-cmov.codalm
                t-dmov.tipmov = t-cmov.tipmov
                t-dmov.codmov = t-cmov.codmov.
    end.            
    /* ingreso 66 */
    create t-cmov.
    buffer-copy b-cmov to t-cmov
        assign
            t-cmov.codalm = b-cmov.almdes
            t-cmov.tipmov = 'I'
            t-cmov.codmov = 66
            t-cmov.usuario = s-user-id.
    for each b-dmov of b-cmov no-lock:
        create t-dmov.
        buffer-copy b-dmov to t-dmov
            assign
                t-dmov.codalm = t-cmov.codalm
                t-dmov.tipmov = t-cmov.tipmov
                t-dmov.codmov = t-cmov.codmov.
    end.            
end.

for each t-cmov:
    find almcmov of t-cmov no-lock no-error.
    if not available almcmov then do:
        create almcmov.
        buffer-copy t-cmov to almcmov.
    end.        
    for each t-dmov of t-cmov:
        create almdmov.
        buffer-copy t-dmov to almdmov.
        find first t-matg of t-dmov no-lock no-error.
        if not available t-matg then do:
            create t-matg.
            buffer-copy t-dmov to t-matg.
        end.
    end.
end.

output to c:\tmp\materiales.txt.
for each t-matg:
display t-matg.codmat with stream-io no-labels no-box.
end.
output close.


disable triggers for load of almcmov.
disable triggers for load of almdmov.

def temp-table t-cmov like almcmov.
def temp-table t-dmov like almdmov.

def var x-linea as char format 'x(200)'.
def var x-codalm as char.
def var x-codmat as char.
def var x-stkact-1 as dec.
def var x-stkact-2 as dec.
def var x-candes as dec.
def var x-fchdoc as date.
def var x-nroser as int init 000.
def var x-nrodoc as int init 000706.

x-fchdoc = 07/01/2006.

input from c:\tmp\diferencias.txt.
repeat:
    import unformatted x-linea.
    assign
        x-codalm = substring(x-linea,1,2)
        x-codmat = substring(x-linea,10,6)
        x-stkact-1 = dec(substring(x-linea,20,15))
        x-stkact-2 = dec(substring(x-linea,40,15)).
    if x-codmat <> '' and x-stkact-1 > x-stkact-2
    then do:
        create t-dmov.
        assign
            t-dmov.codcia = 001
            t-dmov.codalm = x-codalm
            t-dmov.tipmov = 'S'
            t-dmov.codmov = 99
            t-dmov.nroser = x-nroser
            t-dmov.nrodoc = x-nrodoc
            t-dmov.fchdoc = x-fchdoc
            t-dmov.codmat = x-codmat
            t-dmov.candes = absolute(x-stkact-1 - x-stkact-2)
            t-dmov.factor = 1.
    end.
    if x-codmat <> '' and x-stkact-1 < x-stkact-2
    then do:
        create t-dmov.
        assign
            t-dmov.codcia = 001
            t-dmov.codalm = x-codalm
            t-dmov.tipmov = 'I'
            t-dmov.codmov = 99
            t-dmov.nroser = x-nroser
            t-dmov.nrodoc = x-nrodoc
            t-dmov.codmat = x-codmat
            t-dmov.fchdoc = x-fchdoc
            t-dmov.candes = absolute(x-stkact-1 - x-stkact-2)
            t-dmov.factor = 1.
    end.
end.
input close.

for each t-dmov:
    create almdmov.
    buffer-copy t-dmov to almdmov.
    find first almcmov of almdmov no-lock no-error.
    if not available almcmov then do:
        create almcmov.
        buffer-copy almdmov to almcmov
            assign almcmov.usuario = 'ADMIN'.
    end.
    delete t-dmov.
end.

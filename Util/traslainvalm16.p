def var s-codcia as int init 001.
def var s-codalm as char init '16a'.
def var s-fchinv as date.

def var s-user-id as char init 'Sistemas'.

s-fchinv = date(04,28,2005).

def temp-table t-matg
    field codmat like almmmatg.codmat
    field caninv like invconteo.caninv.
def var x-linea as char format 'x(100)'.    
    
input from c:\tmp\inv16a.prn.
repeat:
    import unformatted x-linea.
    create t-matg.
    assign
        t-matg.codmat = substring(x-linea,1,6)
        t-matg.caninv = decimal(substring(x-linea,7,26)).
end.
input close.
output to c:\tmp\inv16arepetido.txt.
for each t-matg where t-matg.codmat <> '':
    find first invconteo where invconteo.codcia = s-codcia
        and invconteo.codalm = s-codalm
        and invconteo.fchinv = s-fchinv
        and invconteo.codmat = t-matg.codmat
        exclusive-lock no-error.
    if available invconteo
    then display t-matg.codmat t-matg.caninv.
    else create invconteo.
    assign
        invconteo.codcia = s-codcia
        invconteo.codalm = s-codalm
        invconteo.fchinv = s-fchinv
        invconteo.responsable = s-user-id
        invconteo.codmat = t-matg.codmat
        invconteo.caninv = t-matg.caninv.
    find first invrecont where invrecont.codcia = s-codcia
        and invrecont.codalm = s-codalm
        and invrecont.fchinv = s-fchinv
        and invrecont.codmat = t-matg.codmat
        exclusive-lock no-error.
    if not available invrecont
    then create invrecont.
    assign
        invrecont.codcia = s-codcia
        invrecont.codalm = s-codalm
        invrecont.fchinv = s-fchinv
        invrecont.responsable = s-user-id
        invrecont.codmat = t-matg.codmat
        invrecont.caninv = t-matg.caninv.
end.
output close.

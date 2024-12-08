def temp-table cpedi like faccpedi.
def temp-table dpedi like facdpedi
    field cangui as dec extent 20
    field nrogui as char extent 20.
    
for each faccpedi where codcia = 001
    and coddiv = '00000'
    and coddoc = 'ped'
    and flgest = 'P'
    and lookup(trim(codven), '173,015') > 0
    and fchped >= 01/01/2006 no-lock:
    create cpedi.
    buffer-copy faccpedi to cpedi.
    for each facdpedi of faccpedi no-lock:
        create dpedi.
        buffer-copy facdpedi to dpedi.
    end.
end.

    
/* buscamos las guias de remision */
def var i as int.
def var j as int.
def var x-orden as char.
for each cpedi:
    display cpedi.nroped.
    pause 0.
    x-orden = ''.
    for each ccbcdocu where ccbcdocu.codcia = 001
        and ccbcdocu.coddiv = '00000'
        and ccbcdocu.coddoc = 'fac'
        and ccbcdocu.codcli = cpedi.codcli
        and ccbcdocu.fchdoc >= 01/01/2006
        and ccbcdocu.nroped = cpedi.nroped
        and ccbcdocu.flgest <> 'a' no-lock:
        if x-orden = ''
        then x-orden = trim(ccbcdocu.nrodoc).
        else x-orden = x-orden + ',' + trim(ccbcdocu.nrodoc).
    end.        
    if x-orden = '' then next.
    j = 0.
    do i = 1 to num-entries(x-orden):
        for each ccbcdocu where ccbcdocu.codcia = 001
            and ccbcdocu.coddoc = 'fac'
            and ccbcdocu.nrodoc = entry(i, x-orden) no-lock:
            for each ccbddocu of ccbcdocu no-lock:
                find dpedi of cpedi where dpedi.codmat = ccbddocu.codmat
                    exclusive-lock no-error.
                if available dpedi then do:
                    bloque:
                    do j = 1 to 20:
                        if dpedi.nrogui[j] = '' then leave bloque.
                    end.
                    assign
                        dpedi.nrogui[j] = ccbcdocu.nrodoc
                        dpedi.cangui[j] = ccbddocu.candes.
                end.
                release dpedi.
            end.
        end.
    end.
end.

output to c:\tmp\luissenfac.txt.
for each cpedi,
    each dpedi of cpedi,
    first almmmatg of dpedi no-lock:
    display cpedi.nroped cpedi.fchped cpedi.usuario
        cpedi.codcli format 'x(11)'
        cpedi.nomcli format 'x(30)'
        dpedi.codmat 
        almmmatg.desmat format 'x(30)'
        dpedi.canped 
        dpedi.undvta
        dpedi.cangui[1] dpedi.nrogui [1]
        dpedi.cangui[2] dpedi.nrogui [2]
        dpedi.cangui[3] dpedi.nrogui [3]
        dpedi.cangui[4] dpedi.nrogui [4]
        dpedi.cangui[5] dpedi.nrogui [5]
        dpedi.cangui[6] dpedi.nrogui [6]
        dpedi.cangui[7] dpedi.nrogui [7]
        dpedi.cangui[8] dpedi.nrogui [8]
/*        dpedi.cangui[9] dpedi.nrogui [9]
 *         dpedi.cangui[10] dpedi.nrogui [10]*/
        with stream-io no-box width 320.
end.
    
output close.

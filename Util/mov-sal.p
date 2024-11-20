output to c:\tmp\mov-sal.txt.
for each almtmov where codcia = 001 and tipmov = 's':
    display
        codmov
        desmov
        pidref1
        glorf1
        pidref2
        glorf2
        pidcli
        pidpro
        movcmp
        reqguia
        movtrf
        pidodt
        movval
        unding
        pidcct
        with stream-io no-box width 320.
end.
output close.

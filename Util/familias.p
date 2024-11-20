output to c:\tmp\familias.txt.
for each almtfami no-lock where codcia = 001,
    each almsfami of almtfami no-lock:
    display
        almtfami.codfam
        desfam
        almsfami.subfam
        AlmSFami.dessub
        with stream-io.
end.
output close.

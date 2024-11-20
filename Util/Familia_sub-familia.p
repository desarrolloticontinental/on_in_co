
output to d:\sie\familia.txt.
FOR EACH almtfami where
    almtfami.codcia =  1 no-lock,
    each almsfami where
    almsfami.codcia = almtfami.codcia and
    almsfami.codfam = almtfami.codfam no-lock:
    display
        Almtfami.codfam
        Almtfami.desfam
        AlmSFami.subfam
        AlmSFami.dessub
        with width 132 stream-io.
end.

output close.
